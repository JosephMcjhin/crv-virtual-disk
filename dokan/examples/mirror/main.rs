use std::{
	ffi::c_void,
	fs::{self, File, OpenOptions},
	io::{Read, Seek, SeekFrom, Write},
	os::windows::fs::{MetadataExt, OpenOptionsExt},
	os::windows::io::{AsRawHandle, FromRawHandle, RawHandle},
	path::{Path, PathBuf},
	ptr,
	time::SystemTime,
};

use clap::{Arg, ArgAction, Command};
use dokan::{
	init, shutdown, unmount, CreateFileInfo, DiskSpaceInfo, FileInfo, FileSystemHandler,
	FileSystemMounter, FileTimeOperation, FillDataError, FillDataResult, FindData,
	MountFlags, MountOptions, OperationInfo, OperationResult, VolumeInfo, IO_SECURITY_CONTEXT,
};
use dokan_sys::win32::{
	FILE_CREATE, FILE_DELETE_ON_CLOSE, FILE_DIRECTORY_FILE, FILE_MAXIMUM_DISPOSITION,
	FILE_OPEN, FILE_OPEN_IF, FILE_OVERWRITE, FILE_OVERWRITE_IF, FILE_SUPERSEDE,
};
use widestring::{U16CStr, U16CString};
use winapi::{
	shared::{
		minwindef::{DWORD, FILETIME, MAX_PATH},
		ntstatus::*,
		winerror::ERROR_FILE_NOT_FOUND,
	},
	um::{
		errhandlingapi::GetLastError,
		fileapi::{GetDiskFreeSpaceExW, GetVolumeInformationW, SetFileAttributesW, SetFileTime},
		handleapi::CloseHandle,
		winbase::FILE_FLAG_BACKUP_SEMANTICS,
		winnt::{self, ULARGE_INTEGER},
	},
};

struct FileHandle {
	handle: RawHandle,
	path: PathBuf,
	is_directory: bool,
	delete_pending: bool,
}

unsafe impl Send for FileHandle {}
unsafe impl Sync for FileHandle {}

impl FileHandle {
	fn new(handle: RawHandle, path: PathBuf, is_directory: bool, delete_pending: bool) -> Self {
		Self {
			handle,
			path,
			is_directory,
			delete_pending,
		}
	}
}

impl Drop for FileHandle {
	fn drop(&mut self) {
		unsafe {
			CloseHandle(self.handle as *mut c_void);
		}
		if self.delete_pending {
			let _ = if self.is_directory {
				fs::remove_dir(&self.path)
			} else {
				fs::remove_file(&self.path)
			};
		}
	}
}

#[derive(Debug)]
struct MirrorHandler {
	root_path: PathBuf,
	debug: bool,
}

impl MirrorHandler {
	fn new<P: AsRef<Path>>(root_path: P, debug: bool) -> Self {
		Self {
			root_path: root_path.as_ref().to_path_buf(),
			debug,
		}
	}

	fn get_real_path(&self, file_name: &U16CStr) -> PathBuf {
		let file_name_str = file_name.to_string_lossy();
		let trimmed = file_name_str.trim_start_matches('\\');
		let path = self.root_path.join(trimmed);
		if self.debug {
			eprintln!("get_real_path: {:?} -> {:?}", file_name_str, path);
		}
		path
	}

	fn system_time_to_filetime(time: SystemTime) -> FILETIME {
		let duration = time
			.duration_since(SystemTime::UNIX_EPOCH)
			.unwrap_or_default();
		let intervals =
			(duration.as_secs() as u64 + 11644473600) * 10_000_000 + duration.subsec_nanos() as u64 / 100;
		FILETIME {
			dwLowDateTime: intervals as u32,
			dwHighDateTime: (intervals >> 32) as u32,
		}
	}

	fn win32_error_to_ntstatus(error: DWORD) -> i32 {
		unsafe { dokan_sys::DokanNtStatusFromWin32(error) }
	}
}

impl<'c, 'h: 'c> FileSystemHandler<'c, 'h> for MirrorHandler {
	type Context = FileHandle;

	fn create_file(
		&'h self,
		file_name: &U16CStr,
		_security_context: &IO_SECURITY_CONTEXT,
		desired_access: winnt::ACCESS_MASK,
		file_attributes: u32,
		share_access: u32,
		create_disposition: u32,
		create_options: u32,
		_info: &mut OperationInfo<'c, 'h, Self>,
	) -> OperationResult<CreateFileInfo<Self::Context>> {
		if create_disposition > FILE_MAXIMUM_DISPOSITION {
			return Err(STATUS_INVALID_PARAMETER);
		}

		let real_path = self.get_real_path(file_name);
		//i dont knot what fuck is this，but it works
		//can not judge from create_options & FILE_DIRECTORY_FILE != 0
		let is_directory = real_path.is_dir();
		let delete_pending = create_options & FILE_DELETE_ON_CLOSE != 0;

		if self.debug {
			eprintln!(
				"create_file: is_dir={}, disposition={}, options=0x{:x}, share=0x{:x}",
				is_directory, create_disposition, create_options, share_access
			);
		}

		// Handle directory
		if is_directory {
			let new_file_created: bool = match create_disposition {
				FILE_CREATE | FILE_OPEN_IF => {
					if !real_path.exists() {
						fs::create_dir(&real_path)
							.map_err(|_| Self::win32_error_to_ntstatus(unsafe { GetLastError() }))?;
						true
					} else {
						false
					}
				}
				FILE_OPEN => {
					if !real_path.exists() {
						return Err(STATUS_OBJECT_NAME_NOT_FOUND);
					}
					false
				}
				_ => return Err(STATUS_INVALID_PARAMETER),
			};

			let mut opts = OpenOptions::new();
			opts.read(true)
				.share_mode(share_access)
				.custom_flags(FILE_FLAG_BACKUP_SEMANTICS);

			let file = opts
				.open(&real_path)
				.map_err(|e| {
					eprintln!("Failed to open directory: {:?}, error: {:?}", real_path, e);
					Self::win32_error_to_ntstatus(e.raw_os_error().unwrap_or(5) as u32)
				})?;

			let handle = file.as_raw_handle();
			std::mem::forget(file);

			return Ok(CreateFileInfo {
				context: FileHandle::new(handle, real_path, true, delete_pending),
				is_dir: true,
				new_file_created,
			});
		}

		// Handle files
		let mut opts = OpenOptions::new();
		
		// Set access mode - always allow read for simplicity, add write if needed
		opts.read(true);
		if desired_access & (winnt::GENERIC_WRITE | winnt::FILE_WRITE_DATA | winnt::FILE_APPEND_DATA) != 0 {
			opts.write(true);
		}

		// Set share mode
		opts.share_mode(share_access);
		opts.custom_flags(file_attributes);

		let new_file_created = match create_disposition {
			FILE_CREATE => {
				if real_path.exists() {
					return Err(STATUS_OBJECT_NAME_COLLISION);
				}
				opts.create_new(true);
				true
			}
			FILE_OPEN => {
				if !real_path.exists() {
					return Err(STATUS_OBJECT_NAME_NOT_FOUND);
				}
				false
			}
			FILE_OPEN_IF => {
				let exists = real_path.exists();
				opts.create(true);
				!exists
			}
			FILE_OVERWRITE => {
				if !real_path.exists() {
					return Err(STATUS_OBJECT_NAME_NOT_FOUND);
				}
				opts.truncate(true);
				false
			}
			FILE_OVERWRITE_IF | FILE_SUPERSEDE => {
				let exists = real_path.exists();
				opts.create(true).truncate(true);
				!exists
			}
			_ => return Err(STATUS_INVALID_PARAMETER),
		};

		let file = opts
			.open(&real_path)
			.map_err(|e| Self::win32_error_to_ntstatus(e.raw_os_error().unwrap_or(5) as u32))?;

		let handle = file.as_raw_handle();
		std::mem::forget(file);

		Ok(CreateFileInfo {
			context: FileHandle::new(handle, real_path, false, delete_pending),
			is_dir: false,
			new_file_created,
		})
	}

	fn close_file(
		&'h self,
		_file_name: &U16CStr,
		_info: &OperationInfo<'c, 'h, Self>,
		_context: &'c Self::Context,
	) {
		// FileHandle Drop will handle closing
	}

	fn read_file(
		&'h self,
		_file_name: &U16CStr,
		offset: i64,
		buffer: &mut [u8],
		_info: &OperationInfo<'c, 'h, Self>,
		context: &'c Self::Context,
	) -> OperationResult<u32> {
		let result = {
			let mut file = unsafe { File::from_raw_handle(context.handle) };
			let res = file
				.seek(SeekFrom::Start(offset as u64))
				.and_then(|_| file.read(buffer))
				.map(|bytes_read| bytes_read as u32)
				.map_err(|e| Self::win32_error_to_ntstatus(e.raw_os_error().unwrap_or(5) as u32));
			std::mem::forget(file);
			res
		};
		result
	}

	fn write_file(
		&'h self,
		_file_name: &U16CStr,
		offset: i64,
		buffer: &[u8],
		info: &OperationInfo<'c, 'h, Self>,
		context: &'c Self::Context,
	) -> OperationResult<u32> {
		let result = {
			let mut file = unsafe { File::from_raw_handle(context.handle) };
			let res = if info.write_to_eof() {
				file.seek(SeekFrom::End(0))
					.and_then(|_| file.write(buffer))
			} else {
				file.seek(SeekFrom::Start(offset as u64))
					.and_then(|_| file.write(buffer))
			}
			.map(|bytes_written| bytes_written as u32)
			.map_err(|e| Self::win32_error_to_ntstatus(e.raw_os_error().unwrap_or(5) as u32));
			std::mem::forget(file);
			res
		};
		result
	}

	fn flush_file_buffers(
		&'h self,
		_file_name: &U16CStr,
		_info: &OperationInfo<'c, 'h, Self>,
		context: &'c Self::Context,
	) -> OperationResult<()> {
		let result = {
			let file = unsafe { File::from_raw_handle(context.handle) };
			let res = file
				.sync_all()
				.map_err(|e| Self::win32_error_to_ntstatus(e.raw_os_error().unwrap_or(5) as u32));
			std::mem::forget(file);
			res
		};
		result
	}

	fn get_file_information(
		&'h self,
		_file_name: &U16CStr,
		_info: &OperationInfo<'c, 'h, Self>,
		context: &'c Self::Context,
	) -> OperationResult<FileInfo> {
		let metadata = fs::metadata(&context.path)
			.map_err(|e| Self::win32_error_to_ntstatus(e.raw_os_error().unwrap_or(5) as u32))?;

		Ok(FileInfo {
			attributes: metadata.file_attributes(),
			creation_time: metadata.created().unwrap_or(SystemTime::UNIX_EPOCH),
			last_access_time: metadata.accessed().unwrap_or(SystemTime::UNIX_EPOCH),
			last_write_time: metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH),
			file_size: metadata.len(),
			number_of_links: 1,
			file_index: 0,
		})
	}

	fn find_files(
		&'h self,
		_file_name: &U16CStr,
		mut fill_find_data: impl FnMut(&FindData) -> FillDataResult,
		_info: &OperationInfo<'c, 'h, Self>,
		context: &'c Self::Context,
	) -> OperationResult<()> {
		let read_dir = fs::read_dir(&context.path)
			.map_err(|e| Self::win32_error_to_ntstatus(e.raw_os_error().unwrap_or(5) as u32))?;

		for entry in read_dir {
			let entry = entry
				.map_err(|e| Self::win32_error_to_ntstatus(e.raw_os_error().unwrap_or(5) as u32))?;

			let metadata = entry
				.metadata()
				.map_err(|e| Self::win32_error_to_ntstatus(e.raw_os_error().unwrap_or(5) as u32))?;

			let file_name =
				U16CString::from_os_str(entry.file_name()).unwrap_or_else(|_| U16CString::from_str("?").unwrap());

			let find_data = FindData {
				attributes: metadata.file_attributes(),
				creation_time: metadata.created().unwrap_or(SystemTime::UNIX_EPOCH),
				last_access_time: metadata.accessed().unwrap_or(SystemTime::UNIX_EPOCH),
				last_write_time: metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH),
				file_size: metadata.len(),
				file_name,
			};

			fill_find_data(&find_data).map_err(|e| match e {
				FillDataError::BufferFull => STATUS_BUFFER_OVERFLOW,
				FillDataError::NameTooLong => STATUS_SUCCESS,
			})?;
		}

		Ok(())
	}

	fn set_file_attributes(
		&'h self,
		_file_name: &U16CStr,
		file_attributes: u32,
		_info: &OperationInfo<'c, 'h, Self>,
		context: &'c Self::Context,
	) -> OperationResult<()> {
		let path_u16 = U16CString::from_os_str(&context.path).unwrap();
		unsafe {
			if SetFileAttributesW(path_u16.as_ptr(), file_attributes) == 0 {
				return Err(Self::win32_error_to_ntstatus(GetLastError()));
			}
		}
		Ok(())
	}

	fn set_file_time(
		&'h self,
		_file_name: &U16CStr,
		creation_time: FileTimeOperation,
		last_access_time: FileTimeOperation,
		last_write_time: FileTimeOperation,
		_info: &OperationInfo<'c, 'h, Self>,
		context: &'c Self::Context,
	) -> OperationResult<()> {
		let to_filetime_ptr = |op: &FileTimeOperation| match op {
			FileTimeOperation::SetTime(time) => {
				let ft = Self::system_time_to_filetime(*time);
				Box::into_raw(Box::new(ft))
			}
			_ => ptr::null_mut(),
		};

		let ct_ptr = to_filetime_ptr(&creation_time);
		let at_ptr = to_filetime_ptr(&last_access_time);
		let wt_ptr = to_filetime_ptr(&last_write_time);

		unsafe {
			let result = SetFileTime(context.handle as *mut c_void, ct_ptr, at_ptr, wt_ptr);

			if !ct_ptr.is_null() {
				let _ = Box::from_raw(ct_ptr);
			}
			if !at_ptr.is_null() {
				let _ = Box::from_raw(at_ptr);
			}
			if !wt_ptr.is_null() {
				let _ = Box::from_raw(wt_ptr);
			}

			if result == 0 {
				return Err(Self::win32_error_to_ntstatus(GetLastError()));
			}
		}

		Ok(())
	}

	fn delete_file(
		&'h self,
		_file_name: &U16CStr,
		_info: &OperationInfo<'c, 'h, Self>,
		context: &'c Self::Context,
	) -> OperationResult<()> {
		// Check if file is readonly
		if let Ok(metadata) = fs::metadata(&context.path) {
			if metadata.file_attributes() & winnt::FILE_ATTRIBUTE_READONLY != 0 {
				return Err(STATUS_CANNOT_DELETE);
			}
		}

		// The actual deletion will happen in Drop if delete_pending is true
		// Here we just validate that deletion is possible
		Ok(())
	}

	fn delete_directory(
		&'h self,
		_file_name: &U16CStr,
		info: &OperationInfo<'c, 'h, Self>,
		context: &'c Self::Context,
	) -> OperationResult<()> {
		if info.delete_pending() {
			// Check if directory is empty
			let is_empty = if let Ok(read_dir) = fs::read_dir(&context.path) {
				read_dir.count() == 0
			} else {
				true
			};

			if !is_empty {
				return Err(STATUS_DIRECTORY_NOT_EMPTY);
			}
		}

		Ok(())
	}

	fn move_file(
		&'h self,
		_file_name: &U16CStr,
		new_file_name: &U16CStr,
		replace_if_existing: bool,
		_info: &OperationInfo<'c, 'h, Self>,
		context: &'c Self::Context,
	) -> OperationResult<()> {
		let new_path = self.get_real_path(new_file_name);

		if new_path.exists() && !replace_if_existing {
			return Err(STATUS_OBJECT_NAME_COLLISION);
		}

		if new_path.exists() && replace_if_existing {
			let _ = if new_path.is_dir() {
				fs::remove_dir(&new_path)
			} else {
				fs::remove_file(&new_path)
			};
		}

		fs::rename(&context.path, &new_path).map_err(|e| {
			Self::win32_error_to_ntstatus(e.raw_os_error().unwrap_or(ERROR_FILE_NOT_FOUND as i32) as u32)
		})?;

		Ok(())
	}

	fn set_end_of_file(
		&'h self,
		_file_name: &U16CStr,
		offset: i64,
		_info: &OperationInfo<'c, 'h, Self>,
		context: &'c Self::Context,
	) -> OperationResult<()> {
		let result = {
			let file = unsafe { File::from_raw_handle(context.handle) };
			let res = file
				.set_len(offset as u64)
				.map_err(|e| Self::win32_error_to_ntstatus(e.raw_os_error().unwrap_or(5) as u32));
			std::mem::forget(file);
			res
		};
		result
	}

	fn set_allocation_size(
		&'h self,
		_file_name: &U16CStr,
		alloc_size: i64,
		_info: &OperationInfo<'c, 'h, Self>,
		context: &'c Self::Context,
	) -> OperationResult<()> {
		let result = {
			let file = unsafe { File::from_raw_handle(context.handle) };
			let res = file
				.set_len(alloc_size as u64)
				.map_err(|e| Self::win32_error_to_ntstatus(e.raw_os_error().unwrap_or(5) as u32));
			std::mem::forget(file);
			res
		};
		result
	}

	fn get_disk_free_space(&'h self, _info: &OperationInfo<'c, 'h, Self>) -> OperationResult<DiskSpaceInfo> {
		let path_u16 = U16CString::from_os_str(&self.root_path).unwrap();
		let mut free_bytes: u64 = 0;
		let mut total_bytes: u64 = 0;
		let mut available_bytes: u64 = 0;

		unsafe {
			if GetDiskFreeSpaceExW(
				path_u16.as_ptr(),
				&mut available_bytes as *mut u64 as *mut ULARGE_INTEGER,
				&mut total_bytes as *mut u64 as *mut ULARGE_INTEGER,
				&mut free_bytes as *mut u64 as *mut ULARGE_INTEGER,
			) == 0
			{
				return Err(Self::win32_error_to_ntstatus(GetLastError()));
			}
		}

		Ok(DiskSpaceInfo {
			byte_count: total_bytes,
			free_byte_count: free_bytes,
			available_byte_count: available_bytes,
		})
	}

	fn get_volume_information(&'h self, _info: &OperationInfo<'c, 'h, Self>) -> OperationResult<VolumeInfo> {
		let path_u16 = U16CString::from_os_str(&self.root_path).unwrap();
		let mut volume_name_buffer = [0u16; MAX_PATH + 1];
		let mut volume_serial_number: DWORD = 0;
		let mut maximum_component_length: DWORD = 0;
		let mut file_system_flags: DWORD = 0;
		let mut file_system_name_buffer = [0u16; MAX_PATH + 1];

		unsafe {
			if GetVolumeInformationW(
				path_u16.as_ptr(),
				volume_name_buffer.as_mut_ptr(),
				volume_name_buffer.len() as DWORD,
				&mut volume_serial_number,
				&mut maximum_component_length,
				&mut file_system_flags,
				file_system_name_buffer.as_mut_ptr(),
				file_system_name_buffer.len() as DWORD,
			) == 0
			{
				return Err(Self::win32_error_to_ntstatus(GetLastError()));
			}
		}

		Ok(VolumeInfo {
			name: U16CString::from_vec_truncate(volume_name_buffer.to_vec()),
			serial_number: volume_serial_number,
			max_component_length: maximum_component_length,
			fs_flags: file_system_flags,
			fs_name: U16CString::from_vec_truncate(file_system_name_buffer.to_vec()),
		})
	}

	fn mounted(
		&'h self,
		_mount_point: &U16CStr,
		_info: &OperationInfo<'c, 'h, Self>,
	) -> OperationResult<()> {
		Ok(())
	}

	fn unmounted(&'h self, _info: &OperationInfo<'c, 'h, Self>) -> OperationResult<()> {
		Ok(())
	}
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let matches = Command::new("dokan-rust mirror example")
		.author(env!("CARGO_PKG_AUTHORS"))
		.arg(
			Arg::new("source")
				.short('s')
				.long("source")
				.num_args(1)
				.value_name("SOURCE_PATH")
				.required(true)
				.help("Source directory path to mirror."),
		)
		.arg(
			Arg::new("mount_point")
				.short('m')
				.long("mount-point")
				.num_args(1)
				.value_name("MOUNT_POINT")
				.required(true)
				.help("Mount point path."),
		)
		.arg(
			Arg::new("single_thread")
				.short('t')
				.long("single-thread")
				.help("Force a single thread.")
				.action(ArgAction::SetTrue),
		)
		.arg(
			Arg::new("dokan_debug")
				.short('d')
				.long("dokan-debug")
				.help("Enable Dokan's debug output.")
				.action(ArgAction::SetTrue),
		)
		.arg(
			Arg::new("removable")
				.short('r')
				.long("removable")
				.help("Mount as a removable drive.")
				.action(ArgAction::SetTrue),
		)
		.get_matches();

	let source_path = matches.get_one::<String>("source").unwrap();
	let mount_point = U16CString::from_str(matches.get_one::<String>("mount_point").unwrap())?;

	if !Path::new(source_path).exists() {
		eprintln!("Error: Source path does not exist: {}", source_path);
		std::process::exit(1);
	}

	let mut flags = MountFlags::empty();
	if matches.get_flag("dokan_debug") {
		flags |= MountFlags::DEBUG | MountFlags::STDERR;
	}
	if matches.get_flag("removable") {
		flags |= MountFlags::REMOVABLE;
	}

	let options = MountOptions {
		single_thread: matches.get_flag("single_thread"),
		flags,
		..Default::default()
	};

	let handler = MirrorHandler::new(source_path, matches.get_flag("dokan_debug"));

	init();

	let mut mounter = FileSystemMounter::new(&handler, &mount_point, &options);

	println!("Mirroring '{}' to mount point...", source_path);

	let file_system = mounter.mount()?;

	let mount_point_clone = mount_point.clone();
	ctrlc::set_handler(move || {
		if unmount(&mount_point_clone) {
			println!("File system will unmount...")
		} else {
			eprintln!("Failed to unmount file system.");
		}
	})
	.expect("failed to set Ctrl-C handler");

	println!("File system is mounted, press Ctrl-C to unmount.");

	drop(file_system);

	println!("File system is unmounted.");

	shutdown();

	Ok(())
}
