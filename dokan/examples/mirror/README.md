# Mirror File System Example

This example demonstrates a mirror file system that maps a local directory to a Dokan mount point.

## Features

- Mirrors an existing directory to a virtual drive
- Supports all basic file operations (read, write, create, delete, rename)
- Handles file and directory attributes
- Supports file time operations
- Provides disk space information from the source drive

## Usage

```bash
cargo run --example mirror -- -s <SOURCE_PATH> -m <MOUNT_POINT>
```

### Arguments

- `-s, --source <SOURCE_PATH>`: The source directory to mirror (required)
- `-m, --mount-point <MOUNT_POINT>`: The mount point (drive letter or directory) (required)
- `-t, --single-thread`: Force single-threaded operation
- `-d, --dokan-debug`: Enable Dokan's debug output
- `-r, --removable`: Mount as a removable drive

### Examples

```bash
# Mirror C:\MyFiles to M: drive
cargo run --example mirror -- -s "D:\crv-virtual-disk\test-vfs" -m "M:\"

# Mirror with debug output
cargo run --example mirror -- -s "D:\crv-virtual-disk\test-vfs" -m "M:\" -d

# Mirror as a removable drive
cargo run --example mirror -- -s "D:\crv-virtual-disk\test-vfs" -m "M:\" -r
```

## Notes

- The source directory must exist before mounting
- Press Ctrl-C to unmount the file system
- All operations on the mounted drive are directly reflected in the source directory
- File handles are managed by the Windows file system API

