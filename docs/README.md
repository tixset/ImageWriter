# 💾 ImageWriter

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Platform](https://img.shields.io/badge/Platform-Windows-0078d4.svg)](https://www.microsoft.com/windows)
[![Language](https://img.shields.io/badge/Language-Delphi%207-ee1f35.svg)](https://www.embarcadero.com/products/delphi)
[![Version](https://img.shields.io/badge/version-2.2.0-blue)](https://github.com/tixset/ImageWriter/releases)
[![GitHub](https://img.shields.io/badge/GitHub-tixset%2FImageWriter-181717.svg?logo=github)](https://github.com/tixset/ImageWriter)

[![GitHub Stars](https://img.shields.io/github/stars/tixset/ImageWriter?style=social)](https://github.com/tixset/ImageWriter/stargazers)
[![GitHub Forks](https://img.shields.io/github/forks/tixset/ImageWriter?style=social)](https://github.com/tixset/ImageWriter/network/members)
[![GitHub Issues](https://img.shields.io/github/issues/tixset/ImageWriter)](https://github.com/tixset/ImageWriter/issues)
[![Last Commit](https://img.shields.io/github/last-commit/tixset/ImageWriter)](https://github.com/tixset/ImageWriter/commits)
[![Code Size](https://img.shields.io/github/languages/code-size/tixset/ImageWriter)](https://github.com/tixset/ImageWriter)

[![English](https://img.shields.io/badge/lang-English-blue.svg)](README.md)
[![Russian](https://img.shields.io/badge/lang-Русский-red.svg)](README.ru.md)

*Disk image writing utility for Windows*

## 📸 Screenshot

![Main window — project list](../assets/screenshots/main-window.png)

---

## 📋 Overview

ImageWriter is a powerful tool designed to write disk images to physical storage devices. It comes in two versions to accommodate different use cases and security requirements.

## 🔢 Versions

### ImageWriter.exe (Classic)
- **Requires administrator rights** at startup
- Monolithic application architecture
- Suitable for local files only
- Simple and straightforward usage
- Elevated privileges throughout the entire session

### ImageWriterPro.exe (Recommended)
- **Runs without administrator rights** initially
- Privilege separation architecture: GUI (user context) + Worker (elevated)
- **Network drive support** - can read images from network shares
- UAC prompt **only when performing write/read operations**
- More secure - elevated privileges only when necessary

## ✨ Key Features

### Archive Support
- **ZIP** - automatic decompression of .zip archives during write
- **GZIP** - automatic decompression of .gz, .img.gz, .iso.gz files
- **Streaming decompression** - no temporary files needed
- **Cancel support** - can cancel archive operations safely
- Dynamic zlib1.dll loading with fallback mechanisms

### Hash Verification
- **MD5** - traditional hash algorithm support
- **SHA-256** - modern cryptographic hash (FIPS 180-2 compliant)
- Algorithm selection via dropdown menu
- Automatic .md5 / .sha256 file generation and verification
- Device verification mode - read and verify disk contents

### Partition Preview
- **Automatic partition table detection** - MBR and GPT support
- **Pre-write analysis** - shows what partitions will be created
- **Detailed information** displayed before writing:
  - Partition type (FAT32, NTFS, Linux ext4, etc.)
  - Size in MB and sectors
  - Starting LBA address
  - Bootable flag (for MBR)
- Supports up to 4 MBR partitions and 128 GPT partitions

### Network Drive Support
ImageWriterPro can access files on network drives (mapped drives, UNC paths) thanks to its privilege separation architecture. The GUI runs in user context and retains access to network resources, while only the disk writing component runs elevated.

#### UNC Path Authentication
If you specify a UNC path (e.g., \\192.168.156.156\Workspace\file.img) and access is denied, the program will automatically invoke the standard Windows authentication dialog. If authentication is successful, the operation continues with the file on the share. This is implemented via the NetShareAuth module and works for both reading and writing images from/to network shares.

### Archive Support
- **ZIP** - automatic extraction on-the-fly
- **GZIP** - transparent decompression (.gz, .img.gz, .iso.gz)
- **Format detection** - XZ, tar.gz, tar.xz, 7z, BZIP2 formats recognized
- Direct writing without creating temporary files
- Future: full decompression support for XZ/7z/BZIP2

### WMI Integration
- Get disk information without administrator rights
- Display disk sizes and models before elevation
- Better user experience and transparency

### Safety Features
- **Single instance protection** - prevents running multiple copies
- **Mutex-based locking** - separate locks for GUI and Worker processes
- **Drive type detection** - distinguishes between network, fixed, and removable drives
- **Automatic calculations** - block size and count computed automatically

### Performance
- **Optimized IPC** - 65KB pipe buffer, 4KB data blocks
- **Progress updates** - every 256KB to minimize overhead
- **Background operations** - non-blocking UI during disk operations

### Device Health Monitoring (New in 2.2.0+)
- **Automatic diagnostics** when selecting a device:
  - **WMI information**: capacity, serial number, sector size
  - **SMART data**: temperature, reallocated sectors, power-on hours
  - **Partition validation**: GPT/MBR table integrity using accurate IOCTL disk sizes
- **Visual health indicators**:
  - Owner-draw ComboBox with colored backgrounds (green/orange/red)
  - Status icons: ✅ (healthy), ⚠️ (warning), ❌ (critical), ❔ (unknown)
  - Tooltips with brief health summary
- **Health Report button** - detailed analysis:
  - Hardware diagnostics, SMART metrics, partition validation
  - Trend analysis and degradation detection
  - Low speed detection (5 MB/s threshold)
- **Write operation safety**:
  - Blocks writes to devices with CRITICAL status
  - Warns before writing to WARNING devices
  - Prevents data loss on failing hardware
- **Critical bug fixes**:
  - COM reentrancy fix using PostMessage(WM_REFRESH_DEVICES)
  - WMI retry logic corrections (5 attempts × 500ms)
  - False partition validation errors eliminated (IOCTL vs WMI size)
- **Documentation**: See `docs/DEVICE_HEALTH_FEATURES.md` for complete guide

## 💻 System Requirements

- **Operating System**: Windows XP / Vista / 7 / 8 / 10 / 11
- **Compiler**: Delphi 7 (for building from source)
- **Runtime**: No additional dependencies required
- **Privileges**: Administrator rights needed for write/read operations only

## 🔨 Building from Source

### Using build.bat (Recommended)
```cmd
build.bat
```

This will:
- Clean temporary files (*.dcu, *.ddp, *.RES)
- Compile both ImageWriter.exe and ImageWriterPro.exe
- Display success/error messages

### Manual Compilation
```cmd
DCC32.EXE -B -U"studio;studio\md5;studio\random" ImageWriter.dpr
DCC32.EXE -B -U"studio;studio\md5;studio\random" ImageWriterPro.dpr
```

## 🏛️ Architecture

### ImageWriterPro Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    GUI Process (User Context)                │
│  - User interface (VCL forms)                                │
│  - File operations (read from network/local drives)          │
│  - Network drive access preserved                            │
│  - Manifest: asInvoker (no elevation required)               │
└───────────────────────┬─────────────────────────────────────┘
                        │
                        │ IPC via Named Pipes
                        │ (8 message types: Data, Log, Progress,
                        │  Completed, Error, Cancel, Ready)
                        │
┌───────────────────────▼─────────────────────────────────────┐
│                  Worker Process (Admin Context)              │
│  - Physical disk access (\\?\PhysicalDrive#)                 │
│  - IOCTL operations                                          │
│  - Raw sector writing                                        │
│  - Launched via ShellExecuteEx with "runas"                  │
│  - UAC prompt shown when needed                              │
└─────────────────────────────────────────────────────────────┘
```

### Communication Protocol

The IPC protocol uses 8 message types:

1. **ipcData** - File data blocks (4KB chunks)
2. **ipcLog** - Log messages from worker to GUI
3. **ipcProgress** - Progress updates (bytes processed)
4. **ipcCompleted** - Operation completed successfully
5. **ipcError** - Error occurred during operation
6. **ipcCancel** - Cancel operation request
7. **ipcReady** - Worker ready to receive data
8. **ipcControl** - Control messages (size information)

### Synchronization Protocol

1. GUI launches worker process with `--worker` parameter
2. Worker creates Named Pipe server and waits for connection
3. GUI connects to the pipe
4. Worker sends **ipcReady** after opening physical device
5. GUI waits for **ipcReady**, processes pending messages
6. GUI sends total size, then starts sending data blocks
7. Worker receives and writes blocks until `TotalProcessed >= TotalSize`
8. Worker sends **ipcCompleted** when done
9. GUI continues reading buffered messages until completion

## 📁 Project Structure

```
ImageWriter/
├── ImageWriter.dpr          # Main project (requireAdministrator)
├── ImageWriterPro.dpr       # Pro version with privilege separation
├── mainform.pas/dfm         # Main GUI form (shared by both versions)
│
├── Core Modules (ImageWriterPro architecture)
│   ├── ElevationHelper.pas  # UAC elevation and privilege checks
│   ├── IPCPipe.pas          # Named Pipes IPC implementation
│   ├── WorkerMode.pas       # Worker process logic
│   └── BinFile.pas          # Cross-platform disk access abstraction
│
├── Platform-Specific
│   ├── WinBinFile.pas       # Windows physical drive access
│   ├── UnixBinFile.pas      # Unix/Linux file access (legacy)
│   ├── WinIOCTL.pas         # Windows IOCTL definitions
│   └── Native.pas           # Native API wrappers
│
├── Archive Support
│   ├── ExtractZLib.pas      # zlib1.dll extraction from resources
│   ├── GZipStream.pas       # GZIP/ZIP archive handling
│   └── Zlib.pas             # ZLib wrapper
│
├── Utilities
│   ├── DiskIO.pas           # Legacy disk I/O code
│   ├── volume.pas           # Volume operations
│   └── QTThunkU.pas         # Legacy thunking
│
├── Resources
│   ├── dd.rc                # ImageWriter resources
│   ├── resources_pro.rc     # ImageWriterPro resources
│   ├── dd.exe.manifest      # ImageWriter manifest (requireAdministrator)
│   └── ImageWriterPro.exe.manifest  # Pro manifest (asInvoker)
│
├── Studio Libraries
│   └── studio/
│       ├── debug.pas        # Debug utilities (DEPRECATED - redirects to LogUtils)
│       ├── persrc.pas       # Persistent resources
│       ├── studio_tools.pas # Common tools
│       ├── md5/md5.pas      # MD5 hash
│       └── random/MT19937.pas  # Mersenne Twister RNG
│
├── Documentation
│   ├── README.md            # This file (English)
│   ├── README.ru.md         # Russian documentation
│   ├── CODING_STYLE_GUIDE.md # Code style guidelines
│   ├── COMMIT_CONVENTIONS.md # Git commit standards
│   ├── BUILD_WITH_ZLIB.txt  # ZLib build instructions
│   └── STREAMING_GZIP_RU.md # GZIP streaming notes
│
├── Configuration
│   ├── BUILD_CONFIGURATION.md  # Build instructions
│   ├── *.dof                   # IDE project settings
│   └── *.cfg                   # Compiler configuration
│
└── Build Scripts
    ├── build.bat            # Automated build script
    └── .gitignore           # Git ignore rules
```

## 🛠️ Usage

### GUI Mode

#### ImageWriter.exe (Classic Version)

1. Run as Administrator (UAC prompt on launch)
2. Select input file (image, .gz, .zip)
3. Select output device (physical drive or file)
4. Click "Write" or "Read"
5. Wait for completion

#### ImageWriterPro.exe (Recommended)

1. Run normally (no UAC prompt)
2. Browse and select input file (supports network drives)
3. Select output device from dropdown
4. Click "Write" or "Read"
5. **UAC prompt appears** requesting elevation
6. Worker process starts and performs the operation
7. Progress shown in GUI
8. Worker closes automatically when done or if GUI closes

### CLI Mode (Command-Line Interface)

ImageWriter now supports command-line interface for automation, scripting, and unattended operations.

#### Basic Usage

```cmd
ImageWriter.exe --cli <command> [options]
```

#### Commands

**Show help and version:**
```cmd
ImageWriter.exe --cli --help
ImageWriter.exe --cli --version
```

**List available devices:**
```cmd
ImageWriter.exe --cli --list
```

Example output:
```
Scanning for removable devices...

Found 2 removable device(s):

[1] \\.\PhysicalDrive2 - SanDisk Ultra USB Device (29.8 GB)
[2] E:\ - Kingston DataTraveler (7.5 GB)
```

**Write image to device:**
```cmd
ImageWriter.exe --cli --write --device \\.\PhysicalDrive2 --file ubuntu-22.04.iso
ImageWriter.exe --cli --write --device E:\ --file windows.img.gz --verify
```

**Read device to image file:**
```cmd
ImageWriter.exe --cli --read --device E:\ --file backup.img
ImageWriter.exe --cli --read --device \\.\PhysicalDrive2 --file backup.img.gz --hash md5
```

**Verify device against image:**
```cmd
ImageWriter.exe --cli --verify --device E:\ --file backup.img --hash sha256
```

#### Options

- `--device <path>` - Device path:
  - Physical drive: `\\.\PhysicalDrive2`
  - Volume letter: `E:\`
  
- `--file <path>` - Image file path:
  - Raw image: `file.img`
  - ISO image: `ubuntu.iso`
  - Compressed: `file.img.gz`, `file.zip`
  
- `--bs <size>` - Block size in bytes (default: 1048576)
  - Examples: `--bs 512`, `--bs 4096`, `--bs 1048576`
  
- `--count <n>` - Number of blocks to process (default: all)
  - Example: `--count 100` (process only first 100 blocks)
  
- `--hash <algorithm>` - Calculate hash:
  - `--hash md5` - Calculate MD5 hash
  - `--hash sha256` - Calculate SHA-256 hash
  
- `--verify` - Verify written data by reading back
  
- `--force` - Skip safety checks (⚠️ dangerous!)
  - Bypasses removable device check
  - Use only if you know what you're doing
  
- `--quiet` - Suppress progress output
  - Only errors will be shown

#### Exit Codes

- `0` - Success
- `1` - Error (invalid parameters, operation failed)
- `2` - Safety check failed (use --force to bypass)

#### Examples

**Create compressed backup with hash:**
```cmd
ImageWriter.exe --cli --read --device E:\ --file "D:\Backups\usb_backup.img.gz" --hash md5
```

**Write ISO and verify:**
```cmd
ImageWriter.exe --cli --write --device \\.\PhysicalDrive2 --file ubuntu-22.04.iso --verify --hash sha256
```

**Silent operation (for scripts):**
```cmd
ImageWriter.exe --cli --write --device E:\ --file bootable.img --quiet
if %ERRORLEVEL% EQU 0 (
    echo Success!
) else (
    echo Failed with error code %ERRORLEVEL%
)
```

**Batch processing:**
```cmd
@echo off
for %%D in (E F G) do (
    echo Writing to drive %%D:\...
    ImageWriter.exe --cli --write --device %%D:\ --file image.img --force --quiet
    if %ERRORLEVEL% NEQ 0 (
        echo ERROR: Failed to write to %%D:\
    )
)
```

#### PowerShell Examples

```powershell
# Get list of devices and parse output
$devices = & ImageWriter.exe --cli --list | Select-String -Pattern '\[(\d+)\]'

# Write with progress tracking
$process = Start-Process -FilePath "ImageWriter.exe" `
    -ArgumentList "--cli --write --device E:\ --file ubuntu.iso" `
    -NoNewWindow -Wait -PassThru
    
if ($process.ExitCode -eq 0) {
    Write-Host "Write completed successfully" -ForegroundColor Green
} else {
    Write-Host "Write failed with code $($process.ExitCode)" -ForegroundColor Red
}

# Verify hash after operation
$exitCode = & ImageWriter.exe --cli --verify --device E:\ --file image.img --hash md5
if ($LASTEXITCODE -eq 0) {
    Write-Host "Verification passed" -ForegroundColor Green
}
```

#### CI/CD Integration

CLI mode is perfect for continuous integration and deployment pipelines:

```yaml
# GitHub Actions example
- name: Write bootable USB
  run: |
    ImageWriter.exe --cli --write --device E:\ --file bootable.img --verify
    
- name: Verify integrity
  run: |
    ImageWriter.exe --cli --verify --device E:\ --file bootable.img --hash sha256
```

#### Security Notes

⚠️ **Important:** CLI mode requires administrator privileges just like GUI mode. You may need to:

1. Run from elevated command prompt/PowerShell
2. Use `runas /user:Administrator cmd` and then run ImageWriter
3. Configure scheduled tasks with elevated privileges

**Safety:** By default, CLI mode will refuse to write to fixed disks. Use `--force` only if you understand the risks.

## Technical Details

### Delphi 7 Compatibility

- Uses `array of Byte` instead of `TBytes` (Delphi 7 doesn't have TBytes)
- ActiveX unit for `CoCreateGuid` (GUID generation)
- Custom `CtlCode()` function for IOCTL constants
- VCL-based UI (not FireMonkey)

### IPC Implementation

- **Named Pipes**: `\\.\pipe\ImageWriterPro_[GUID]`
- **Buffer Size**: 65536 bytes
- **Data Blocks**: 4KB chunks for optimal performance
- **Timeout**: 5 seconds for connection, infinite for operations
- **Bidirectional**: Full duplex communication

### WMI Integration

Uses WMI (Windows Management Instrumentation) to query disk information without elevation:
- `Win32_DiskDrive` - Get disk size, model, serial number
- `Win32_DiskPartition` - Partition information
- Falls back to IOCTL when WMI unavailable

### Single Instance Protection

Global mutexes ensure only one instance runs:
- **GUI Mutex**: `Global\ImageWriterPro_GUI_Mutex`
- **Worker Mutex**: `Global\ImageWriterPro_Worker_[PipeName]`

Each worker instance has its own mutex based on pipe name (GUID).

## Known Limitations

1. **READ operation doesn't use IPC streaming yet**
   - Worker creates file directly in its context
   - Cannot save to network drives when elevated
   - Workaround: Use local path, then copy manually

2. **Volume lock/unlock not delegated to worker**
   - GUI attempts to lock volumes without elevation
   - May fail on system drives
   - Doesn't affect write operation success

## License

GPL (see source code for details)

## Version History

### 2.2.0 (2025-12-22) - Logging System Unification

- **Unified logging system** - debug.pas now redirects to LogUtils.pas (wrapper approach)
- **Improved code quality** - Added deprecation warnings for legacy debug.pas usage
- **Full backward compatibility** - All existing code works without changes
- **Enhanced documentation** - Added LOGGING_MIGRATION_GUIDE.md

### 2.1.0 (2025-12-18) - Enhanced User Experience

- **Improved logging** - Enhanced startup information (program path, version, edition, computer name, user)
- **Fixed device enumeration** - Proper volLink display via GetDiskExtents
- **Disk size detection** - Fallback to GetDiskExtents when DriveTotalBytes fails
- **Resource management** - Restored application icons and version information
- **Project structure** - Renamed dd.dpr to ImageWriterPro.dpr for clarity
- **Automatic dialog selection** - Uses UserContext dialog when elevated on Vista+, falls back to VCL on XP

### 2.0.0 (2025-12-18) - Privilege Separation Release
- Implemented privilege separation architecture
- Added network drive support via IPC streaming
- WMI integration for device information without elevation
- Fixed IPC deadlock issues (ipcReady synchronization protocol)
- Added automatic block calculation on path input
- Single instance protection with mutexes
- Optimized progress updates (every 256KB instead of 4KB)
- Fixed "99% completion" issue (size-based exit condition)
- Added drive type detection (Network/Fixed/Removable)
- Worker auto-closes when GUI exits
- Created comprehensive documentation and build automation

## Contributing

Contributions are welcome! Please ensure:
- Code compiles with Delphi 7
- Both ImageWriter.exe and ImageWriterPro.exe build successfully
- Test with local and network drives
- Update documentation for significant changes

## Documentation

- **[Project Analysis & Improvements](PROJECT_ANALYSIS_IMPROVEMENTS.md)** - Comprehensive roadmap with 75+ proposed features (1724 lines, detailed)
- **[Improvements Summary](IMPROVEMENTS_SUMMARY.md)** - Quick overview of proposed improvements and priorities
- **[README (English)](README.md)** - Main documentation (this file)
- **[README (Русский)](README.ru.md)** - Russian documentation

### Development Roadmap

**Current Status:** Version 2.2.0-dev
- ✅ Priority 1: Critical improvements - COMPLETED (READ via IPC, Volume locking via Worker)
- 🔜 Priority 2-3: Functional improvements & UX enhancements
- 🔜 Priority 9-14: Advanced features (Multi-Writer, Batch mode, Dark theme)
- 🔜 Long-term: Plugins system, Cloud integration, REST API

See [IMPROVEMENTS_SUMMARY.md](IMPROVEMENTS_SUMMARY.md) for quick wins and [PROJECT_ANALYSIS_IMPROVEMENTS.md](PROJECT_ANALYSIS_IMPROVEMENTS.md) for detailed analysis.

## Credits

Original ImageWriter project with additions:
- Privilege separation architecture
- IPC communication layer
- WMI integration
- Network drive support

## Support

For issues and questions, please check:
1. This README and [README.ru.md](README.ru.md)
2. [CHANGELOG.md](../CHANGELOG.md) for version history
3. [ROADMAP.md](../ROADMAP.md) for planned features
4. Source code comments and Git commit history
5. [GitHub Issues](https://github.com/tixset/ImageWriter/issues) for known problems
