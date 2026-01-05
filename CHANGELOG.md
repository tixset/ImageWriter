# 📝 Changelog

*All notable changes to ImageWriter will be documented in this file.*

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

**Maintainer:** Anton Zelenov (tixset@gmail.com)  
**GitHub:** https://github.com/tixset/ImageWriter

---

## [Unreleased]

### 🐛 Critical Bug Fixes (2025-12-25)
- **Fixed COM reentrancy error** - WMI calls from WM_DEVICECHANGE handler blocked by Windows COM
  - Implemented PostMessage(WM_REFRESH_DEVICES) for delayed device refresh
  - Moved PopulateDevices() to separate message handler (WMRefreshDevices)
  - Eliminated "Cannot make outgoing call during incoming synchronous call" error
- **Fixed WMI retry logic** - corrected inconsistent retry conditions in GetFullDiskInfoWMI
  - Changed all retry checks from `Retry < 3` to `Retry < 5` (matching loop range)
  - Increased retry delay from 200ms to 500ms between attempts
  - Increased initial device detection delay from 500ms to 1000ms
  - Added detailed DEBUG logging for WMI query failures (attempt counts, error messages)
- **Fixed false partition validation errors** - used accurate IOCTL disk size instead of WMI
  - ValidatePartitionTable now uses GetPhysicalDiskSize (IOCTL) instead of Info.Size (WMI)
  - WMI can return inaccurate sizes (e.g., 256052966400 vs 256060514304 bytes = 7.5 MB diff)
  - Eliminated false CRITICAL status on healthy SSD (PhysicalDrive0)
- **Enhanced device health diagnostics** - comprehensive health analysis on device selection
  - Added AnalyzeDiskHealth() method with WMI, SMART, and partition validation
  - Implemented TDeviceHealthStatus enum (dhsUnknown, dhsHealthy, dhsWarning, dhsCritical)
  - Added FDeviceHealthInfo array for tracking health of up to 10 devices
  - Added FSMARTHistory array for storing 100 historical SMART measurements per device
  - Displays hardware diagnostics (capacity, serial number, sector size)
  - Shows SMART diagnostics (temperature, reallocated sectors, pending sectors, power-on hours)
  - Validates partition table integrity with actual disk size
  - Health summary with recommendations (OK/WARNING/CRITICAL)
- **Device health visual indicators** - added owner-draw ComboBox with health status
  - ComboDeviceDrawItem: colored backgrounds (green=healthy, orange=warning, red=critical)
  - ComboDeviceMeasureItem: increased row height to 20px for better visibility
  - Status icons: ✅ Healthy, ⚠️ Warning, ❌ Critical, ❔ Unknown
  - Tooltip shows health status and issue count on hover
- **Health Report button** - detailed device health analysis
  - Shows model, serial number, status icon, last check time
  - Displays warning and critical issue counts
  - Includes partition table error messages if any
  - Trend analysis from SMART history (temperature, bad sectors)
- **Write operation safety** - blocking writes to critical-status devices
  - ValidateDeviceForWrite() checks health before write operation
  - CRITICAL devices: write blocked with error dialog
  - WARNING devices: confirmation dialog with risk warning
  - Low write speed detection (threshold 5 MB/s) with periodic warnings
- **"Ready!" message formatting** - green success highlighting for startup message
  - Added "ready!" to success message detection pattern (green background)
  - Changed startup message from "Ready" to "Ready!" with newline
  - Improved success message detection (successfully, completed, success, ready!)

### 🐛 Code Quality Improvements (2025-12-24)
- **Fixed critical compilation errors** - ImageWriterPro.exe now compiles without errors
  - Fixed AuthenticateNetworkShare → NetShareAuth.EnsureNetworkShareAccess typo
  - Added missing variable declarations for IMAGEWRITER_PRO conditional blocks
  - Fixed StartPersistentWorker to use ShellExecuteEx instead of RunElevated
- **Fixed critical warnings** - eliminated uninitialized variable warnings
  - XZStream.pas: Added LastError initialization before first use
  - BZip2Stream.pas: Added LastError initialization before first use
- **Eliminated all compiler warnings** - 28 signed/unsigned mixing warnings fixed
  - Added explicit Integer()/Cardinal() type casts in XZStream, BZip2Stream, PartitionAnalyzer
  - Suppressed platform-specific warnings with {$WARN SYMBOL_PLATFORM OFF/ON}
  - Removed unused variables (DeviceBenchmark, mainform)
- **Compiler hints reduced** - from 11 to 5 expected hints
  - All remaining hints related to IMAGEWRITER_PRO conditional compilation
  - GZipStream, ElevationHelper, WorkerMode optimizations
- **Build status** - both ImageWriter.exe and ImageWriterPro.exe compile cleanly
  - User code warnings: 0
  - Legacy library warnings: 18 (persrc.pas, MT19937.pas - acceptable)
  - Hints: 5 (expected, Pro-feature related)

### 🎨 Enhanced Log Formatting
- **RTF-based colored backgrounds** - WARNING/ERROR/SUCCESS messages display with soft colored backgrounds
  - WARNING: Soft orange background (RGB 255, 220, 180)
  - ERROR: Soft red background (RGB 255, 200, 200)
  - SUCCESS: Light green background (RGB 200, 255, 200) for completed operations
- **RTF streaming** - implemented proper RTF insertion via EM_STREAMIN Windows message
- **TEditStream callback** - custom RTFStreamInCallback for streaming RTF data to RichEdit
- **Background positioning** - fixed RTF \highlight command ordering with \par\pard paragraph markers
- **Improved label formatting** - multi-word labels (e.g., "Program Info:", "Drive E:") properly bolded
- **Expanded color coding** - values colored by type: blue (sizes), green (success), red (errors), purple (drives), navy (numbers), teal (other)
- **Header detection** - lines starting with ===, ---, *** displayed as bold navy headers
- **Duplicate prefix removal** - eliminated redundant "WARNING:" and "ERROR:" prefixes from log messages
- **Attribute reset** - proper formatting reset after RTF insertion to prevent background bleeding

---

## [2.2.0] - 2025-12-22

### 🆕 Compression Libraries
- **BZIP2 support** - added libbz2.dll (32-bit) for BZIP2 decompression
- **XZ/LZMA support** - added liblzma.dll (32-bit) for XZ/LZMA decompression
- **Dynamic loading** - all compression libraries loaded on-demand via LoadLibrary
- **Resource embedding** - all DLL embedded in executable resources (1.7 MB total)
- **Multi-library infrastructure** - ExtractZLib.pas extended to support all compression DLL
- **BZip2Stream.pas** - full BZIP2 streaming decompression with bz_stream handling
- **XZStream.pas** - full XZ/LZMA streaming decompression with 256MB memory limit
- **3-tier fallback** - DLL search: %LOCALAPPDATA%\ImageWriter\ → exe directory → lib/ subdirectory
- **Updated tests** - ArchiveIntegrationTests.pas includes BZIP2 and XZ decompression tests

### 🆕 SMART Disk Health Monitoring
- **SMART reading** - implemented GetSMARTData() in DeviceBenchmark.pas via IOCTL_ATA_PASS_THROUGH
- **ATA structures** - added IDEREGS, SENDCMDINPARAMS, SENDCMDOUTPARAMS to WinIOCTL.pas
- **Attribute parsing** - reads up to 30 SMART attributes with values and thresholds
- **Health status** - determines OK/WARNING/CRITICAL based on attribute values
- **Temperature monitoring** - extracts temperature from SMART attribute 0xC2
- **Power-on hours** - reads cumulative power-on time from attribute 0x09
- **Drive cycle count** - retrieves start/stop count from attribute 0x0C
- **Updated tests** - DeviceBenchmarkTests.pas includes SMART data retrieval test

### 🆕 Command-Line Interface (CLI)
- **Full CLI implementation** - complete command-line interface for automation and scripting
- **ConsoleMode.pas** - comprehensive CLI module (1100+ lines) with command parsing and execution
- **All operations supported** - read, write, verify with progress tracking
- **Archive support** - automatic GZIP/ZIP compression/decompression
- **Hash verification** - MD5 and SHA256 hash calculation and comparison
- **Progress bar** - visual progress indicator with speed and ETA
- **Commands:**
  - `--cli --help` - display usage information and examples
  - `--cli --version` - show version and copyright
  - `--cli --list` - enumerate removable devices
  - `--cli --write --device E:\ --file ubuntu.iso --verify` - write image with verification
  - `--cli --read --device E:\ --file backup.img.gz --hash md5` - read to compressed image
  - `--cli --verify --device E:\ --file backup.img --hash sha256` - verify data integrity
- **Options:**
  - `--bs <size>` - block size (default 1MB)
  - `--count <n>` - limit read/write operations
  - `--force` - bypass safety checks (dangerous!)
  - `--quiet` - suppress progress output
- **Integration:**
  - DeviceManager - device enumeration via WMI
  - FileOperations - file size and format detection
  - ArchiveHandler - GZIP/ZIP streaming
  - HashUtils - MD5/SHA256 calculation
  - WinBinFile - low-level device I/O
- **Safety features:**
  - `--force` flag required for write operations
  - Device size validation
  - File existence checks
  - Exit codes: 0 (success), 1 (error), 2 (safety check failed)
- **Documentation:**
  - Updated README.md with CLI examples
  - Added comprehensive CLI section in docs/README.md
  - PowerShell and CMD usage examples
  - CI/CD integration examples

### 🎨 UI/UX Improvements
- **Drag & Drop support** - drag image files directly into EditIn field for quick file selection
- **Refresh button** - manual device list refresh button for detecting newly connected devices
- **Device size display** - LabelDeviceInfo shows selected device capacity for confirmation
- **Hash algorithm selector** - ComboHashAlgo dropdown for choosing MD5 or SHA256 verification
- **Quiet Mode** - dedicated button to lower process priority and minimize window during operations
- **Settings persistence** - automatic save/load of BS, Count, Seek, Skip, Hash algorithm to ImageWriter.ini
- **Sound notifications** - audio feedback on operation completion (success/error/eject confirmation)
- **Auto-eject functionality** - optional automatic device ejection after successful write operations
- **Windows 7+ taskbar progress** - operation progress displayed in Windows taskbar icon
- **Window flash notification** - window flashes on taskbar when operation completes
- **Special devices support** - Unix-style virtual devices (/dev/zero, /dev/random, /dev/urandom) in EditIn dropdown

### 🔧 ImageWriterPro Features
- **Persistent worker process** - long-running elevated worker via IPC for improved performance
- **WorkerCommands.pas** - comprehensive command protocol for GUI-worker communication (+389 lines)
- **Reduced UAC prompts** - single elevation at startup instead of per-operation prompts
- **IPC optimization** - named pipe communication for efficient data transfer

### 📝 Improved Logging System
- **BatchLogHelper.pas** - new helper class for batching high-frequency log messages
- **Structured format** - operation logs now use consistent format with [INFO]/[WARNING]/[ERROR] prefixes
- **System event markers** - startup/shutdown events clearly marked in logs for easy filtering
- **Based on LOG_FORMAT_SPECIFICATION.md** - implemented recommendations from logging specification
- **TLogUtils wrapper** - centralized logging interface with methods: Info(), Warning(), Error(), Critical()
- **Log rotation** - automatic log file management to prevent excessive disk usage
- **Note:** Delphi 7 uses ANSI strings - Unicode icons not supported

### 🔬 Device Benchmarking & Diagnostics
- **Device Benchmark module** - complete benchmarking system for testing disk performance
- **4 test types** - Sequential Read/Write, Random Read/Write (4KB blocks)
- **Performance metrics** - Min/Max/Avg speed calculation, 10 iterations for averaging
- **New module DeviceBenchmark.pas** - TDeviceBenchmark class with RunBenchmark, RunAllBenchmarks methods (520 lines)
- **Benchmark UI** - modal dialog with 3 tabs: Benchmark, SMART Data, Device Info
- **New form BenchmarkForm.pas** - TBenchmarkForm with progress tracking, result display, export (478 lines)
- **SMART data structures** - TSMARTData, TSMARTAttribute ready for future SMART implementation
- **Direct I/O** - FILE_FLAG_NO_BUFFERING | FILE_FLAG_WRITE_THROUGH for accurate speed measurement
- **Destructive test warnings** - confirmation dialogs for Sequential/Random Write tests
- **Export to TXT** - save benchmark results and SMART data to text files
- **Progress callbacks** - real-time speed and progress display during tests
- **Cancellation support** - ability to stop running benchmarks
- **Main form integration** - "Benchmark" button opens modal dialog for selected device

### 🗃️ Archive Analysis Features
- **Partition table analysis from archives** - analyze MBR/GPT partition tables without extracting full archive
- **Stream-based partition detection** - read only first 64KB from archive for analysis (vs multi-GB extraction)
- **All archive formats supported** - ZIP, GZIP, XZ, BZIP2, 7z, tar.gz, tar.xz (7 formats total)
- **New module ArchivePartitionReader.pas** - utility class for efficient archive header reading (302 lines)
- **Enhanced PartitionInfo.pas** - added DetectPartitionTableTypeFromStream, ParseMBRPartitionsFromStream, ParseGPTPartitionsFromStream (+195 lines)
- **UI integration** - EditInChange now auto-detects archive type and displays partition info on file selection
- **Memory efficient** - only 1KB for detection, 512B for MBR, 64KB for GPT (supports up to 512 GPT partitions)

### 📊 Code Quality & Documentation
- **Unified logging system** - debug.pas now redirects to LogUtils.pas using wrapper approach
- **Added deprecation warnings** - debug.pas marked as deprecated with compiler warnings in DEBUG mode
- **Full backward compatibility** - all existing code works without changes
- **Created ROADMAP.md** - comprehensive project roadmap with short/mid/long-term goals
- **Created CODE_OF_CONDUCT.md** - Contributor Covenant v2.1 for community guidelines
- **Updated documentation** - README.md and README.ru.md updated with logging changes

### 🔄 LogUtils Enhancements (Optional Tasks Completed)
- **Log rotation** - automatic rotation when file reaches maximum size (default 10MB)
- **Minimum level filtering** - filter out logs below specified level (Debug/Info/Warning/Error/Critical)
- **File size management** - SetMaxFileSize, GetLogFileSize, RotateLogFile methods
- **Migrated 4 modules** - Native.pas, WinBinFile.pas, persrc.pas, studio_tools.pas now use TLogUtils directly
- **Runtime tested** - all new features tested and working correctly

### 📚 API Documentation
- **Complete XML-doc comments** - 51 public methods across 6 manager modules
- **LogUtils.pas** - 18 methods documented (logging system API)
- **DeviceManager.pas** - 7 methods documented (WMI, device enumeration)
- **ProgressManager.pas** - 8 methods documented (speed, ETA calculation)
- **HashUtils.pas** - 7 methods documented (MD5/SHA256 hashing)
- **ValidationHelper.pas** - 5 methods documented (parameter validation)
- **ArchiveHandler.pas** - 6 methods documented (ZIP/GZIP streaming)
- 466 lines of documentation added with summary, params, returns, remarks

### 🧪 Testing Improvements
- **5 new test modules** - comprehensive unit and integration tests
- **DeviceManagerTests.pas** - device enumeration, WMI queries, validation
- **HashUtilsTests.pas** - MD5/SHA256 with known value verification, performance tests
- **ValidationHelperTests.pas** - image/device validation, size checks, extension validation
- **ArchiveIntegrationTests.pas** - ZIP/GZIP extraction, streaming, cancellation
- **IPCIntegrationTests.pas** - worker communication, data transfer, error handling
- 40+ test cases covering managers and core functionality
- 1458 lines of test code added

### ⚙️ Technical Details
- Created TLogUtilsAdapter class to bridge callback systems
- All Log() calls now redirect to TLogUtils.Info()
- Legacy callback mechanism preserved for compatibility
- Added initialization/finalization sections for proper cleanup
- New methods: SetMinimumLevel, SetMaxFileSize, SetLogRotation, GetLogFileSize, RotateLogFile
- ~25 Log() calls migrated to appropriate TLogUtils levels (Error/Warning/Info/Debug)
- Deleted obsolete documentation (LOGGING_MIGRATION_GUIDE.md, LOGGING_UNIFICATION_PLAN.md)
- Cleaned up all .dcu compilation artifacts

## [2.0.0] - 2024-xx-xx

### ➕ Added
- Dynamic ZLib loading - zlib1.dll is now loaded on-demand only when working with compressed archives
- Comprehensive logging system with file and UI output
- Enhanced exception handling for UI initialization
- Network share authentication support (Pro version)
- Improved device enumeration with better error handling
- **Automated test suite with DUnit framework**
  - Device detection and enumeration tests
  - Safety mechanism validation (fixed disk protection)
  - ZLib compression/decompression tests
  - Safe operations with removable devices only
  - Automatic test runner script (run_tests.bat)

### 🔄 Changed
- ZLib library is no longer statically linked - defers DLL loading until needed
- Improved startup performance by deferring unnecessary library loads
- Better error messages for device access issues
- Enhanced progress reporting during disk operations

### 🔧 Fixed
- Access Violation issues during form initialization
- Proper handling of UI control initialization errors
- Better recovery from failed device enumeration
- Network drive mounting issues resolved

## [2.0.0] - 2024-xx-xx

### ➕ Added
- GUI interface for disk image writing
- Support for multiple image formats (raw, GZIP, ZIP)
- Progress bar and status reporting
- Device safety filters
- UAC elevation support for Vista and later
- Pro version with network share support

### 🔄 Changed
- Complete rewrite from command-line tool to GUI application
- Modern Windows compatibility (Vista/7/8/10/11)
- Enhanced error handling and user feedback

## [0.6beta3] - Original dd for Windows by John Newbigin

### 📜 Original Features
- Command-line disk imaging tool for Windows
- Support for block devices and virtual devices
- Progress reporting with --progress flag
- Safety filters (--filter=removable, --filter=fixed)
- Virtual devices (/dev/zero, /dev/random)
- Support for standard I/O pipes

---

**Note:** Versions 2.0.0 and later represent a significant evolution from the original dd for Windows command-line tool, adding a graphical interface and modern Windows features while maintaining the core disk imaging functionality.

For the original dd for Windows by John Newbigin, see: http://www.chrysocome.net/dd
