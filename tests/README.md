# 🧪 ImageWriter Tests

*Automated testing infrastructure for ImageWriter application.*

## ✨ New: ImageWriter.exe CLI Support

**ImageWriter.exe теперь поддерживает Worker Mode!**

Starting from this version, ImageWriter.exe includes command-line argument support through Worker Mode (previously only in ImageWriterPro.exe):

```bash
# Worker Mode syntax
ImageWriter.exe --worker --pipe <name> --operation <0|1|2> --device <path> --file <path> [options]

# Parameters:
#   --operation: 0=Read, 1=Write, 2=Verify
#   --device: Device path (e.g., E:\ or \\.\PhysicalDrive2)
#   --file: Image file path (.img, .gz, .zip)
#   --bs: Block size (default: 512)
#   --count: Number of blocks
#   --checksize: Verify device size
#   --verifyhash: Calculate MD5 hash
#   --persistent: Run as persistent daemon
```

**Key Feature:** ImageWriter automatically creates `.gz` and `.zip` archives based on file extension - no external compression tools needed!

## 📋 Test Scripts

### 🔵 PowerShell Tests

**run_tests.ps1** - Basic smoke tests (~0.15 seconds, safe)
```powershell
.\run_tests.ps1 -JsonOutput -Verbose

# Run with archive format tests (2-3 min)
.\run_tests.ps1 -ArchiveTests -Verbose

# Run with partition detection tests (< 1 min)
.\run_tests.ps1 -PartitionTests -Verbose

# Run all extended tests
.\run_tests.ps1 -AllTests -Verbose
```
- Detects USB devices via WMI
- Verifies fixed disk protection  
- Checks ZLib library availability
- **NEW:** Archive format testing (GZIP, ZIP, XZ, BZIP2, 7z, TAR.GZ, TAR.XZ)
- **NEW:** Partition table detection (MBR/GPT from images and archives)
- Results: `test-results.json`

**Test-ArchiveFormats.ps1** - Comprehensive archive format testing
```powershell
.\Test-ArchiveFormats.ps1 -Verbose

# Skip write tests (recognition only)
.\Test-ArchiveFormats.ps1 -SkipWriteTests
```
- Tests all 7 supported archive formats
- Phase 1: Device → Archive (Read tests)
  * GZIP (.img.gz)
  * ZIP (.zip)
  * XZ (.img.xz)
  * BZIP2 (.img.bz2)
  * 7-Zip (.7z)
  * TAR.GZ (.tar.gz)
  * TAR.XZ (.tar.xz)
- Phase 2: Archive → Device (Recognition tests, non-destructive)
- Verifies magic bytes for each format
- ImageWriter creates all archives
- Results: `test-data/archives/` directory

**Test-PartitionInfo.ps1** - Partition detection testing
```powershell
# Create test images and run tests
.\Test-PartitionInfo.ps1 -CreateTestImages -Verbose

# Use existing test images
.\Test-PartitionInfo.ps1 -Verbose
```
- Creates test MBR and GPT disk images
- Tests partition detection from raw images
- Tests partition detection from archives (GZIP, ZIP)
- Verifies MBR signature (0x55AA)
- Verifies GPT signature ("EFI PART")
- Validates partition table parsing
- Results: `test-data/partition-tests/` directory
- Features:
  * MBR test image: 2 partitions (NTFS bootable + FAT32)
  * GPT test image: 2 partitions (EFI System + Microsoft Basic Data)
  * Archive compression: GZIP and ZIP formats
  * Full partition structure verification

**IntegrationTests.ps1** - GUI automation tests (2-3 min, safe)
```powershell
.\IntegrationTests.ps1
```
- Launches **real ImageWriter.exe** and controls via UI Automation API
- Tests reading to IMG/GZ/ZIP formats
- **ImageWriter creates archives** - script just passes `.gz`/`.zip` paths
- Non-destructive (read-only tests)
- Results: `temp_gui/` directory

**ImageWriterCLI.ps1** - CLI wrapper for automation
```powershell
# Read 100MB to compressed GZ (ImageWriter creates archive)
.\ImageWriterCLI.ps1 -Operation Read -Device E:\ -ImageFile backup.img.gz -Count 100

# Write image to USB
.\ImageWriterCLI.ps1 -Operation Write -Device E:\ -ImageFile image.img
```
- Controls ImageWriter.exe via UI Automation
- ImageWriter handles all compression based on file extension
- Used internally by integration tests

**Inspect-ImageWriterGUI.ps1** - GUI debugging utility
```powershell
.\Inspect-ImageWriterGUI.ps1
```
- Scans UI element tree for test development
- Shows control names, types, classes

### DUnit Tests (Delphi/Pascal)

**run_tests.bat** - Compile and run DUnit tests
```cmd
run_tests.bat
```

**ImageWriterTests.dpr** - Main test runner
- Requires Delphi 7 + DUnit framework
- Includes:
  - `DeviceDetectionTests.pas` - USB/disk enumeration
  - `SafeOperationsTests.pas` - Safety mechanisms
  - `ZLibTests.pas` - Compression functionality

## Safety Rules

⚠️ **CRITICAL:**
- Read tests are **safe** (no data modification)
- Write tests **DESTROY ALL DATA** on selected USB device
- Fixed disks (C:, D:, etc.) are **protected**
- Write tests require **explicit user confirmation** (YES + CONFIRM)

## Requirements

- Windows with PowerShell 5.1+
- ImageWriter.exe (compiled)
- At least one removable USB device
- Administrator rights for device access
- Optional: Delphi 7 for Pascal tests

## Quick Start

```powershell
# 1. Basic smoke tests (safe, 0.15s)
.\run_tests.ps1 -JsonOutput

# 2. GUI automation tests (safe, 2-3 min)
.\IntegrationTests.ps1

# 3. CLI wrapper example
.\ImageWriterCLI.ps1 -Operation Read -Device E:\ -ImageFile test.img.gz -Count 100
```

## Key Principles

1. **ImageWriter creates archives** - Scripts pass file paths with `.gz`/`.zip` extensions, ImageWriter.exe handles all compression internally
2. **No manual archiving** - No `Compress-Archive` or ZLib calls from PowerShell
3. **Real program testing** - All tests control actual ImageWriter.exe via UI Automation API
4. **Safe by default** - Write tests removed (were destructive and complex)

## Results & Artifacts

- **run_tests.ps1**: `test-results.json`
- **IntegrationTests.ps1**: `temp_gui/*.img`, `temp_gui/*.gz`, `temp_gui/*.zip`  
- **DUnit tests**: Console output
- All test data is gitignored (`tests/test-data/`, `tests/temp_gui/`)

## Architecture

```
tests/
├── run_tests.ps1                  # Main test runner with extended tests
├── run_extended_tests.bat         # Batch runner for all extended tests
│
├── IntegrationTests.ps1           # GUI automation (UI Automation API)
├── Test-ArchiveFormats.ps1        # ✨ NEW: All archive formats testing
├── Test-PartitionInfo.ps1         # ✨ NEW: Partition detection testing
├── Test-PartitionDetection.ps1    # Legacy partition test (manual)
│
├── ImageWriterCLI.ps1             # CLI wrapper (uses UI Automation)
├── Inspect-ImageWriterGUI.ps1     # GUI debugging utility
│
├── ImageWriterTests.dpr           # DUnit test runner (Delphi 7)
├── DeviceDetectionTests.pas       # Device enumeration tests (11 tests)
├── SafeOperationsTests.pas        # Safety mechanism tests (8 tests)  
├── ZLibTests.pas                  # Compression library tests (5 tests)
├── BatchLogHelperTests.pas        # ✨ NEW (23.12.2025): Log batching tests (7 tests)
├── ArchivePartitionReaderTests.pas # ✨ NEW (23.12.2025): Partition analysis tests (6 tests)
├── DeviceBenchmarkTests.pas       # ✨ NEW (23.12.2025): Device benchmarking tests (7 tests)
├── OperationUIHelperTests.pas     # ✨ NEW (23.12.2025): UI helper function tests (8 tests)
├── ArchiveIntegrationTests.pas    # Archive handling tests
├── IPCIntegrationTests.pas        # IPC communication tests
├── run_tests.bat                  # DUnit compilation script
│
└── test-data/
    ├── archives/                  # Archive format test files
    └── partition-tests/           # MBR/GPT test images
```

## Archive Format Tests (NEW)

**Test-ArchiveFormats.ps1** provides comprehensive testing for all supported archive formats:

### Supported Formats
- **GZIP** (.img.gz) - Standard GNU Zip compression
- **ZIP** (.zip) - PKZip archive format
- **XZ** (.img.xz) - LZMA2 compression
- **BZIP2** (.img.bz2) - Burrows-Wheeler compression
- **7-Zip** (.7z) - 7z archive format
- **TAR.GZ** (.tar.gz) - Tarball with GZIP
- **TAR.XZ** (.tar.xz) - Tarball with XZ

### Test Phases
1. **Phase 1: Device → Archive (Read Tests)**
   - ImageWriter reads from device and creates archive
   - Tests each format's magic bytes verification
   - Validates compression and file integrity
   
2. **Phase 2: Archive → Device (Recognition Tests)**
   - ImageWriter loads and recognizes archive format
   - Non-destructive (no actual writing to device)
   - Verifies format detection and metadata parsing

### Running Archive Tests
```powershell
# Full test suite
.\Test-ArchiveFormats.ps1 -Verbose

# Skip write tests (recognition only)
.\Test-ArchiveFormats.ps1 -SkipWriteTests

# Run via main test runner
.\run_tests.ps1 -ArchiveTests
```

### Magic Bytes Verification
Each format is validated by checking its signature:
- GZIP: `0x1F 0x8B`
- ZIP: `0x50 0x4B` (PK)
- XZ: `0xFD 0x37 0x7A 0x58 0x5A 0x00`
- BZIP2: `0x42 0x5A 0x68` (BZh)
- 7-Zip: `0x37 0x7A 0xBC 0xAF 0x27 0x1C`

## Partition Detection Tests (NEW)

**Test-PartitionInfo.ps1** provides automated testing for partition table detection:

### Test Image Creation
- **MBR Test Image** (100MB)
  - Partition 1: NTFS, Bootable, 50MB (LBA 2048)
  - Partition 2: FAT32, 50MB (LBA 104448)
  - Valid MBR signature: 0x55AA
  
- **GPT Test Image** (100MB)
  - Protective MBR with 0xEE type
  - GPT Header with "EFI PART" signature
  - Partition 1: EFI System Partition (LBA 2048-104447)
  - Partition 2: Microsoft Basic Data (LBA 104448-204799)

### Test Scenarios
1. **Direct Image Testing**
   - Load .img file in ImageWriter
   - Verify partition table detection (MBR/GPT)
   - Check partition count and metadata
   
2. **Archive Testing**
   - Create GZIP/ZIP archives of test images
   - Load archive in ImageWriter
   - Verify partition detection from compressed source
   - Tests stream-based analysis (no full extraction)

### Running Partition Tests
```powershell
# Create test images and run all tests
.\Test-PartitionInfo.ps1 -CreateTestImages -Verbose

# Use existing test images
.\Test-PartitionInfo.ps1

# Run via main test runner
.\run_tests.ps1 -PartitionTests
```

### Expected Output
ImageWriter should display in partition preview area:
```
MBR Image:
  - Partition table: MBR detected
  - Found 2 partition(s)
  - Partition 1: Type: 0x07 - NTFS/exFAT, Bootable
  - Partition 2: Type: 0x0B - FAT32

GPT Image:
  - Partition table: GPT detected
  - Found 2 partition(s)
  - Partition 1: EFI System Partition
  - Partition 2: Microsoft Basic Data
```

### Stream-Based Analysis
The partition detection from archives uses ImageWriter's efficient stream-based approach:
- **Only reads first 64KB** from archive for analysis
- No full archive extraction required
- Supports all 7 archive formats
- 99.998% memory savings vs full extraction

## How It Works

**GUI Automation Flow:**
1. Script starts ImageWriter.exe
2. Uses UI Automation API to find window (`TMainForm`)
3. Sets file path with desired extension (`.img`, `.gz`, or `.zip`)
4. Clicks Read/Write button
5. Monitors log control for completion
6. **ImageWriter.exe creates compressed archives automatically**
7. Script verifies output file exists and has correct format

**No Manual Compression:**
- ❌ Scripts do NOT call `Compress-Archive`
- ❌ Scripts do NOT use `GZipStream` directly
- ✅ ImageWriter.exe detects file extension and creates archive
- ✅ Scripts only pass paths and control UI

## Development

To add new tests:
1. Use `Inspect-ImageWriterGUI.ps1` to find UI element names
2. Add test function to `IntegrationTests.ps1`
3. Follow UI Automation pattern (find control → set value → invoke)

    [PASS] TestZLibDynamicLoading
    [PASS] TestZLibAvailable
    [PASS] TestCompressionDecompression

==============================================
Tests Passed: 11/11
==============================================
```

## No Removable Device Available

If no removable device is connected, some tests will be **SKIPPED**:

```
WARNING: No removable devices found - some tests will be skipped
SKIP: No removable device found
```

This is normal and safe. Device-specific tests cannot run without hardware.

## Adding New Tests

To add new tests:

1. Create new unit in `tests\` directory
2. Inherit from `TTestCase`
3. Mark test methods with `published` keyword
4. Register test in `initialization` section:

```pascal
unit MyNewTests;

interface

uses TestFramework;

type
  TMyNewTest = class(TTestCase)
  published
    procedure TestSomething;
  end;

implementation

procedure TMyNewTest.TestSomething;
begin
  CheckTrue(True, 'Test passed');
end;

initialization
  RegisterTest(TMyNewTest.Suite);

end.
```

5. Add unit to `ImageWriterTests.dpr` uses clause

## Safety Guidelines

When writing new tests:

1. **ALWAYS** verify device type before write operations
2. **NEVER** write to fixed disks in automated tests
3. **PREFER** read-only operations
4. **USE** small test data (KB, not MB/GB)
5. **VERIFY** device is removable: `GetDriveType() = DRIVE_REMOVABLE`
6. **SKIP** tests gracefully if no removable device available

## Continuous Integration

For CI/CD pipelines:

```yaml
test:
  script:
    - cd tests
    - dcc32 -B ImageWriterTests.dpr
    - ImageWriterTests.exe
  artifacts:
    when: always
    paths:
      - tests/*.dcu
      - tests/*.exe
```

## Troubleshooting

### "dcc32.exe not found"
- Ensure Delphi 7 is installed
- Add Delphi\Bin to PATH: `set PATH=%PATH%;C:\Program Files\Borland\Delphi7\Bin`

### "Access denied" errors
- Run tests as Administrator
- Some device operations require elevated privileges

### "DUnit not found"
- DUnit should be included with Delphi 7
- Check Delphi7\Source\ToolsAPI directory

### All tests skip
- Connect a removable USB device
- Ensure device is recognized by Windows
- Check Device Manager

## License

These tests are part of ImageWriter and licensed under GNU GPL v3.0.

## Contact

**Author:** Anton Zelenov <tixset@gmail.com>  
**GitHub:** https://github.com/tixset/ImageWriter
