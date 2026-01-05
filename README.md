# 💾 ImageWriter

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Platform](https://img.shields.io/badge/Platform-Windows-0078d4.svg)](https://www.microsoft.com/windows)
[![Language](https://img.shields.io/badge/Language-Delphi%207-ee1f35.svg)](https://www.embarcadero.com/products/delphi)
[![Version](https://img.shields.io/badge/version-2.2.0-blue)](https://github.com/tixset/ImageWriter/releases)
[![Status](https://img.shields.io/badge/Status-Active-success.svg)](https://github.com/tixset/ImageWriter)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](CONTRIBUTING.md)

[![GitHub Stars](https://img.shields.io/github/stars/tixset/ImageWriter?style=social)](https://github.com/tixset/ImageWriter/stargazers)
[![GitHub Forks](https://img.shields.io/github/forks/tixset/ImageWriter?style=social)](https://github.com/tixset/ImageWriter/network/members)
[![GitHub Issues](https://img.shields.io/github/issues/tixset/ImageWriter)](https://github.com/tixset/ImageWriter/issues)
[![Last Commit](https://img.shields.io/github/last-commit/tixset/ImageWriter)](https://github.com/tixset/ImageWriter/commits)
[![Code Size](https://img.shields.io/github/languages/code-size/tixset/ImageWriter)](https://github.com/tixset/ImageWriter)

[![English](https://img.shields.io/badge/lang-English-blue.svg)](docs/README.md)
[![Russian](https://img.shields.io/badge/lang-Русский-red.svg)](docs/README.ru.md)

*A modern disk image writing utility for USB drives and SD cards*

## 📸 Screenshot

![Main window — project list](assets/screenshots/main-window.png)

---

## 📖 About

ImageWriter is a modernized and enhanced version of the dd for Windows utility, originally developed by John Newbigin. This project provides a graphical interface and additional features for writing disk images to removable media.

---

## 👤 Credits

**Original Project:** [dd for Windows](http://www.chrysocome.net/dd) by John Newbigin (jn@chrysocome.net)  
**Current Developer:** Anton Zelenov (tixset@gmail.com)  
**GitHub:** https://github.com/tixset/ImageWriter  
**License:** [GNU General Public License v3.0](LICENSE)

## 📁 Project Structure

```
ImageWriter/
├── src/                   # Source code
│   ├── *.pas, *.dfm      # Delphi modules and forms
│   ├── managers/         # Business logic managers
│   │   ├── ArchiveHandler.pas    # Archive format support
│   │   ├── DeviceBenchmark.pas   # Performance testing
│   │   ├── DeviceManager.pas     # Device operations
│   │   └── ...          # Other manager modules
│   └── studio/           # Helper modules
│       ├── md5/          # MD5 hashing
│       ├── random/       # Random number generator
│       └── sha256/       # SHA-256 hashing
├── tests/                 # Automated test suite
│   ├── *.pas             # Test modules (DUnit)
│   ├── ImageWriterTests.dpr  # Test runner
│   ├── run_tests.bat     # Test execution script
│   ├── *.ps1             # PowerShell integration tests
│   └── README.md         # Test documentation
├── docs/                  # Documentation
│   ├── README.md         # Main documentation (English)
│   ├── README.ru.md      # Documentation in Russian
│   ├── CODING_STYLE_GUIDE.md   # Code style guidelines
│   ├── COMMIT_CONVENTIONS.md   # Git commit standards
│   └── ...              # Other documentation files
├── resources/            # Application resources
│   ├── *.rc             # Resource files
│   └── *.RES            # Compiled resources
├── lib/                  # External libraries
│   └── zlib1.dll        # ZLib compression library
├── LICENSE              # GNU GPL v3.0 license
├── CHANGELOG.md         # Version history
├── CONTRIBUTING.md      # Contribution guidelines
├── CODE_OF_CONDUCT.md   # Community guidelines
├── ROADMAP.md           # Development roadmap
├── SECURITY.md          # Security policy
├── BUILD_CONFIGURATION.md  # Build instructions (IDE + command-line)
├── *.dpr                # Delphi project files
├── *.dof                # Delphi IDE project settings
├── *.cfg                # Compiler configuration (command-line)
├── build.bat            # Build script
└── .gitignore

```

## ✨ Features

- 🖥️ **Graphical User Interface** - Easy-to-use Windows GUI
- ⌨️ **Command-Line Interface (CLI)** - Console mode for automation and scripting
- 💾 **Multiple Image Formats** - Support for raw, GZIP (.gz), ZIP (.zip), XZ, BZIP2, 7z, TAR
- ✅ **Hash Verification** - MD5 and SHA-256 checksums for data integrity
- 📊 **Partition Information** - Display MBR and GPT partition tables (even from archives!)
- 🔒 **UAC Elevation** - Automatic privilege elevation on Vista and later
- ⚡ **High Performance** - Optimized I/O operations with progress tracking
- 🎯 **Drag & Drop** - Simple file selection via drag and drop
- 🔧 **Advanced Options** - Block size, count, seek, and skip parameters
- 🛡️ **Safe Operations** - Volume locking and dismounting for data safety
- 📊 **Progress Tracking** - Real-time progress bar and status updates with ETA and speed
- 🌐 **Network Shares** - Pro version supports network-mounted drives (UNC paths)
- 🔄 **Dynamic Loading** - On-demand library loading for better startup performance
- 📝 **Comprehensive Logging** - Detailed operation logs for troubleshooting
- 🔧 **Clean Code Architecture** - Modular design with 37+ manager methods
- ✅ **Automated Testing** - DUnit tests and PowerShell integration tests
- 🎯 **Active Development** - Recent refactoring and GitHub-ready infrastructure
- 🛡️ **GitHub Ready** - Full CI/CD workflows, issue templates, security policy
- 🔬 **Device Benchmarking** - Test disk performance with read/write benchmarks
- 🏥 **Device Health Monitoring** - Automatic diagnostics with WMI, SMART, and partition validation
- 🛡️ **Smart Safety Features** - Visual health indicators and write protection for critical devices

## 🚀 Quick Start

### Prerequisites

- Windows 7 or later (Vista supported with limitations)
- Administrator privileges for writing to disk devices
- zlib1.dll (included, auto-extracted when needed)

### Building from Source

```cmd
build.bat
```

Requires: Borland Delphi 7 or compatible compiler

### Usage

#### GUI Mode

1. Run **ImageWriter.exe** or **ImageWriterPro.exe**
2. Select source image file or device
3. Select target device
4. Configure options (block size, count, etc.)
5. Click "Write" or "Read"

**Versions:**
- **ImageWriter.exe** - Standard version for local drives
- **ImageWriterPro.exe** - Extended version with network share support

#### CLI Mode (Console)

**List available devices:**
```cmd
ImageWriter.exe --cli --list
```

**Write image to USB drive:**
```cmd
ImageWriter.exe --cli --write --device \\.\PhysicalDrive2 --file ubuntu.iso --verify
```

**Read device to compressed image:**
```cmd
ImageWriter.exe --cli --read --device E:\ --file backup.img.gz --hash md5
```

**Verify device against image:**
```cmd
ImageWriter.exe --cli --verify --device E:\ --file backup.img --hash sha256
```

**Show help:**
```cmd
ImageWriter.exe --cli --help
```

**CLI Options:**
- `--device <path>` - Device path (e.g., `\\.\PhysicalDrive2` or `E:\`)
- `--file <path>` - Image file (.img, .iso, .gz, .zip)
- `--bs <size>` - Block size in bytes (default: 1048576)
- `--count <n>` - Number of blocks to copy
- `--hash <md5|sha256>` - Calculate hash after operation
- `--verify` - Verify written data
- `--force` - Skip safety checks (dangerous!)
- `--quiet` - Suppress progress output

See `ImageWriter.exe --cli --help` for complete documentation.

## 📖 Documentation

Full documentation is available in the [docs/](docs/) directory:
- [User Guide (EN)](docs/README.md)
- [Руководство пользователя (RU)](docs/README.ru.md)
- [Coding Style Guide](docs/CODING_STYLE_GUIDE.md)
- [Contributing Guidelines](CONTRIBUTING.md)

## 🧪 Testing

Run automated tests:

```cmd
cd tests
run_tests.bat
```

Tests include:
- **Unit Tests** - DUnit framework for core functionality
- **Integration Tests** - PowerShell scripts for real-world scenarios
- **GUI Automation** - UI testing via Windows Automation API

## 🤝 Contributing

We welcome contributions! Please read [CONTRIBUTING.md](CONTRIBUTING.md) before submitting pull requests.

## 📜 License

GNU General Public License v3.0 - see [LICENSE](LICENSE) for details.

## 🔒 Security

If you discover a security vulnerability, please see [SECURITY.md](SECURITY.md) for responsible disclosure guidelines.

## 📚 Additional Documentation

ImageWriter includes an automated test suite:

```cmd
cd tests
run_tests.bat
```

**Safety Features:**
- Tests ONLY on removable devices (USB drives)
- NEVER tests on fixed disks
- Automatic device type verification
- Safe read-only operations

See [tests/README.md](tests/README.md) for details.

## 🛠️ Technology Stack

- **Delphi 7** (Borland Delphi Version 15.0)
- **Win32 API** for low-level disk access
- **Named Pipes** for IPC in Pro version
- **ZLib** for compressed image support (dynamic loading)

## 🤝 Contributing

Contributions are welcome! Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

**Additional Resources:**
- [Code of Conduct](CODE_OF_CONDUCT.md)
- [Security Policy](SECURITY.md)
- [Development Roadmap](ROADMAP.md)

## 📜 License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **John Newbigin** - Original author of [dd for Windows](http://www.chrysocome.net/dd)
- The ZLib team for the compression library
- All contributors to this project

## 📞 Contact

- **Author:** Anton Zelenov (tixset@gmail.com)
- **GitHub:** https://github.com/tixset/ImageWriter
- **Original dd for Windows:** John Newbigin (jn@chrysocome.net)

## 🔗 Links

- [Original dd for Windows](http://www.chrysocome.net/dd)
- [GNU GPL v3.0 License](https://www.gnu.org/licenses/gpl-3.0.html)
- [ZLib Library](https://www.zlib.net/)

---

**Made with ❤️ for the open-source community**

**Note:** This is a derivative work based on dd for Windows.  
Original project © Chrysocome and John Newbigin, available under the GPL license.
