# 📦 External Libraries

*This directory contains external dependencies required by ImageWriter.*

---

## 🗃️ Compression Libraries

ImageWriter supports multiple compression formats through dynamically loaded DLL libraries. All libraries are embedded as resources in the executable and are auto-extracted to `%LOCALAPPDATA%\ImageWriter\` when needed.

### 1. ZLib Library (GZIP/ZIP)

- **File:** `zlib1.dll`
- **Version:** 1.1.3 or compatible
- **Purpose:** GZIP (.gz) and ZIP (.zip) compression/decompression
- **License:** zlib License (compatible with GPL)
- **Website:** https://www.zlib.net/
- **Status:** ✅ Integrated (auto-extracted from resources)

### 2. BZIP2 Library

- **File:** `libbz2.dll`
- **Version:** 1.0.5 or compatible
- **Purpose:** BZIP2 (.bz2) compression/decompression
- **License:** BSD-style license (compatible with GPL)
- **Website:** https://sourceware.org/bzip2/
- **Status:** ✅ Integrated (auto-extracted from resources)
- **Download:** See [DOWNLOAD_INSTRUCTIONS.md](DOWNLOAD_INSTRUCTIONS.md) for manual download

### 3. LZMA Library (XZ)

- **File:** `liblzma.dll`
- **Version:** 5.4+ or compatible
- **Purpose:** XZ (.xz) and LZMA compression/decompression
- **License:** Public Domain (compatible with GPL)
- **Website:** https://tukaani.org/xz/
- **Status:** ✅ Integrated (auto-extracted from resources)
- **Download:** See [DOWNLOAD_INSTRUCTIONS.md](DOWNLOAD_INSTRUCTIONS.md) for manual download

---

## ⚡ Dynamic Loading

ImageWriter uses **on-demand dynamic loading** for all compression libraries:

- Libraries are **NOT** loaded at program startup
- Each library is loaded only when its specific format is needed:
  - `zlib1.dll` → when opening .gz or .zip files
  - `libbz2.dll` → when opening .bz2 files
  - `liblzma.dll` → when opening .xz files
- Program starts normally even if libraries are missing
- Missing libraries only affect their specific compression format

### Loading Priority

1. **Embedded resource** (extracted to `%LOCALAPPDATA%\ImageWriter\`)
2. **EXE directory** (same folder as ImageWriter.exe)
3. **lib/ subdirectory** (ImageWriter\lib\)
4. **System PATH** (Windows\System32 or other PATH locations)

---

## 💾 Installation

### Automatic (Recommended)

ImageWriter includes embedded copies of all DLL files in its resources. They will be automatically extracted to:

```
%LOCALAPPDATA%\ImageWriter\
  ├── zlib1.dll
  ├── libbz2.dll
  └── liblzma.dll
```

**No manual installation required!**

### Manual Installation

If you prefer manual installation or need to use specific versions, place DLL files in one of these locations:

1. **Same directory as ImageWriter.exe** (highest priority)
2. **lib/ subdirectory** (ImageWriter\lib\)
3. **Windows system directory** (C:\Windows\System32 for 32-bit DLLs)

### Building from Source

When building ImageWriter from source, you need to:

1. Download 32-bit versions of all three DLLs (see [DOWNLOAD_INSTRUCTIONS.md](DOWNLOAD_INSTRUCTIONS.md))
2. Place them in the `lib/` directory
3. Build with `build.bat` - DLLs will be automatically embedded as resources
4. Compiled resources are included in the final .exe file

---

## 📋 Library Comparison

| Library | Format | Compression Ratio | Speed | Use Case |
|---------|--------|------------------|-------|----------|
| zlib1.dll | GZIP, ZIP | Medium (5:1) | Fast | General purpose, web downloads |
| libbz2.dll | BZIP2 | High (7:1) | Medium | Better compression than GZIP |
| liblzma.dll | XZ, LZMA | Very High (10:1) | Slow | Linux disk images, maximum compression |

---

## 🔧 Troubleshooting

### "Failed to load DLL" Error

**Causes:**
- DLL architecture mismatch (x86 vs x64) - ImageWriter requires **32-bit DLLs**
- Missing MSVC runtime (vcruntime140.dll)
- Antivirus blocking file extraction
- Insufficient permissions for %LOCALAPPDATA%

**Solutions:**
1. Verify DLLs are 32-bit: `dumpbin /headers <dll> | findstr machine` should show `14C (x86)`
2. Install Visual C++ Redistributable: https://aka.ms/vs/17/release/vc_redist.x86.exe
3. Add ImageWriter to antivirus exclusions
4. Manually copy DLLs to EXE directory

### "Compression format not supported" Error

The specific DLL for that format is missing or failed to load. Check the log for details about which DLL failed.

---

## 📜 License Information

All libraries use GPL-compatible licenses:

- **zlib1.dll**: zlib/libpng License (permissive, GPL-compatible)
- **libbz2.dll**: BSD-style license (permissive, GPL-compatible)
- **liblzma.dll**: Public Domain / BSD-like (permissive, GPL-compatible)

ImageWriter is licensed under **GNU GPL v3.0**, and distributing these libraries together is fully compliant with all licenses.

---

## 📚 Additional Resources

- **Download Instructions:** [DOWNLOAD_INSTRUCTIONS.md](DOWNLOAD_INSTRUCTIONS.md)
- **Project Documentation:** [../docs/README.md](../docs/README.md)
- **Build Guide:** [../CONTRIBUTING.md](../CONTRIBUTING.md)
