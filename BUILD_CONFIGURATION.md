# 🔧 Build Configuration

## 🏗️ Project Compilation

ImageWriter can be compiled in two ways:

### 1. Via Command Line (build.bat)
```batch
build.bat
```
This script automatically compiles both projects:
- `ImageWriter.exe` - base version
- `ImageWriterPro.exe` - enhanced version with network share support

### 2. Via Delphi 7 IDE

Open `ImageWriter.dpr` or `ImageWriterPro.dpr` in Delphi 7 IDE.

**Important:** Configuration files are required for IDE compilation, which are already created in the project root:

**Project Settings (.dof files):**
- `ImageWriter.dof` - settings for ImageWriter.dpr (used by IDE)
- `ImageWriterPro.dof` - settings for ImageWriterPro.dpr (used by IDE)

**Command-Line Parameters (.cfg files):**
- `ImageWriter.cfg` - configuration for compilation via DCC32
- `ImageWriterPro.cfg` - configuration for compilation via DCC32

These files contain:
- Module search paths (`-U`, `-O`, `-I`, `-R`)
- Compiler definitions (`-dZLIB_USE_DLL`, `-dIMAGEWRITER_PRO`)
- Compiler parameters (optimization, debugging, etc.)

### 📂 Path Structure

`.cfg` files are configured with the following module search paths:
```
src
src\studio
src\studio\md5
src\studio\sha256
src\studio\random
```

### ⚙️ Compiler Definitions

- `ZLIB_USE_DLL` - use zlib1.dll instead of static linking
- `IMAGEWRITER_PRO` - activates Pro version features (only in ImageWriterPro.cfg)

## 🔍 Troubleshooting

### ❌ Error "File not found: 'studio_tools.dcu'" in IDE
1. **Close Delphi 7 IDE if it is open**
2. Ensure that `ImageWriter.dof` and `ImageWriterPro.dof` files exist in the project root
3. Reopen the project - IDE will automatically read the .dof file
4. Check paths in Project → Options → Directories/Conditionals:
   - Search path should contain: `src;src\studio;src\studio\md5;src\studio\sha256;src\studio\random`
   - Conditionals should contain: `ZLIB_USE_DLL` (and `IMAGEWRITER_PRO` for Pro version)

### ❌ Error "File not found: 'studio_tools.dcu'"
Ensure that `ImageWriter.cfg` or `ImageWriterPro.cfg` file exists in the project root.

### ❌ DCU files not found
Check paths in `.cfg` files - they must be relative to the project root.

### ❌ build.bat doesn't work
Ensure that the path to DCC32.EXE in build.bat is correct:
```batch
SET DCC32="C:\Program Files (x86)\Borland\Delphi7\Bin\DCC32.EXE"
```

## 📌 Notes

- **`.dof` files** - used by Delphi 7 IDE to store project settings (paths, definitions, version)
- **`.cfg` files** - used by DCC32 compiler for command-line compilation
- `build.bat` passes parameters via command line and also uses `.cfg` files
- DCU files are created in module directories during compilation
- **When first opening a project in IDE** - Delphi 7 automatically reads the .dof file and applies settings
- After changing paths in `build.bat`, you need to update the corresponding `.cfg` and `.dof` files

## 📁 Configuration File Structure

```
ImageWriter/
├── ImageWriter.dpr          # Main project
├── ImageWriter.dof          # IDE settings for ImageWriter
├── ImageWriter.cfg          # Compiler parameters for ImageWriter
├── ImageWriterPro.dpr       # Pro project
├── ImageWriterPro.dof       # IDE settings for ImageWriterPro
├── ImageWriterPro.cfg       # Compiler parameters for ImageWriterPro
└── build.bat                # Build script
```
