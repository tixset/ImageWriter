# 🤝 Contributing to ImageWriter

**First off, thank you for considering contributing to ImageWriter!** It's people like you that make ImageWriter such a great tool.

## 📜 Code of Conduct

This project and everyone participating in it is governed by respect and professionalism. By participating, you are expected to uphold this standard.

## 🐞 How Can I Contribute?

### 🐛 Reporting Bugs

Before creating bug reports, please check the existing issues to avoid duplicates. When you create a bug report, include as many details as possible:

- **Use a clear and descriptive title**
- **Describe the exact steps to reproduce the problem**
- **Provide specific examples**
- **Describe the behavior you observed and what you expected**
- **Include screenshots if possible**
- **Specify your Windows version**
- **Include the ImageWriter version you're using**

### ✨ Suggesting Enhancements

Enhancement suggestions are tracked as GitHub issues. When creating an enhancement suggestion:

- **Use a clear and descriptive title**
- **Provide a detailed description of the suggested enhancement**
- **Explain why this enhancement would be useful**
- **List examples of how it would be used**

### 📥 Pull Requests

- Fork the repository and create your branch from `master` or `main`
- If you've added code, ensure it compiles with Delphi 7
- Follow the existing code style (see [CODING_STYLE_GUIDE.md](docs/CODING_STYLE_GUIDE.md))
- Add header comments to new units using the project template
- Add tests for new functionality
- Update CHANGELOG.md with your changes
- Follow commit message conventions (see [COMMIT_CONVENTIONS.md](docs/COMMIT_CONVENTIONS.md))
- Write clear, descriptive commit messages

## 📝 Commit Message Conventions

This project follows [Conventional Commits](https://www.conventionalcommits.org/) specification.

**Format:** `<type>(<scope>): <subject>`

**Examples:**
```
feat(archive): add ZIP support
fix(device): handle access denied error
docs(readme): update installation instructions
test(validation): add path sanitization tests
```

See [COMMIT_CONVENTIONS.md](docs/COMMIT_CONVENTIONS.md) for detailed guidelines.

---

## 🛠️ Development Setup

### 💻 Prerequisites

- Borland Delphi 7 or compatible compiler
- Windows development environment
- Basic knowledge of Pascal/Delphi

### 🔨 Building the Project

```cmd
build.bat
```

This will compile both ImageWriter.exe and ImageWriterPro.exe.

### 📁 Project Structure

```
ImageWriter/
├── src/              # Source code (.pas, .dfm files)
│   └── managers/     # Business logic managers (37+ modules)
├── resources/        # Resources (icons, manifests)
├── docs/             # Documentation
├── tests/            # Automated test suite (DUnit + PowerShell)
├── lib/              # External libraries (zlib1.dll)
├── *.dpr             # Delphi project files
├── *.dof             # IDE project settings
├── *.cfg             # Compiler configuration
├── BUILD_CONFIGURATION.md  # Build instructions
└── build.bat         # Build script
```

## 🎯 Coding Standards

### 📄 Unit Headers

All new units should include a header comment:

```pascal
{******************************************************************************}
{                                                                              }
{  ImageWriter - [Unit Purpose]                                               }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov                                      }
{  Based on dd for Windows by John Newbigin (http://chrysocome.net/dd)        }
{                                                                              }
{  This program is free software: you can redistribute it and/or modify       }
{  it under the terms of the GNU General Public License as published by       }
{  the Free Software Foundation, either version 3 of the License, or          }
{  (at your option) any later version.                                        }
{                                                                              }
{  Description:                                                                }
{    [Brief description of what this unit does]                               }
{                                                                              }
{******************************************************************************}
```

### ✨ Code Style

- Use meaningful variable and function names
- Add comments for complex logic
- Keep functions focused and manageable in size
- Handle exceptions appropriately
- Use try-finally blocks for resource management

### 📝 Commit Messages

- Use the present tense ("Add feature" not "Added feature")
- Use the imperative mood ("Move cursor to..." not "Moves cursor to...")
- Limit the first line to 72 characters or less
- Reference issues and pull requests when relevant

Example:
```
Add dynamic ZLib loading feature

- Replace static imports with LoadLibrary/GetProcAddress
- Add EnsureZLibAvailable function
- Update documentation

Fixes #123
```

## 🧪 Testing

Before submitting a pull request:

1. ✅ Test on multiple Windows versions if possible (7, 10, 11)
2. 🔒 Test both elevated and non-elevated scenarios
3. 💾 Test with various image formats (raw, .gz, .zip)
4. 🔍 Verify no memory leaks or resource leaks
5. 🖱️ Check that the UI responds properly

## ❓ Questions?

Feel free to open an issue with the label "question" if you have any questions about contributing.

## 📜 License

By contributing to ImageWriter, you agree that your contributions will be licensed under the GNU General Public License v3.0.

## 👏 Attribution

This project is based on [dd for Windows](http://www.chrysocome.net/dd) by John Newbigin. We acknowledge and appreciate the original work that made this project possible.

## 📧 Contact

**Project Maintainer:** Anton Zelenov (tixset@gmail.com)  
**GitHub:** https://github.com/tixset/ImageWriter
