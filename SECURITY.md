# 🔒 Security Policy

## ✅ Supported Versions

We release patches for security vulnerabilities in the following versions:

| Version | Supported          |
| ------- | ------------------ |
| 2.2.x   | :white_check_mark: |
| 2.1.x   | :white_check_mark: |
| 2.0.x   | :white_check_mark: |
| < 2.0   | :x:                |

## 🚨 Reporting a Vulnerability

**⚠️ Please do not report security vulnerabilities through public GitHub issues.**

If you discover a security vulnerability in ImageWriter, please report it responsibly:

### 📧 Contact

- **Email:** tixset@gmail.com
- **GitHub:** https://github.com/tixset/ImageWriter
- **Subject:** `[SECURITY] ImageWriter Vulnerability Report`

### 📝 What to Include

Please include the following information in your report:

1. **Type of vulnerability** (e.g., buffer overflow, path traversal, privilege escalation)
2. **Affected component** (e.g., file handling, device access, archive extraction)
3. **Steps to reproduce** the vulnerability
4. **Potential impact** of the vulnerability
5. **Suggested fix** (if you have one)
6. **Your name/handle** (for credit in the fix announcement)

### ⏱️ Response Timeline

- **Initial Response:** Within 48 hours
- **Triage:** Within 1 week
- **Fix for Critical Issues:** Within 7-14 days
- **Fix for Non-Critical Issues:** Within 30 days

### 🔄 Security Update Process

1. We will acknowledge receipt of your report
2. We will investigate and validate the vulnerability
3. We will develop and test a fix
4. We will release a security update
5. We will publicly disclose the vulnerability (after the fix is released)
6. We will credit you in the release notes (unless you prefer to remain anonymous)

## 🛡️ Security Best Practices for Users

When using ImageWriter:

1. **Administrator Privileges:** Only run ImageWriter with administrator privileges when necessary
2. **Verify Images:** Always verify checksums (MD5/SHA-256) of downloaded disk images
3. **Backup Data:** Ensure all important data is backed up before writing to any device
4. **Device Selection:** Double-check the target device before starting write operations
5. **Source Files:** Only use disk images from trusted sources
6. **Updates:** Keep ImageWriter updated to the latest version

## ⚠️ Known Security Considerations

### By Design

- **Elevated Privileges:** ImageWriter requires administrator privileges for disk write operations (by design)
- **Direct Disk Access:** The application performs low-level disk operations (intended functionality)
- **File System Bypass:** Writing raw disk images bypasses file system protections (required for operation)

### Mitigations

- **User Confirmation:** Multiple confirmation dialogs before destructive operations
- **Device Information:** Clear display of target device information
- **Volume Locking:** Volumes are locked during write operations
- **Validation:** Input validation for paths, sizes, and parameters
- **UAC Integration:** Proper UAC elevation handling on Vista and later

## ✨ Security Enhancements

Recent security improvements (v2.2.0+):

- Fixed race condition in streaming copy operations
- Added GPT overflow vulnerability protection
- Implemented proper handle leak prevention
- Enhanced input validation for file paths
- Improved error handling to prevent information disclosure

## 📢 Disclosure Policy

- We follow **responsible disclosure** practices
- Security vulnerabilities will be disclosed **90 days** after a fix is available
- Critical vulnerabilities affecting user data will be prioritized

## 🏆 Hall of Fame

We appreciate security researchers who help improve ImageWriter:

<!-- Security researchers who reported vulnerabilities will be listed here -->

---

**Last Updated:** December 22, 2025  
**Contact:** Anton Zelenov (tixset@gmail.com)  
**GitHub:** https://github.com/tixset/ImageWriter
