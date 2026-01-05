@echo off
REM ******************************************************************************
REM   ImageWriter - Test Runner Script
REM   
REM   Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>
REM   
REM   Description:
REM     Batch script to compile and run ImageWriter automated tests.
REM     Requires Delphi 7 compiler (dcc32.exe) in PATH.
REM ******************************************************************************

echo ================================================
echo   ImageWriter Automated Test Suite
echo ================================================
echo.

REM Check if DCC32 is available
where dcc32 >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: dcc32.exe not found in PATH
    echo Please ensure Delphi 7 is installed and dcc32.exe is in your PATH
    pause
    exit /b 1
)

echo Compiling test suite...
echo.

REM Compile tests
dcc32 -B -U..\src;..\lib ImageWriterTests.dpr

if %errorlevel% neq 0 (
    echo.
    echo ERROR: Compilation failed
    pause
    exit /b 1
)

echo.
echo Compilation successful!
echo.
echo ================================================
echo   Running Tests
echo ================================================
echo.
echo WARNING: Tests require a removable USB device
echo Tests will NOT touch fixed disks
echo.

REM Run tests
ImageWriterTests.exe

echo.
echo ================================================
echo   Tests Complete
echo ================================================
echo.

pause
