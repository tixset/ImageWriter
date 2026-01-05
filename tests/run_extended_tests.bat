@echo off
REM run_extended_tests.bat
REM Comprehensive test suite for ImageWriter archive and partition features

echo ========================================
echo ImageWriter Extended Test Suite
echo ========================================
echo.

echo [1/3] Running basic smoke tests...
echo ----------------------------------------
powershell -ExecutionPolicy Bypass -File "%~dp0run_tests.ps1" -Verbose
if %ERRORLEVEL% NEQ 0 (
    echo [FAIL] Basic tests failed
    exit /b 1
)
echo.

echo [2/3] Running archive format tests...
echo ----------------------------------------
powershell -ExecutionPolicy Bypass -File "%~dp0Test-ArchiveFormats.ps1" -Verbose
if %ERRORLEVEL% NEQ 0 (
    echo [WARN] Some archive tests failed
)
echo.

echo [3/3] Running partition detection tests...
echo ----------------------------------------
powershell -ExecutionPolicy Bypass -File "%~dp0Test-PartitionInfo.ps1" -CreateTestImages -Verbose
if %ERRORLEVEL% NEQ 0 (
    echo [WARN] Some partition tests failed
)
echo.

echo ========================================
echo Extended Test Suite Completed
echo ========================================
echo.
echo Test artifacts:
echo   - test-data\archives\         - Archive test files
echo   - test-data\partition-tests\  - Partition test images
echo   - test-results.json           - JSON results
echo.

pause
