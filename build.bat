@echo off
REM Build script for ImageWriter projects
cd /d "%~dp0"

SET DCC32="C:\Program Files (x86)\Borland\Delphi7\Bin\DCC32.EXE"
SET UNITS=-U"src;src\studio;src\studio\md5;src\studio\sha256;src\studio\random"
SET DEFINES=-dZLIB_USE_DLL

echo ========================================
echo Building ImageWriter projects
echo ========================================
echo.

echo Cleaning old files...
del /Q src\*.dcu 2>nul
del /Q src\studio\*.dcu 2>nul
del /Q src\studio\md5\*.dcu 2>nul
del /Q src\studio\sha256\*.dcu 2>nul
del /Q src\studio\random\*.dcu 2>nul
del /Q *.exe 2>nul
echo.


echo Building ImageWriter.exe...
%DCC32% -B %DEFINES% %UNITS% ImageWriter.dpr
if errorlevel 1 goto error
echo [OK] ImageWriter.exe compiled
echo.

echo Building ImageWriterPro.exe...
%DCC32% -B %DEFINES% %UNITS% ImageWriterPro.dpr
if errorlevel 1 goto error
echo [OK] ImageWriterPro.exe compiled
echo.

echo ========================================
echo Build completed successfully!
echo ========================================
echo.
dir /B *.exe
goto end

:error
echo.
echo ========================================
echo Build FAILED!
echo ========================================
exit /b 1

:end
