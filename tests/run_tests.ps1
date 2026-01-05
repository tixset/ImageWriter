# PowerShell Test Runner with API for automation
# This script provides structured output that can be parsed programmatically

param(
    [switch]$JsonOutput = $false,
    [switch]$Verbose = $false,
    [switch]$ArchiveTests = $false,
    [switch]$PartitionTests = $false,
    [switch]$AllTests = $false
)

$ErrorActionPreference = "Continue"

# Test result structure
class TestResult {
    [string]$TestName
    [bool]$Passed
    [string]$Message
    [double]$Duration
}

$testResults = @()
$startTime = Get-Date

Write-Host "=============================================="
Write-Host "  ImageWriter Automated Test Framework"
Write-Host "  Machine-readable output available"
Write-Host "=============================================="
Write-Host ""

# Test 1: Check for removable devices
Write-Host "[TEST] Detecting removable devices..." -ForegroundColor Cyan
$test1Start = Get-Date
$removableDevices = @(Get-WmiObject Win32_DiskDrive | Where-Object { 
    $_.MediaType -like "*Removable*" -or $_.InterfaceType -eq "USB"
})

$test1Duration = ((Get-Date) - $test1Start).TotalMilliseconds
$test1 = [TestResult]@{
    TestName = "RemovableDeviceDetection"
    Passed = $removableDevices.Count -gt 0
    Message = "Found $($removableDevices.Count) removable device(s)"
    Duration = $test1Duration
}
$testResults += $test1

if ($removableDevices.Count -gt 0) {
    Write-Host "  [PASS] $($test1.Message)" -ForegroundColor Green
    if ($Verbose) {
        foreach ($dev in $removableDevices) {
            Write-Host "    - $($dev.Model) ($($dev.Size / 1GB) GB)" -ForegroundColor Gray
        }
    }
} else {
    Write-Host "  [WARN] No removable devices found" -ForegroundColor Yellow
}
Write-Host ""

# Test 2: Verify fixed disk protection
Write-Host "[TEST] Verifying fixed disk protection..." -ForegroundColor Cyan
$test2Start = Get-Date
$fixedDisks = Get-WmiObject Win32_DiskDrive | Where-Object { 
    $_.MediaType -match "Fixed" 
}

$test2Duration = ((Get-Date) - $test2Start).TotalMilliseconds
$test2 = [TestResult]@{
    TestName = "FixedDiskProtection"
    Passed = $fixedDisks.Count -gt 0
    Message = "Detected $($fixedDisks.Count) fixed disk(s) - they are PROTECTED"
    Duration = $test2Duration
}
$testResults += $test2

Write-Host "  [PASS] $($test2.Message)" -ForegroundColor Green
if ($Verbose) {
    foreach ($disk in $fixedDisks) {
        Write-Host "    - PROTECTED: $($disk.Model) ($('{0:N2}' -f ($disk.Size / 1GB)) GB)" -ForegroundColor Gray
    }
}
Write-Host ""

# Test 3: Check ZLib DLL
Write-Host "[TEST] Checking ZLib library..." -ForegroundColor Cyan
$test3Start = Get-Date
$zlibPath = Join-Path $PSScriptRoot "..\lib\zlib1.dll"
$zlibExists = Test-Path $zlibPath

$test3Duration = ((Get-Date) - $test3Start).TotalMilliseconds
$test3 = [TestResult]@{
    TestName = "ZLibAvailability"
    Passed = $zlibExists
    Message = if ($zlibExists) { "ZLib DLL found at $zlibPath" } else { "ZLib DLL not found" }
    Duration = $test3Duration
}
$testResults += $test3

if ($zlibExists) {
    Write-Host "  [PASS] $($test3.Message)" -ForegroundColor Green
} else {
    Write-Host "  [FAIL] $($test3.Message)" -ForegroundColor Red
}
Write-Host ""

# Test 4: File system safety check
Write-Host "[TEST] File system safety verification..." -ForegroundColor Cyan
$test4Start = Get-Date
$systemDrive = $env:SystemDrive
$driveType = (Get-WmiObject Win32_LogicalDisk -Filter "DeviceID='$systemDrive'").DriveType

$test4Duration = ((Get-Date) - $test4Start).TotalMilliseconds
$test4 = [TestResult]@{
    TestName = "SystemDriveSafety"
    Passed = $driveType -eq 3  # 3 = Local Disk (fixed)
    Message = "System drive $systemDrive is type $driveType (3=Fixed, 2=Removable)"
    Duration = $test4Duration
}
$testResults += $test4

if ($test4.Passed) {
    Write-Host "  [PASS] $($test4.Message)" -ForegroundColor Green
} else {
    Write-Host "  [WARN] Unexpected system drive type" -ForegroundColor Yellow
}
Write-Host ""

# Test 5: Archive Format Tests (if enabled)
if ($ArchiveTests -or $AllTests) {
    Write-Host "[TEST] Running archive format tests..." -ForegroundColor Cyan
    $test5Start = Get-Date
    
    $archiveTestScript = Join-Path $PSScriptRoot "Test-ArchiveFormats.ps1"
    if (Test-Path $archiveTestScript) {
        try {
            $archiveResult = & $archiveTestScript -Verbose:$Verbose
            $archiveExitCode = $LASTEXITCODE
            
            $test5Duration = ((Get-Date) - $test5Start).TotalMilliseconds
            $test5 = [TestResult]@{
                TestName = "ArchiveFormatTests"
                Passed = ($archiveExitCode -eq 0)
                Message = if ($archiveExitCode -eq 0) { "All archive format tests passed" } else { "Some archive tests failed (exit code: $archiveExitCode)" }
                Duration = $test5Duration
            }
            $testResults += $test5
            
            if ($test5.Passed) {
                Write-Host "  [PASS] $($test5.Message)" -ForegroundColor Green
            } else {
                Write-Host "  [FAIL] $($test5.Message)" -ForegroundColor Red
            }
        } catch {
            Write-Host "  [ERROR] Archive tests failed: $_" -ForegroundColor Red
            $test5 = [TestResult]@{
                TestName = "ArchiveFormatTests"
                Passed = $false
                Message = "Archive tests failed: $_"
                Duration = 0
            }
            $testResults += $test5
        }
    } else {
        Write-Host "  [SKIP] Archive test script not found" -ForegroundColor Yellow
    }
    Write-Host ""
}

# Test 6: Partition Info Tests (if enabled)
if ($PartitionTests -or $AllTests) {
    Write-Host "[TEST] Running partition detection tests..." -ForegroundColor Cyan
    $test6Start = Get-Date
    
    $partitionTestScript = Join-Path $PSScriptRoot "Test-PartitionInfo.ps1"
    if (Test-Path $partitionTestScript) {
        try {
            $partitionResult = & $partitionTestScript -CreateTestImages -Verbose:$Verbose
            $partitionExitCode = $LASTEXITCODE
            
            $test6Duration = ((Get-Date) - $test6Start).TotalMilliseconds
            $test6 = [TestResult]@{
                TestName = "PartitionDetectionTests"
                Passed = ($partitionExitCode -eq 0)
                Message = if ($partitionExitCode -eq 0) { "All partition detection tests passed" } else { "Some partition tests failed (exit code: $partitionExitCode)" }
                Duration = $test6Duration
            }
            $testResults += $test6
            
            if ($test6.Passed) {
                Write-Host "  [PASS] $($test6.Message)" -ForegroundColor Green
            } else {
                Write-Host "  [FAIL] $($test6.Message)" -ForegroundColor Red
            }
        } catch {
            Write-Host "  [ERROR] Partition tests failed: $_" -ForegroundColor Red
            $test6 = [TestResult]@{
                TestName = "PartitionDetectionTests"
                Passed = $false
                Message = "Partition tests failed: $_"
                Duration = 0
            }
            $testResults += $test6
        }
    } else {
        Write-Host "  [SKIP] Partition test script not found" -ForegroundColor Yellow
    }
    Write-Host ""
}

# Summary
$totalDuration = ((Get-Date) - $startTime).TotalSeconds
$passedCount = ($testResults | Where-Object { $_.Passed }).Count
$totalCount = $testResults.Count

Write-Host "=============================================="
Write-Host "  Test Summary"
Write-Host "=============================================="
Write-Host "Total Tests: $totalCount"
Write-Host "Passed: $passedCount" -ForegroundColor Green
Write-Host "Failed: $($totalCount - $passedCount)" -ForegroundColor $(if ($totalCount -eq $passedCount) { "Green" } else { "Red" })
Write-Host "Duration: $([math]::Round($totalDuration, 2)) seconds"
Write-Host "=============================================="
Write-Host ""

# JSON output for programmatic consumption
if ($JsonOutput) {
    $output = @{
        Summary = @{
            TotalTests = $totalCount
            Passed = $passedCount
            Failed = $totalCount - $passedCount
            Duration = $totalDuration
            Timestamp = Get-Date -Format "o"
        }
        Tests = $testResults
        Environment = @{
            OS = [System.Environment]::OSVersion.VersionString
            MachineName = $env:COMPUTERNAME
            RemovableDevices = $removableDevices.Count
            FixedDisks = $fixedDisks.Count
        }
    }
    
    $jsonFile = Join-Path $PSScriptRoot "test-results.json"
    $output | ConvertTo-Json -Depth 10 | Out-File $jsonFile -Encoding UTF8
    Write-Host "JSON results saved to: $jsonFile" -ForegroundColor Cyan
}

# Exit code
if ($passedCount -eq $totalCount) {
    exit 0
} else {
    exit 1
}
