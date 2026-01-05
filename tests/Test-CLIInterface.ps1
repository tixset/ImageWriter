# CLI Interface Tests for ImageWriter
param(
    [switch]$Verbose
)

Write-Host ""
Write-Host "ImageWriter CLI Interface Tests" -ForegroundColor Cyan
Write-Host ""

# Check if running as administrator
$isAdmin = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

if (-not $isAdmin) {
    Write-Host "WARNING: Tests require administrator privileges" -ForegroundColor Yellow
    Write-Host "Please run PowerShell as Administrator to execute CLI tests" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "Skipping all CLI tests..." -ForegroundColor Yellow
    exit 0
}

$results = @{Passed = 0; Failed = 0; Tests = @()}
$exePath = Join-Path $PSScriptRoot "..\ImageWriter.exe"

if (-not (Test-Path $exePath)) {
    Write-Host "ERROR: ImageWriter.exe not found at $exePath" -ForegroundColor Red
    exit 1
}

# Test 1: Help command
Write-Host "[TEST] CLI --help command..." -ForegroundColor Yellow
try {
    $output = & $exePath --cli --help 2>&1
    if ($output -match "Usage:") {
        Write-Host "  PASS: Help displayed correctly" -ForegroundColor Green
        $results.Passed++
        $results.Tests += "[PASS] CLI help command"
    } else {
        throw "Help output not found"
    }
} catch {
    Write-Host ("  FAIL: " + $_.ToString()) -ForegroundColor Red
    $results.Failed++
    $results.Tests += ("[FAIL] CLI help command: " + $_.ToString())
}

# Test 2: Version command
Write-Host "[TEST] CLI --version command..." -ForegroundColor Yellow
try {
    $output = & $exePath --cli --version 2>&1
    if ($output -match "ImageWriter") {
        Write-Host "  PASS: Version displayed correctly" -ForegroundColor Green
        $results.Passed++
        $results.Tests += "[PASS] CLI version command"
    } else {
        throw "Version output not found"
    }
} catch {
    Write-Host ("  FAIL: " + $_.ToString()) -ForegroundColor Red
    $results.Failed++
    $results.Tests += ("[FAIL] CLI version command: " + $_.ToString())
}

# Test 3: List devices command
Write-Host "[TEST] CLI --list command..." -ForegroundColor Yellow
try {
    $output = & $exePath --cli --list 2>&1
    if ($output -match "Scanning|Found|devices") {
        Write-Host "  PASS: List command executed" -ForegroundColor Green
        $results.Passed++
        $results.Tests += "[PASS] CLI list devices"
    } else {
        throw "List output not found"
    }
} catch {
    Write-Host ("  FAIL: " + $_.ToString()) -ForegroundColor Red
    $results.Failed++
    $results.Tests += ("[FAIL] CLI list devices: " + $_.ToString())
}

# Test 4: Invalid command
Write-Host "[TEST] CLI error handling..." -ForegroundColor Yellow
try {
    $output = & $exePath --cli --invalid-command 2>&1
    $exitCode = $LASTEXITCODE
    if ($exitCode -ne 0) {
        Write-Host "  PASS: Invalid command rejected (exit code: $exitCode)" -ForegroundColor Green
        $results.Passed++
        $results.Tests += "[PASS] CLI error handling"
    } else {
        throw "Invalid command should return non-zero exit code"
    }
} catch {
    Write-Host ("  FAIL: " + $_.ToString()) -ForegroundColor Red
    $results.Failed++
    $results.Tests += ("[FAIL] CLI error handling: " + $_.ToString())
}

# Test 5: Missing required parameter
Write-Host "[TEST] CLI parameter validation..." -ForegroundColor Yellow
try {
    $output = & $exePath --cli --write 2>&1
    $exitCode = $LASTEXITCODE
    if ($exitCode -ne 0) {
        Write-Host "  PASS: Missing parameters detected (exit code: $exitCode)" -ForegroundColor Green
        $results.Passed++
        $results.Tests += "[PASS] CLI parameter validation"
    } else {
        throw "Missing parameters should return non-zero exit code"
    }
} catch {
    Write-Host ("  FAIL: " + $_.ToString()) -ForegroundColor Red
    $results.Failed++
    $results.Tests += ("[FAIL] CLI parameter validation: " + $_.ToString())
}

# Results summary
Write-Host ""
Write-Host "Results Summary:" -ForegroundColor Cyan
Write-Host ("Passed: " + $results.Passed.ToString()) -ForegroundColor Green
Write-Host ("Failed: " + $results.Failed.ToString()) -ForegroundColor Red

Write-Host ""
foreach ($test in $results.Tests) {
    if ($test -match "PASS") {
        Write-Host ("  " + $test) -ForegroundColor Green
    } else {
        Write-Host ("  " + $test) -ForegroundColor Red
    }
}

if ($results.Failed -eq 0) {
    Write-Host ""
    Write-Host "All CLI tests passed!" -ForegroundColor Green
    exit 0
} else {
    Write-Host ""
    Write-Host "Some CLI tests failed!" -ForegroundColor Red
    exit 1
}
