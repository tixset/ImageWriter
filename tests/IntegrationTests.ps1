# GUI Integration Tests for ImageWriter - CORRECT approach
# Program ITSELF creates GZ/ZIP archives via UI automation

param([string]$TestSize = "1GB", [switch]$SkipWriteTests)
$ErrorActionPreference = "Stop"

Add-Type -AssemblyName UIAutomationClient
Add-Type -AssemblyName UIAutomationTypes

function Get-ImageWriterWindow {
    $desktop = [System.Windows.Automation.AutomationElement]::RootElement
    $allWindows = $desktop.FindAll([System.Windows.Automation.TreeScope]::Children, [System.Windows.Automation.Condition]::TrueCondition)
    foreach ($win in $allWindows) {
        if ($win.Current.Name -match "ImageWriter" -and $win.Current.ClassName -eq "TMainForm") { return $win }
    }
    return $null
}

function Find-Control {
    param($Parent, $ClassName, $Name)
    $children = $Parent.FindAll([System.Windows.Automation.TreeScope]::Descendants, [System.Windows.Automation.Condition]::TrueCondition)
    foreach ($child in $children) {
        $match = $true
        if ($ClassName -and $child.Current.ClassName -ne $ClassName) { $match = $false }
        if ($Name -and $child.Current.Name -ne $Name) { $match = $false }
        if ($match) { return $child }
    }
    return $null
}

function Click-Button {
    param($Button)
    $invokePattern = $Button.GetCurrentPattern([System.Windows.Automation.InvokePattern]::Pattern)
    $invokePattern.Invoke()
}

function Set-TextBox {
    param($TextBox, $Text)
    $valuePattern = $TextBox.GetCurrentPattern([System.Windows.Automation.ValuePattern]::Pattern)
    $valuePattern.SetValue($Text)
}

function Wait-ForReady {
    param($Window, [int]$TimeoutSeconds = 300)
    $logControl = Find-Control -Parent $Window -ClassName "TRichEdit"
    if (-not $logControl) { throw "Log not found" }
    $startTime = Get-Date
    while ((Get-Date) -lt $startTime.AddSeconds($TimeoutSeconds)) {
        $textPattern = $logControl.GetCurrentPattern([System.Windows.Automation.TextPattern]::Pattern)
        $logText = $textPattern.DocumentRange.GetText(-1)
        $lastLines = ($logText -split "`n") | Select-Object -Last 5
        foreach ($line in $lastLines) {
            if ($line -match "\[INFO\].*Ready") { return $true }
            if ($line -match "\[ERROR\]") { throw "Error in log: $line" }
        }
        Start-Sleep -Milliseconds 500
    }
    throw "Operation timeout"
}

function Set-RadioButton {
    param($Window, $GroupName, $ButtonName)
    $group = Find-Control -Parent $Window -ClassName "TRadioGroup" -Name $GroupName
    if (-not $group) { throw "Group not found: $GroupName" }
    $button = Find-Control -Parent $group -ClassName "TGroupButton" -Name $ButtonName
    if (-not $button) { throw "Button not found: $ButtonName in $GroupName" }
    Click-Button -Button $button
}

function Test-ReadOperation {
    param($OutputPath, $FileType)
    Write-Host "`n=== Test: Read to $FileType (ImageWriter creates it!) ===" -ForegroundColor Cyan
    $process = Start-Process -FilePath "..\ImageWriter.exe" -PassThru
    Start-Sleep -Seconds 3
    try {
        $window = Get-ImageWriterWindow
        if (-not $window) { throw "ImageWriter window not found" }
        Write-Host "Window found: $($window.Current.Name)" -ForegroundColor Green
        Set-RadioButton -Window $window -GroupName "Work Mode" -ButtonName "Read"
        Start-Sleep -Milliseconds 500
        $fileEdit = Find-Control -Parent $window -ClassName "Edit"
        if (-not $fileEdit) { throw "File path field not found" }
        Set-TextBox -TextBox $fileEdit -Text $OutputPath
        Write-Host "Path set: $OutputPath (ImageWriter will create $FileType!)" -ForegroundColor Green
        $readButton = Find-Control -Parent $window -ClassName "TBitBtn" | Select-Object -First 1
        if (-not $readButton) { throw "Read button not found" }
        Click-Button -Button $readButton
        Write-Host "Reading started..." -ForegroundColor Yellow
        Wait-ForReady -Window $window -TimeoutSeconds 300
        Write-Host "Reading completed!" -ForegroundColor Green
        if (-not (Test-Path $OutputPath)) { throw "File not created: $OutputPath" }
        $fileSize = (Get-Item $OutputPath).Length
        $bytes = [System.IO.File]::ReadAllBytes($OutputPath) | Select-Object -First 2
        if ($FileType -eq "GZ") {
            if ($bytes[0] -ne 0x1F -or $bytes[1] -ne 0x8B) { throw "Not a GZ archive (invalid magic bytes)" }
        }
        elseif ($FileType -eq "ZIP") {
            if ($bytes[0] -ne 0x50 -or $bytes[1] -ne 0x4B) { throw "Not a ZIP archive (invalid magic bytes)" }
        }
        Write-Host "File created by ImageWriter: $OutputPath ($([math]::Round($fileSize/1MB, 2)) MB)" -ForegroundColor Green
        return $true
    }
    finally {
        if (-not $process.HasExited) {
            $process.CloseMainWindow() | Out-Null
            Start-Sleep -Seconds 1
            if (-not $process.HasExited) { $process.Kill() }
        }
    }
}

Write-Host "ImageWriter GUI Integration Tests" -ForegroundColor Cyan
Write-Host "Tests control REAL ImageWriter.exe via UI Automation API" -ForegroundColor Yellow
Write-Host "Program ITSELF creates GZ/ZIP archives!" -ForegroundColor Yellow
Write-Host ""

$testDir = "Q:\ImageWriter\tests\temp_gui"
if (-not (Test-Path $testDir)) { New-Item -ItemType Directory -Path $testDir | Out-Null }

$results = @{ Passed = 0; Failed = 0; Tests = @() }

try {
    $imgPath = Join-Path $testDir "test_read.img"
    if (Test-ReadOperation -OutputPath $imgPath -FileType "IMG") {
        $results.Passed++
        $results.Tests += "PASS: Read to IMG"
    }
} catch {
    $results.Failed++
    $results.Tests += "FAIL: Read to IMG - $_"
    Write-Host "FAILED: $_" -ForegroundColor Red
}

try {
    $gzPath = Join-Path $testDir "test_read.img.gz"
    if (Test-ReadOperation -OutputPath $gzPath -FileType "GZ") {
        $results.Passed++
        $results.Tests += "PASS: Read to GZ (created by ImageWriter.exe)"
    }
} catch {
    $results.Failed++
    $results.Tests += "FAIL: Read to GZ - $_"
    Write-Host "FAILED: $_" -ForegroundColor Red
}

try {
    $zipPath = Join-Path $testDir "test_read.zip"
    if (Test-ReadOperation -OutputPath $zipPath -FileType "ZIP") {
        $results.Passed++
        $results.Tests += "PASS: Read to ZIP (created by ImageWriter.exe)"
    }
} catch {
    $results.Failed++
    $results.Tests += "FAIL: Read to ZIP - $_"
    Write-Host "FAILED: $_" -ForegroundColor Red
}

Write-Host "`n=== Test Results ===" -ForegroundColor Cyan
foreach ($test in $results.Tests) {
    if ($test -like "PASS*") { Write-Host $test -ForegroundColor Green }
    else { Write-Host $test -ForegroundColor Red }
}
Write-Host "`nPassed: $($results.Passed)" -ForegroundColor Green
Write-Host "Failed: $($results.Failed)" -ForegroundColor Red

if ($results.Failed -eq 0) { exit 0 } else { exit 1 }