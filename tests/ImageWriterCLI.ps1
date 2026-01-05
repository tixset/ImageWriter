<#
.SYNOPSIS
    ImageWriter automation wrapper using UI Automation API

.DESCRIPTION
    Controls ImageWriter.exe programmatically via Windows UI Automation.
    ImageWriter itself creates GZ/ZIP archives based on file extension.

.PARAMETER Operation
    Read or Write

.PARAMETER Device  
    Physical device path (e.g., E:\ or \\.\PhysicalDrive2)

.PARAMETER ImageFile
    Path to image file - ImageWriter creates .gz/.zip automatically

.PARAMETER Count
    Number of MB to read/write (optional, 0 = all)

.EXAMPLE
    .\ImageWriterCLI.ps1 -Operation Read -Device E:\ -ImageFile backup.img.gz -Count 100
    Reads 100MB from E: to compressed GZ file (ImageWriter creates archive)

.NOTES
    Uses Windows UI Automation API to control ImageWriter.exe
    Requires ImageWriter.exe in parent directory
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory=$true)]
    [ValidateSet('Read', 'Write')]
    [string]$Operation,
    
    [Parameter(Mandatory=$true)]
    [string]$Device,
    
    [Parameter(Mandatory=$true)]
    [string]$ImageFile,
    
    [Parameter()]
    [int]$Count = 0
)

$ErrorActionPreference = 'Stop'

# Load UI Automation assemblies
Add-Type -AssemblyName UIAutomationClient
Add-Type -AssemblyName UIAutomationTypes

$script:ExePath = Join-Path (Split-Path $PSScriptRoot -Parent) "ImageWriter.exe"

function Get-ImageWriterWindow {
    $desktop = [System.Windows.Automation.AutomationElement]::RootElement
    $windows = $desktop.FindAll(
        [System.Windows.Automation.TreeScope]::Children,
        [System.Windows.Automation.Condition]::TrueCondition
    )
    
    foreach ($win in $windows) {
        if ($win.Current.ClassName -eq "TMainForm") {
            return $win
        }
    }
    return $null
}

function Find-Control {
    param($Parent, $ClassName, $Name)
    
    $children = $Parent.FindAll(
        [System.Windows.Automation.TreeScope]::Descendants,
        [System.Windows.Automation.Condition]::TrueCondition
    )
    
    foreach ($child in $children) {
        $match = $true
        if ($ClassName -and $child.Current.ClassName -ne $ClassName) { $match = $false }
        if ($Name -and $child.Current.Name -ne $Name) { $match = $false }
        if ($match) { return $child }
    }
    return $null
}

function Set-TextValue {
    param($Element, $Text)
    $pattern = $Element.GetCurrentPattern([System.Windows.Automation.ValuePattern]::Pattern)
    $pattern.SetValue($Text)
}

function Click-Control {
    param($Element)
    $pattern = $Element.GetCurrentPattern([System.Windows.Automation.InvokePattern]::Pattern)
    $pattern.Invoke()
}

function Wait-ForOperation {
    param($Window, [int]$TimeoutSeconds = 600)
    
    $logControl = Find-Control -Parent $Window -ClassName "TRichEdit"
    if (-not $logControl) { throw "Log control not found" }
    
    $startTime = Get-Date
    while ((Get-Date) -lt $startTime.AddSeconds($TimeoutSeconds)) {
        $textPattern = $logControl.GetCurrentPattern([System.Windows.Automation.TextPattern]::Pattern)
        $logText = $textPattern.DocumentRange.GetText(-1)
        $lastLines = ($logText -split "`n") | Select-Object -Last 10
        
        foreach ($line in $lastLines) {
            if ($line -match "\[INFO\].*Ready" -or $line -match "Operation.*complete") {
                return $true
            }
            if ($line -match "\[ERROR\]") {
                throw "Operation failed: $line"
            }
        }
        
        Start-Sleep -Milliseconds 500
    }
    
    throw "Operation timeout after $TimeoutSeconds seconds"
}

# Main execution
try {
    if (-not (Test-Path $script:ExePath)) {
        throw "ImageWriter.exe not found at: $script:ExePath"
    }
    
    Write-Host "[INFO] Starting ImageWriter.exe..." -ForegroundColor Cyan
    $process = Start-Process -FilePath $script:ExePath -PassThru
    Start-Sleep -Seconds 3
    
    try {
        $window = Get-ImageWriterWindow
        if (-not $window) { throw "ImageWriter window not found" }
        Write-Host "[INFO] Window found" -ForegroundColor Green
        
        # Set operation mode (Read/Write radio button)
        Write-Host "[INFO] Setting mode: $Operation" -ForegroundColor Cyan
        # ImageWriter defaults to Read mode, only change if Write
        if ($Operation -eq "Write") {
            $writeButton = Find-Control -Parent $window -Name "Write"
            if ($writeButton) {
                Click-Control -Element $writeButton
            }
        }
        
        Start-Sleep -Milliseconds 500
        
        # Set file path - ImageWriter will create .gz/.zip based on extension!
        Write-Host "[INFO] Setting file path: $ImageFile" -ForegroundColor Cyan
        $fileEdit = Find-Control -Parent $window -ClassName "Edit"
        if (-not $fileEdit) { throw "File path field not found" }
        Set-TextValue -Element $fileEdit -Text $ImageFile
        
        # Set device
        Write-Host "[INFO] Setting device: $Device" -ForegroundColor Cyan
        $deviceCombo = Find-Control -Parent $window -ClassName "TComboBox"
        if ($deviceCombo) {
            Set-TextValue -Element $deviceCombo -Text $Device
        }
        
        # Set count if specified
        if ($Count -and $Count -gt 0) {
            Write-Host "[INFO] Setting block count: $Count" -ForegroundColor Cyan
            # Find all Edit controls (File, BS, Count, Seek, Skip)
            # Count field is usually the 3rd Edit control
            $editControls = $window.FindAll([System.Windows.Automation.TreeScope]::Descendants, 
                (New-Object System.Windows.Automation.PropertyCondition([System.Windows.Automation.AutomationElement]::ClassNameProperty, "Edit")))
            
            if ($editControls.Count -ge 3) {
                # Third Edit control is typically the Count field
                $countEdit = $editControls[2]
                Set-TextValue -Element $countEdit -Text $Count.ToString()
                Write-Host "[INFO] Block count set to: $Count" -ForegroundColor Green
            }
            else {
                Write-Host "[WARNING] Could not find Count field (only $($editControls.Count) Edit controls found)" -ForegroundColor Yellow
            }
        }
        
        # Start operation
        Write-Host "[INFO] Starting $Operation operation..." -ForegroundColor Yellow
        $startButton = Find-Control -Parent $window -ClassName "TBitBtn"
        if (-not $startButton) { throw "Start button not found" }
        Click-Control -Element $startButton
        
        # Wait for completion
        Write-Host "[INFO] Waiting for operation to complete..." -ForegroundColor Yellow
        Wait-ForOperation -Window $window -TimeoutSeconds 600
        
        Write-Host "[SUCCESS] Operation completed!" -ForegroundColor Green
        
        # Verify file was created
        if ($Operation -eq "Read" -and -not (Test-Path $ImageFile)) {
            throw "Output file not created: $ImageFile"
        }
        
        exit 0
    }
    finally {
        if ($process -and -not $process.HasExited) {
            Write-Host "[INFO] Closing ImageWriter..." -ForegroundColor Cyan
            $process.CloseMainWindow() | Out-Null
            Start-Sleep -Seconds 1
            if (-not $process.HasExited) {
                $process.Kill()
            }
        }
    }
}
catch {
    Write-Host "[ERROR] $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}
    
    $ext = [System.IO.Path]::GetExtension($FilePath).ToLower()
    
    switch ($ext) {
        ".gz" { return "GZIP" }
        ".zip" { return "ZIP" }
        ".img" { return "RAW" }
        ".iso" { return "RAW" }
        ".bin" { return "RAW" }
        default { return "RAW" }
    }
}

function Get-DeviceSize {
    param([string]$DevicePath)
    
    # Extract device number
    if ($DevicePath -match 'PhysicalDrive(\d+)') {
        $deviceNum = $Matches[1]
        
        $disk = Get-WmiObject Win32_DiskDrive | Where-Object { $_.DeviceID -like "*PHYSICALDRIVE$deviceNum" }
        
        if ($disk) {
            return [long]$disk.Size
        }
    }
    
    throw "Cannot determine device size for: $DevicePath"
}

function Invoke-ReadOperation {
    param(
        [string]$DevicePath,
        [string]$OutputFile,
        [int]$BS,
        [long]$BlockCount
    )
    
    Write-Status "Reading from device: $DevicePath"
    Write-Status "Output file: $OutputFile"
    Write-Status "Block size: $BS bytes"
    Write-Status "Block count: $BlockCount"
    
    $format = Get-FileFormat -FilePath $OutputFile
    Write-Status "Detected format: $format"
    
    # Prepare temporary file for raw read
    $tempFile = [System.IO.Path]::GetTempFileName()
    
    try {
        # Read from device using direct file operations
        Write-Status "Opening device for reading..."
        
        $deviceStream = $null
        $outputStream = $null
        $gzipStream = $null
        
        try {
            # Open device
            $deviceStream = New-Object System.IO.FileStream(
                $DevicePath,
                [System.IO.FileMode]::Open,
                [System.IO.FileAccess]::Read,
                [System.IO.FileShare]::ReadWrite
            )
            
            # Create output based on format
            if ($format -eq "GZIP") {
                $fileStream = [System.IO.File]::Create($OutputFile)
                $gzipStream = New-Object System.IO.Compression.GZipStream(
                    $fileStream,
                    [System.IO.Compression.CompressionMode]::Compress
                )
                $outputStream = $gzipStream
            }
            else {
                $outputStream = [System.IO.File]::Create($OutputFile)
            }
            
            # Read and write blocks
            $buffer = New-Object byte[] $BS
            $totalRead = 0
            $blocksRead = 0
            
            while ($blocksRead -lt $BlockCount) {
                $bytesRead = $deviceStream.Read($buffer, 0, $BS)
                
                if ($bytesRead -eq 0) {
                    break
                }
                
                $outputStream.Write($buffer, 0, $bytesRead)
                
                $totalRead += $bytesRead
                $blocksRead++
                
                if ($blocksRead % 100 -eq 0) {
                    $pct = [math]::Round(($blocksRead / $BlockCount) * 100, 1)
                    $mb = [math]::Round($totalRead / 1MB, 2)
                    Write-Progress -Activity "Reading from device" -Status "$mb MB read ($pct%)" -PercentComplete $pct
                }
            }
            
            Write-Progress -Activity "Reading from device" -Completed
            
            Write-Status "Read complete: $totalRead bytes ($blocksRead blocks)" "SUCCESS"
        }
        finally {
            if ($outputStream) { $outputStream.Close() }
            if ($gzipStream) { $gzipStream.Close() }
            if ($deviceStream) { $deviceStream.Close() }
        }
        
        return $true
    }
    catch {
        Write-Status "Read operation failed: $_" "ERROR"
        return $false
    }
}

function Invoke-WriteOperation {
    param(
        [string]$SourceFile,
        [string]$DevicePath,
        [int]$BS
    )
    
    Write-Status "Writing to device: $DevicePath"
    Write-Status "Source file: $SourceFile"
    Write-Status "Block size: $BS bytes"
    
    if (-not (Test-Path $SourceFile)) {
        throw "Source file not found: $SourceFile"
    }
    
    $format = Get-FileFormat -FilePath $SourceFile
    Write-Status "Detected format: $format"
    
    $sourceStream = $null
    $deviceStream = $null
    $gzipStream = $null
    
    try {
        # Open source file
        Write-Status "Opening source file..."
        
        if ($format -eq "GZIP") {
            $fileStream = [System.IO.File]::OpenRead($SourceFile)
            $gzipStream = New-Object System.IO.Compression.GZipStream(
                $fileStream,
                [System.IO.Compression.CompressionMode]::Decompress
            )
            $sourceStream = $gzipStream
        }
        else {
            $sourceStream = [System.IO.File]::OpenRead($SourceFile)
        }
        
        # Open device
        Write-Status "Opening device for writing..."
        $deviceStream = New-Object System.IO.FileStream(
            $DevicePath,
            [System.IO.FileMode]::Open,
            [System.IO.FileAccess]::Write,
            [System.IO.FileShare]::None
        )
        
        # Write blocks
        $buffer = New-Object byte[] $BS
        $totalWritten = 0
        $blocksWritten = 0
        
        while ($true) {
            $bytesRead = $sourceStream.Read($buffer, 0, $BS)
            
            if ($bytesRead -eq 0) {
                break
            }
            
            $deviceStream.Write($buffer, 0, $bytesRead)
            
            $totalWritten += $bytesRead
            $blocksWritten++
            
            if ($blocksWritten % 100 -eq 0) {
                $mb = [math]::Round($totalWritten / 1MB, 2)
                Write-Progress -Activity "Writing to device" -Status "$mb MB written"
            }
        }
        
        Write-Progress -Activity "Writing to device" -Completed
        
        # Flush
        $deviceStream.Flush()
        
        Write-Status "Write complete: $totalWritten bytes ($blocksWritten blocks)" "SUCCESS"
        return $true
    }
    catch {
        Write-Status "Write operation failed: $_" "ERROR"
        return $false
    }
    finally {
        if ($sourceStream) { $sourceStream.Close() }
        if ($gzipStream) { $gzipStream.Close() }
        if ($deviceStream) { $deviceStream.Close() }
    }
}

function Invoke-VerifyOperation {
    param(
        [string]$DevicePath,
        [string]$ReferenceFile,
        [int]$BS,
        [long]$BlockCount
    )
    
    Write-Status "Verifying device against reference file..."
    Write-Status "Device: $DevicePath"
    Write-Status "Reference: $ReferenceFile"
    
    if (-not (Test-Path $ReferenceFile)) {
        throw "Reference file not found: $ReferenceFile"
    }
    
    # Read from device to temp file
    $tempFile = [System.IO.Path]::GetTempFileName()
    
    try {
        $success = Invoke-ReadOperation -DevicePath $DevicePath -OutputFile $tempFile -BS $BS -BlockCount $BlockCount
        
        if (-not $success) {
            throw "Failed to read from device for verification"
        }
        
        # Compute hashes
        Write-Status "Computing MD5 hashes..."
        
        $deviceHash = Get-FileHash -Path $tempFile -Algorithm MD5
        $referenceHash = Get-FileHash -Path $ReferenceFile -Algorithm MD5
        
        Write-Status "Device MD5:    $($deviceHash.Hash)"
        Write-Status "Reference MD5: $($referenceHash.Hash)"
        
        if ($deviceHash.Hash -eq $referenceHash.Hash) {
            Write-Status "VERIFICATION PASSED: Hashes match!" "SUCCESS"
            return $true
        }
        else {
            Write-Status "VERIFICATION FAILED: Hashes do not match!" "ERROR"
            return $false
        }
    }
    finally {
        if (Test-Path $tempFile) {
            Remove-Item $tempFile -Force
        }
    }
}

function Get-FileMD5 {
    param([string]$FilePath)
    
    $md5 = [System.Security.Cryptography.MD5]::Create()
    $stream = [System.IO.File]::OpenRead($FilePath)
    try {
        $hashBytes = $md5.ComputeHash($stream)
        $hashString = [BitConverter]::ToString($hashBytes) -replace '-', ''
        return $hashString.ToLower()
    }
    finally {
        $stream.Close()
        $md5.Dispose()
    }
}

#endregion

#region Main Execution

try {
    Write-Host "`n=============================================="
    Write-Host "  ImageWriter CLI Wrapper"
    Write-Host "==============================================`n"
    
    # Check admin privileges
    if (-not (Test-AdminPrivileges)) {
        Write-Status "ERROR: This operation requires Administrator privileges" "ERROR"
        Write-Status "Please run PowerShell as Administrator and try again" "ERROR"
        exit 1
    }
    
    Write-Status "Running with Administrator privileges" "SUCCESS"
    
    # Validate device path
    if ($Device -notmatch '^\\\\\.\\.+') {
        Write-Status "Invalid device path format. Expected: \\.\PhysicalDriveN" "ERROR"
        exit 1
    }
    
    # Auto-detect count if not specified
    if ($Count -eq 0 -and $Operation -eq "Read") {
        try {
            $deviceSize = Get-DeviceSize -DevicePath $Device
            $Count = [math]::Ceiling($deviceSize / $BlockSize)
            Write-Status "Auto-detected device size: $([math]::Round($deviceSize / 1GB, 2)) GB"
            Write-Status "Block count: $Count"
        }
        catch {
            Write-Status "Warning: Could not auto-detect device size: $_" "WARN"
            Write-Status "Please specify -Count parameter manually" "ERROR"
            exit 1
        }
    }
    
    # Execute operation
    $result = $false
    
    switch ($Operation) {
        "Read" {
            $result = Invoke-ReadOperation -DevicePath $Device -OutputFile $ImageFile -BS $BlockSize -BlockCount $Count
            
            if ($result -and $VerifyHash) {
                $hash = Get-FileMD5 -FilePath $ImageFile
                Write-Status "MD5 Hash: $hash" "SUCCESS"
                
                if ($OutputHash) {
                    $hash | Out-File -FilePath $OutputHash -Encoding ASCII
                    Write-Status "Hash saved to: $OutputHash"
                }
            }
        }
        
        "Write" {
            # Safety check
            Write-Host "`n" -NoNewline
            Write-Host "WARNING: This will OVERWRITE ALL DATA on $Device!" -ForegroundColor Red
            Write-Host "Type 'YES' to confirm: " -NoNewline
            $confirm = Read-Host
            
            if ($confirm -ne "YES") {
                Write-Status "Operation cancelled by user" "WARN"
                exit 0
            }
            
            $result = Invoke-WriteOperation -SourceFile $ImageFile -DevicePath $Device -BS $BlockSize
            
            if ($result -and $VerifyHash) {
                Write-Status "Verifying written data..."
                $fileSize = (Get-Item $ImageFile).Length
                $verifyCount = [math]::Ceiling($fileSize / $BlockSize)
                $result = Invoke-VerifyOperation -DevicePath $Device -ReferenceFile $ImageFile -BS $BlockSize -BlockCount $verifyCount
            }
        }
        
        "Verify" {
            $fileSize = (Get-Item $ImageFile).Length
            $verifyCount = if ($Count -eq 0) { [math]::Ceiling($fileSize / $BlockSize) } else { $Count }
            $result = Invoke-VerifyOperation -DevicePath $Device -ReferenceFile $ImageFile -BS $BlockSize -BlockCount $verifyCount
        }
    }
    
    Write-Host "`n=============================================="
    if ($result) {
        Write-Status "Operation completed successfully" "SUCCESS"
        exit 0
    }
    else {
        Write-Status "Operation failed" "ERROR"
        exit 1
    }
}
catch {
    Write-Host "`n=============================================="
    Write-Status "FATAL ERROR: $_" "ERROR"
    Write-Host $_.ScriptStackTrace -ForegroundColor Red
    exit 1
}

#endregion
