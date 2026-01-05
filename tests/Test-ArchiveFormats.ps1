# Archive Format Tests
param(
    [string]$OutputDir = (Join-Path $PSScriptRoot "test-data\archives")
)

$testDir = $OutputDir
if (-not (Test-Path $testDir)) {
    New-Item -Path $testDir -ItemType Directory -Force | Out-Null
}

$archiveFormats = @(
    @{Name = "GZIP"; Extension = ".img.gz"; MagicBytes = @(0x1F, 0x8B)},
    @{Name = "ZIP"; Extension = ".zip"; MagicBytes = @(0x50, 0x4B)},
    @{Name = "XZ"; Extension = ".img.xz"; MagicBytes = @(0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00)},
    @{Name = "BZIP2"; Extension = ".img.bz2"; MagicBytes = @(0x42, 0x5A, 0x68)},
    @{Name = "7-Zip"; Extension = ".7z"; MagicBytes = @(0x37, 0x7A, 0xBC, 0xAF, 0x27, 0x1C)},
    @{Name = "TAR.GZ"; Extension = ".tar.gz"; MagicBytes = @(0x1F, 0x8B)},
    @{Name = "TAR.XZ"; Extension = ".tar.xz"; MagicBytes = @(0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00)}
)

function Test-MagicBytes {
    param([string]$FilePath, [byte[]]$ExpectedBytes)
    
    if (-not (Test-Path $FilePath)) { return $false }
    
    $fs = [System.IO.File]::OpenRead($FilePath)
    try {
        $buffer = New-Object byte[] $ExpectedBytes.Length
        $read = $fs.Read($buffer, 0, $ExpectedBytes.Length)
        
        if ($read -ne $ExpectedBytes.Length) { return $false }
        
        for ($i = 0; $i -lt $ExpectedBytes.Length; $i++) {
            if ($buffer[$i] -ne $ExpectedBytes[$i]) { return $false }
        }
        
        return $true
    } finally {
        $fs.Close()
    }
}

Write-Host ""
Write-Host "ImageWriter Archive Format Tests" -ForegroundColor Cyan
Write-Host ""

$results = @{Passed = 0; Failed = 0; Tests = @()}

foreach ($format in $archiveFormats) {
    $archivePath = Join-Path $testDir ("test" + $format.Extension)
    
    Write-Host ("Testing " + $format.Name + "...") -ForegroundColor Yellow
    
    if (Test-Path $archivePath) {
        if (Test-MagicBytes -FilePath $archivePath -ExpectedBytes $format.MagicBytes) {
            Write-Host ("  Magic bytes valid") -ForegroundColor Green
            $results.Passed++
            $results.Tests += ("[PASS] " + $format.Name + " magic bytes")
        } else {
            Write-Host ("  Invalid magic bytes") -ForegroundColor Red
            $results.Failed++
            $results.Tests += ("[FAIL] " + $format.Name + " magic bytes")
        }
    } else {
        Write-Host ("  Archive not found (skip)") -ForegroundColor Yellow
    }
}

Write-Host ""
Write-Host "Results Summary:" -ForegroundColor Cyan
Write-Host ("Passed: " + $results.Passed.ToString()) -ForegroundColor Green
Write-Host ("Failed: " + $results.Failed.ToString()) -ForegroundColor Red

if ($results.Failed -eq 0) {
    Write-Host "All tests passed!" -ForegroundColor Green
    exit 0
} else {
    Write-Host "Some tests failed!" -ForegroundColor Red
    exit 1
}
