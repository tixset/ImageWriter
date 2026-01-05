# Partition Detection Tests
param(
    [switch]$CreateTestImages,
    [switch]$All,
    [string]$OutputDir = (Join-Path $PSScriptRoot "test-data\partition-tests")
)

if (-not $CreateTestImages -and -not $All) { $All = $true }
if ($All) { $CreateTestImages = $true }

$testDir = $OutputDir
if (-not (Test-Path $testDir)) {
    New-Item -Path $testDir -ItemType Directory -Force | Out-Null
}

function Create-TestMBRImage {
    param([string]$OutputPath)
    Write-Host "Creating MBR test image (100MB)..." -ForegroundColor Yellow
    
    $imageSize = 100MB
    $fs = [System.IO.File]::Create($OutputPath)
    try {
        $fs.SetLength($imageSize)
        $fs.Seek(0, [System.IO.SeekOrigin]::Begin) | Out-Null
        
        # Boot code area
        $bootCode = New-Object byte[] 446
        $fs.Write($bootCode, 0, 446)
        
        # Partition 1: NTFS, 50MB, LBA 2048
        $partition1 = @(0x80, 0x00, 0x02, 0x00, 0x07, 0x00, 0x00, 0x00,
                        0x00, 0x08, 0x00, 0x00, 0x00, 0x90, 0x01, 0x00)
        $fs.Write($partition1, 0, 16)
        
        # Partition 2: FAT32, 50MB
        $partition2 = @(0x00, 0x00, 0x00, 0x00, 0x0B, 0x00, 0x00, 0x00,
                        0x00, 0x98, 0x01, 0x00, 0x00, 0x90, 0x01, 0x00)
        $fs.Write($partition2, 0, 16)
        
        # Empty partitions
        $emptyPartition = New-Object byte[] 16
        $fs.Write($emptyPartition, 0, 16)
        $fs.Write($emptyPartition, 0, 16)
        
        # MBR signature
        $fs.Seek(510, [System.IO.SeekOrigin]::Begin) | Out-Null
        $fs.WriteByte(0x55)
        $fs.WriteByte(0xAA)
        
        Write-Host ("MBR image created: " + $OutputPath) -ForegroundColor Green
    } finally {
        $fs.Close()
    }
}

function Create-TestGPTImage {
    param([string]$OutputPath)
    Write-Host "Creating GPT test image (100MB)..." -ForegroundColor Yellow
    
    $imageSize = 100MB
    $fs = [System.IO.File]::Create($OutputPath)
    try {
        $fs.SetLength($imageSize)
        
        # Protective MBR
        $fs.Seek(0, [System.IO.SeekOrigin]::Begin) | Out-Null
        $bootCode = New-Object byte[] 446
        $fs.Write($bootCode, 0, 446)
        
        $protectiveMBR = @(0x00, 0x00, 0x02, 0x00, 0xEE, 0xFF, 0xFF, 0xFF,
                          0x01, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x01, 0x00)
        $fs.Write($protectiveMBR, 0, 16)
        
        $emptyPartition = New-Object byte[] 16
        $fs.Write($emptyPartition, 0, 16)
        $fs.Write($emptyPartition, 0, 16)
        $fs.Write($emptyPartition, 0, 16)
        
        $fs.Seek(510, [System.IO.SeekOrigin]::Begin) | Out-Null
        $fs.WriteByte(0x55)
        $fs.WriteByte(0xAA)
        
        # GPT Header (LBA 1)
        $fs.Seek(512, [System.IO.SeekOrigin]::Begin) | Out-Null
        $signature = [System.Text.Encoding]::ASCII.GetBytes("EFI PART")
        $fs.Write($signature, 0, 8)
        
        # Revision, header size, etc.
        $fs.Write(@(0x00, 0x00, 0x01, 0x00), 0, 4)
        $fs.Write(@(0x5C, 0x00, 0x00, 0x00), 0, 4)
        $fs.Write(@(0x00, 0x00, 0x00, 0x00), 0, 4)
        $fs.Write(@(0x00, 0x00, 0x00, 0x00), 0, 4)
        
        # Current LBA, backup LBA
        $fs.Write(@(0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00), 0, 8)
        $lastLBA = ($imageSize / 512) - 1
        $fs.Write([BitConverter]::GetBytes([uint64]$lastLBA), 0, 8)
        
        # First usable, last usable LBA
        $fs.Write(@(0x22, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00), 0, 8)
        $lastUsableLBA = $lastLBA - 33
        $fs.Write([BitConverter]::GetBytes([uint64]$lastUsableLBA), 0, 8)
        
        # Disk GUID
        $diskGuid = [Guid]::NewGuid().ToByteArray()
        $fs.Write($diskGuid, 0, 16)
        
        # Partition entries starting LBA
        $fs.Write(@(0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00), 0, 8)
        
        # Number and size of partition entries
        $fs.Write(@(0x80, 0x00, 0x00, 0x00), 0, 4)
        $fs.Write(@(0x80, 0x00, 0x00, 0x00), 0, 4)
        
        Write-Host ("GPT image created: " + $OutputPath) -ForegroundColor Green
    } finally {
        $fs.Close()
    }
}

Write-Host ""
Write-Host "ImageWriter Partition Detection Tests" -ForegroundColor Cyan
Write-Host ""

if ($CreateTestImages) {
    $mbrPath = Join-Path $testDir "test_mbr.img"
    $gptPath = Join-Path $testDir "test_gpt.img"
    
    Create-TestMBRImage -OutputPath $mbrPath
    Create-TestGPTImage -OutputPath $gptPath
    
    Write-Host ""
    Write-Host "Test images created successfully!" -ForegroundColor Green
    Write-Host ("MBR image: " + $mbrPath) -ForegroundColor Gray
    Write-Host ("GPT image: " + $gptPath) -ForegroundColor Gray
}

Write-Host ""
Write-Host "Done!" -ForegroundColor Green
