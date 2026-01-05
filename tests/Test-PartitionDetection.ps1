# Test-PartitionDetection.ps1
# Тест функции обнаружения разделов

Write-Host "=== Partition Detection Test ===" -ForegroundColor Cyan
Write-Host ""

# Создаем тестовый MBR образ
$testFile = "Q:\ImageWriter\tests\test-data\test_mbr.img"
Write-Host "Creating test MBR image..." -ForegroundColor Yellow

# 10MB образ
$img = New-Object byte[] (10MB)

# MBR сектор
$mbr = New-Object byte[] 512

# Сигнатура MBR
$mbr[510] = 0x55
$mbr[511] = 0xAA

# Раздел 1: Bootable NTFS, начало 2048, размер 1048576 секторов (512MB)
$mbr[446] = 0x80    # Bootable
$mbr[450] = 0x07    # NTFS
$mbr[454] = 0x00    # Start LBA (little-endian)
$mbr[455] = 0x08
$mbr[456] = 0x00
$mbr[457] = 0x00
$mbr[458] = 0x00    # Size in sectors
$mbr[459] = 0x00
$mbr[460] = 0x10
$mbr[461] = 0x00

# Раздел 2: FAT32, начало 1050624, размер 524288 секторов (256MB)
$mbr[462] = 0x00    # Not bootable
$mbr[466] = 0x0B    # FAT32
$mbr[470] = 0x00    # Start LBA
$mbr[471] = 0x08
$mbr[472] = 0x10
$mbr[473] = 0x00
$mbr[474] = 0x00    # Size in sectors
$mbr[475] = 0x00
$mbr[476] = 0x08
$mbr[477] = 0x00

# Копируем MBR в образ
[Array]::Copy($mbr, 0, $img, 0, 512)

# Записываем файл
[IO.File]::WriteAllBytes($testFile, $img)
Write-Host "Created: $testFile (10MB)" -ForegroundColor Green
Write-Host ""

# Проверяем содержимое MBR
Write-Host "MBR Signature:" -ForegroundColor Yellow
$mbrCheck = [IO.File]::ReadAllBytes($testFile)
Write-Host ("  0x{0:X2}{1:X2}" -f $mbrCheck[511], $mbrCheck[510]) -ForegroundColor White

Write-Host ""
Write-Host "Partition 1:" -ForegroundColor Yellow
Write-Host ("  Boot: 0x{0:X2}" -f $mbrCheck[446]) -ForegroundColor White
Write-Host ("  Type: 0x{0:X2} (NTFS)" -f $mbrCheck[450]) -ForegroundColor White
$startLBA1 = [BitConverter]::ToUInt32($mbrCheck, 454)
$size1 = [BitConverter]::ToUInt32($mbrCheck, 458)
Write-Host ("  Start LBA: {0}" -f $startLBA1) -ForegroundColor White
Write-Host ("  Size: {0} sectors ({1} MB)" -f $size1, ($size1 * 512 / 1MB)) -ForegroundColor White

Write-Host ""
Write-Host "Partition 2:" -ForegroundColor Yellow
Write-Host ("  Boot: 0x{0:X2}" -f $mbrCheck[462]) -ForegroundColor White
Write-Host ("  Type: 0x{0:X2} (FAT32)" -f $mbrCheck[466]) -ForegroundColor White
$startLBA2 = [BitConverter]::ToUInt32($mbrCheck, 470)
$size2 = [BitConverter]::ToUInt32($mbrCheck, 474)
Write-Host ("  Start LBA: {0}" -f $startLBA2) -ForegroundColor White
Write-Host ("  Size: {0} sectors ({1} MB)" -f $size2, ($size2 * 512 / 1MB)) -ForegroundColor White

Write-Host ""
Write-Host "=== Test image created successfully ===" -ForegroundColor Green
Write-Host ""
Write-Host "Now run ImageWriter.exe and select this file:" -ForegroundColor Cyan
Write-Host "  $testFile" -ForegroundColor White
Write-Host ""
Write-Host "Expected output in log:" -ForegroundColor Cyan
Write-Host "  - Partition table: MBR detected" -ForegroundColor White
Write-Host "  - Found 2 partition(s)" -ForegroundColor White
Write-Host "  - Partition 1: Type: 0x07 - NTFS/exFAT, Bootable" -ForegroundColor White
Write-Host "  - Partition 2: Type: 0x0B - FAT32" -ForegroundColor White
Write-Host ""
