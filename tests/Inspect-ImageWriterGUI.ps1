# Утилита для анализа структуры GUI ImageWriter
# Использует UI Automation API для определения элементов управления

Add-Type -AssemblyName UIAutomationClient
Add-Type -AssemblyName UIAutomationTypes

function Get-ImageWriterWindow {
    $desktop = [System.Windows.Automation.AutomationElement]::RootElement
    
    # Ищем все окна
    $allWindows = $desktop.FindAll([System.Windows.Automation.TreeScope]::Children, [System.Windows.Automation.Condition]::TrueCondition)
    
    Write-Host "`nВсе открытые окна:" -ForegroundColor Yellow
    foreach ($win in $allWindows) {
        $name = $win.Current.Name
        $class = $win.Current.ClassName
        Write-Host "  - Name: '$name' | Class: '$class'" -ForegroundColor Gray
    }
    
    # Ищем окно по имени (возможно "Image Writer" или другое)
    foreach ($win in $allWindows) {
        if ($win.Current.Name -match "Image.*Writer" -or $win.Current.ClassName -eq "TMainForm") {
            Write-Host "`nНайдено окно ImageWriter!" -ForegroundColor Green
            return $win
        }
    }
    
    return $null
}

function Show-ElementTree {
    param(
        [System.Windows.Automation.AutomationElement]$Element,
        [int]$Depth = 0
    )
    
    $indent = "  " * $Depth
    $name = $Element.Current.Name
    $type = $Element.Current.ControlType.ProgrammaticName
    $class = $Element.Current.ClassName
    $id = $Element.Current.AutomationId
    
    Write-Host "$indent[$type] Name: '$name' | Class: '$class' | ID: '$id'" -ForegroundColor Cyan
    
    if ($Depth -lt 3) {  # Ограничиваем глубину
        $children = $Element.FindAll([System.Windows.Automation.TreeScope]::Children, [System.Windows.Automation.Condition]::TrueCondition)
        
        foreach ($child in $children) {
            Show-ElementTree -Element $child -Depth ($Depth + 1)
        }
    }
}

Write-Host "Запуск ImageWriter.exe..." -ForegroundColor Yellow
$process = Start-Process -FilePath "..\ImageWriter.exe" -PassThru
Start-Sleep -Seconds 3

try {
    Write-Host "`nПоиск окна ImageWriter..." -ForegroundColor Yellow
    $window = Get-ImageWriterWindow
    
    if ($window) {
        Write-Host "Окно найдено!`n" -ForegroundColor Green
        Write-Host "=== Структура GUI ImageWriter ===" -ForegroundColor Green
        Show-ElementTree -Element $window
    }
    else {
        Write-Host "Окно не найдено!" -ForegroundColor Red
    }
}
finally {
    Write-Host "`nЗакрытие ImageWriter..." -ForegroundColor Yellow
    if (-not $process.HasExited) {
        $process.CloseMainWindow() | Out-Null
        Start-Sleep -Seconds 1
        if (-not $process.HasExited) {
            $process.Kill()
        }
    }
}
