# Device Health Monitoring Features

## Обзор

ImageWriter 2.2.0+ включает комплексную систему мониторинга здоровья устройств для предотвращения потери данных и обнаружения проблемных накопителей.

## Возможности

### 1. Автоматическая диагностика устройств

При выборе устройства автоматически выполняется проверка:

#### Уровень устройства (WMI)
- ✅ **Capacity**: Обнаружение поддельных устройств (Size = 0 или < 1 MB)
- ✅ **Serial Number**: Предупреждение при пустом серийном номере
- ✅ **Bytes per Sector**: Проверка стандартных значений (512/4096)

#### Уровень SMART
- ✅ **Overall Health**: Healthy/WARNING/CRITICAL
- ✅ **Temperature**: > 60°C предупреждение, > 70°C критично
- ✅ **ID 5 (Reallocated Sectors)**: > 0 = плохие блоки
- ✅ **ID 197 (Pending Sectors)**: > 0 = CRITICAL, нестабильные сектора
- ✅ **ID 198 (Uncorrectable)**: > 0 = CRITICAL, необратимые повреждения
- ✅ **ID 187 (Reported Errors)**: > 0 = ошибки ECC
- ✅ **Power-On Hours**: > 20000 часов = старое устройство

#### Уровень таблицы разделов
- ✅ **Partition Validation**: Обнаружение разделов больше физического диска
- ✅ **GPT/MBR Check**: Проверка корректности таблицы разделов

### 2. Трендовый анализ деградации

Сохраняется до 100 измерений SMART для каждого устройства:
- Анализ роста температуры
- Отслеживание увеличения плохих секторов
- Прогнозирование отказа накопителя

### 3. Защита от записи на проблемные устройства

#### CRITICAL устройства
- ❌ **Блокировка записи** без возможности продолжения
- Показ количества критических проблем
- Рекомендация немедленной замены

#### WARNING устройства
- ⚠️ **Диалог подтверждения** перед записью
- Показ количества предупреждений
- Возможность продолжить на свой риск

### 4. Визуальные индикаторы

#### ComboBox устройств (Owner-Draw)
- ✅ **Зелёный фон**: Здоровое устройство
- ⚠️ **Оранжевый фон**: Предупреждения
- ❌ **Красный фон**: Критические проблемы
- ❔ **Белый фон**: Статус неизвестен

#### Иконки статуса
- `✅` Healthy
- `⚠️` Warning
- `❌` Critical
- `❔` Unknown

### 5. Tooltip с кратким отчётом

При наведении на устройство в ComboBox показывается:
```
✅ SanDisk Ultra USB Device
Health: ✅ Healthy
```

Или для проблемного устройства:
```
❌ Generic Flash Disk
Health: ❌ 2 CRITICAL issues
```

### 6. Кнопка "Health Report"

Детальный отчёт включает:
- Модель и серийный номер
- Статус с иконкой
- Время последней проверки
- Количество warnings/critical
- Сообщения об ошибках
- Трендовый анализ SMART (если доступен)

Пример отчёта:
```
=== Device Health Report ===

Device: SanDisk Ultra USB Device
Serial: 4C530001240114104070
Status: ❌
Last Check: 25.12.2025 13:35:58

Issues Found:
  Warnings: 0
  Critical: 1

Error: Partition 4 exceeds disk size: 
  End LBA 500118158 > Max LBA 30603279 
  (237.87 GB > 14.60 GB)

Trend Analysis (5 measurements):
  Temperature: Avg 42°C, Max 48°C
  Bad Sectors: 3 currently [!] Degradation detected (+3 sectors)

================================
```

### 7. Мониторинг скорости записи

Автоматическое предупреждение при низкой скорости:
- Порог по умолчанию: **5 MB/s** (для USB 3.0)
- Проверка после записи первых 1 МБ
- Предупреждение каждые 10000 блоков

Пример:
```
[WARNING] Low write speed detected: 2.34 MB/s (expected > 5.0 MB/s)
```

### 8. Цветовое кодирование в логе

Автоматическое выделение проблемных значений красным:
- `WARNING`, `CRITICAL`
- `[!]` маркеры
- `(empty)` пустые поля
- Ошибочные значения в полях

## Использование

### Базовая диагностика
1. Выберите устройство из ComboBox
2. Автоматически выполняется полная диагностика
3. Статус отображается цветом фона и иконкой

### Детальный отчёт
1. Выберите устройство
2. Нажмите кнопку **"Health Report"**
3. Просмотрите детальную информацию

### Запись на проблемное устройство
- **CRITICAL**: Запись заблокирована, появляется предупреждение
- **WARNING**: Показывается диалог подтверждения
- **Healthy**: Запись выполняется без задержек

## Хранение данных

### Health Status
- Сохраняется для 10 устройств (индексы 0-9)
- Автоматически обновляется при каждом выборе
- Хранится до закрытия приложения

### SMART История
- 100 записей на устройство
- Старые записи автоматически удаляются
- Используется для трендового анализа

## Технические детали

### Структуры данных

```pascal
TDeviceHealthStatus = (dhsUnknown, dhsHealthy, dhsWarning, dhsCritical);

TDeviceHealthInfo = record
  Status: TDeviceHealthStatus;
  WarningCount: Integer;
  CriticalCount: Integer;
  LastCheckTime: TDateTime;
  SerialNumber: string;
  Model: string;
  ErrorMessage: string;
end;

TSMARTHistory = record
  CheckTime: TDateTime;
  Temperature: Integer;
  ReallocatedSectors: Integer;
  PendingSectors: Integer;
  PowerOnHours: Int64;
end;
```

### Методы

- `AnalyzeDiskHealth()` - Основная диагностика
- `GetDeviceHealthStatus()` - Получить статус устройства
- `SaveSMARTHistory()` - Сохранить SMART данные
- `AnalyzeSMARTTrend()` - Трендовый анализ
- `ValidateDeviceForWrite()` - Проверка перед записью
- `ShowHealthReport()` - Показать детальный отчёт

## Примеры проблем, которые обнаруживаются

### 1. Поддельная флешка
```
Hardware Diagnostics:
  Capacity: 14.60 GB ✓
  Serial Number: (empty) [!] Possible counterfeit device
  
Partition Table Diagnostics:
  Partition Table: [!] Partition 4 exceeds disk size: 
    End LBA 500118158 > Max LBA 30603279 (237.87 GB > 14.60 GB)
    
Health Summary:
  Status: ❌ CRITICAL - 1 critical issue(s), 1 warning(s)
  Recommendation: Partition table is corrupted - reformat the device
```

### 2. Изношенная флешка
```
SMART Diagnostics:
  Overall Health: WARNING [!] Device degradation detected
  Temperature: 58°C ✓
  ID 5 (Reallocated Sectors): 12 [!] Bad blocks detected
  ID 197 (Pending Sectors): 3 [!] CRITICAL - unstable sectors!
  
Health Summary:
  Status: ❌ CRITICAL - 1 critical issue(s), 2 warning(s)
  Recommendation: Do NOT use this device for important data!
```

### 3. Перегрев
```
SMART Diagnostics:
  Temperature: 75°C [!] Overheating - critical!
  
Health Summary:
  Status: ❌ CRITICAL - 1 critical issue(s), 0 warning(s)
  Recommendation: Do NOT use this device for important data!
```

## Известные ограничения

1. **SMART недоступен для USB**
   - Многие USB флешки не поддерживают SMART
   - Показывается: "SMART Diagnostics: Not available (may not be supported)"

2. **Требуются права администратора**
   - Для доступа к PhysicalDrive нужны права админа
   - В обычном режиме некоторые проверки могут быть недоступны

3. **История сохраняется до закрытия**
   - При перезапуске приложения история обнуляется
   - Планируется сохранение в INI-файл в будущих версиях

## Версионность

- **2.2.0**: Базовая диагностика (WMI, SMART, Partition validation)
- **2.2.1**: Добавлены визуальные индикаторы и Health Report
- **2.2.2**: Трендовый анализ и мониторинг скорости

## См. также

- [RTF_LOG_FORMATTING_GUIDE.md](RTF_LOG_FORMATTING_GUIDE.md) - Принципы цветового кодирования
- [LOGUTILS_INTEGRATION_GUIDE.md](LOGUTILS_INTEGRATION_GUIDE.md) - Система логирования
