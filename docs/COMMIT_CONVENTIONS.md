# Commit Message Conventions

## Обзор

Проект ImageWriter следует [Conventional Commits](https://www.conventionalcommits.org/) спецификации для стандартизации commit messages.

---

## Формат Commit Message

```
<type>(<scope>): <subject>

<body>

<footer>
```

### Компоненты

#### 1. Type (обязательно)

Тип изменения:

- **feat:** новая функциональность
- **fix:** исправление ошибки
- **docs:** изменения в документации
- **style:** форматирование, отсутствие функциональных изменений
- **refactor:** рефакторинг без изменения функциональности
- **perf:** улучшение производительности
- **test:** добавление или изменение тестов
- **build:** изменения в системе сборки
- **ci:** изменения в CI/CD
- **chore:** прочие изменения (обновление зависимостей и т.д.)

#### 2. Scope (опционально)

Модуль или компонент проекта:

- `archive` - работа с архивами
- `device` - управление устройствами
- `hash` - хеширование
- `log` - логирование
- `ui` - пользовательский интерфейс
- `cli` - консольный интерфейс
- `test` - тесты
- `docs` - документация

#### 3. Subject (обязательно)

Краткое описание изменения:

- Используйте повелительное наклонение ("add" не "added")
- Не ставьте точку в конце
- Максимум 72 символа
- Первая буква маленькая (кроме имен собственных)

#### 4. Body (опционально)

Детальное описание:

- Объясните ЧТО и ПОЧЕМУ (не КАК)
- Используйте списки с маркерами `-`
- Разделите от subject пустой строкой

#### 5. Footer (опционально)

Дополнительная информация:

- `Closes #123` - закрывает issue
- `Fixes #456` - исправляет bug
- `BREAKING CHANGE:` - breaking changes

---

## Примеры

### Простой коммит

```
feat(archive): add support for ZIP archives
```

### Коммит с body

```
feat(archive): add support for ZIP archives

- Implement ZIP extraction using ZLib
- Add progress reporting for extraction
- Support password-protected archives (future)
```

### Коммит с footer

```
fix(hash): correct SHA-256 calculation for large files

The previous implementation used incorrect buffer size
which caused hash mismatch for files > 2GB.

Fixes #45
```

### Breaking change

```
refactor(api): change DeviceManager API

BREAKING CHANGE: EnumerateDevices now returns TDeviceList
instead of TStringList. Update all callers.
```

### Multiple scopes

```
feat(cli,device): add --list command to CLI

- Integrate DeviceManager with ConsoleMode
- Display device information in table format
```

---

## Рекомендации

### ✅ DO

```bash
# Хорошо
feat(ui): add progress bar for write operation
fix(device): handle access denied error gracefully
docs(readme): update installation instructions
test(validation): add tests for path sanitization
```

### ❌ DON'T

```bash
# Плохо
Update files
Fixed bug
WIP
asdfasdf
feature
```

---

## Специальные случаи

### Revert commits

```
revert: feat(archive): add ZIP support

This reverts commit abc1234.
Reason: Incompatible with Windows XP.
```

### Merge commits

```
Merge branch 'feature/zip-support' into develop
```

### Initial commit

```
chore: initial commit
```

---

## Автоматизация

### Git Hooks

Создайте `.git/hooks/commit-msg`:

```bash
#!/bin/sh
# Validate commit message format

commit_msg_file=$1
commit_msg=$(cat "$commit_msg_file")

# Check format: type(scope): subject
pattern="^(feat|fix|docs|style|refactor|perf|test|build|ci|chore|revert)(\(.+\))?: .{1,72}$"

if ! echo "$commit_msg" | grep -qE "$pattern"; then
    echo "ERROR: Invalid commit message format"
    echo ""
    echo "Format: <type>(<scope>): <subject>"
    echo ""
    echo "Types: feat, fix, docs, style, refactor, perf, test, build, ci, chore"
    echo "Example: feat(archive): add ZIP support"
    exit 1
fi
```

### Commitizen

Используйте [Commitizen](https://commitizen-tools.github.io/commitizen/) для интерактивного создания коммитов:

```bash
npm install -g commitizen
npm install -g cz-conventional-changelog

# Создать коммит
git cz
```

---

## Генерация CHANGELOG

Conventional commits позволяют автоматически генерировать CHANGELOG:

```bash
# Используя standard-version
npm install -g standard-version
standard-version

# Или conventional-changelog
npm install -g conventional-changelog-cli
conventional-changelog -p angular -i CHANGELOG.md -s
```

---

## Связь с Semantic Versioning

| Commit Type | Version Bump | Пример |
|-------------|--------------|--------|
| `feat:` | MINOR (x.Y.z) | 2.0.0 → 2.1.0 |
| `fix:` | PATCH (x.y.Z) | 2.1.0 → 2.1.1 |
| `BREAKING CHANGE:` | MAJOR (X.y.z) | 2.1.1 → 3.0.0 |
| `docs:`, `style:`, etc. | No version bump | - |

---

## История коммитов ImageWriter

Примеры из реального проекта:

```
feat: Enhanced logging system with Unicode icons and structured format
docs: Updated CODING_STYLE_GUIDE.md with logging improvements
docs: Make CODING_STYLE_GUIDE.md universal for all Delphi projects
docs: Add Delphi 12 examples alongside Delphi 7
docs: Add Delphi 7 vs Delphi 12 project structure differences
docs: Update PROJECT_COMPLIANCE_REPORT.md to version 1.1
```

---

## Ссылки

- [Conventional Commits Specification](https://www.conventionalcommits.org/)
- [Semantic Versioning](https://semver.org/)
- [Keep a Changelog](https://keepachangelog.com/)
- [Angular Commit Guidelines](https://github.com/angular/angular/blob/main/CONTRIBUTING.md#commit)

---

**Дата создания:** 23 декабря 2025 г.  
**Версия:** 1.0  
**Проект:** ImageWriter
