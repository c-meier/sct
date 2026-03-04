# Changelog for student-correction-transformer

## Unreleased changes

## 0.4.0.0

- Add `--formatter-space` option to strip one optional space after comment prefix when uncommenting
- Add `set` file-level directive support (e.g. `//!set formatter-space`)
- Allow discarded text after command prefix (e.g. `##![ some comment` or `//!- description`)
- Support whitespace (space and tab) before comment prefix in commented student/corrected code
- Add language spec for YAML (.yaml, .yml)
- Add language spec for SQL (.sql) and gitignore (.gitignore, .sctignore)
