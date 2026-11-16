# Changelog for `hylo`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- `Hylo.Migrate.Runner.Apply`: transactional DDL execution runner with migration history (User Story 20) — `MigrationRecord`, `_hylo_migrations` history table provisioning (`ensureMigrationsTable`), pending-file scanning in timestamp order (`listPending`), transactional `applyMigration` (BEGIN → DDL → history insert → COMMIT, full ROLLBACK on any failure), and `rollbackLastMigration` (execute recorded `down_sql` + delete record). The runner core is abstracted over a `MigrationBackend` with `postgresBackend` (Hasql) and `sqliteBackend` (in-memory) instances so the execution laws are verifiable against in-memory SQLite catalogs.
- `hylo migrate run` (`--conn`, `--dir`, `--force`), `hylo migrate rollback`, and `hylo migrate status` CLI sub-commands with per-migration `[✓] <file> applied (N ms)` status lines and a `Nothing to apply.` fast path.
- Linter pre-flight before every applied migration (`preflightLintConfig`), aborting with `MigrationLintError` on zero-downtime hazards unless `--force` is passed.
- `Hylo.Core.Codec`: custom ADT / PostgreSQL `ENUM` codecs (User Story 19) — `HyloCodec` alias, `enumCodec :: Eq a => [(Text, a)] -> HyloCodec a` (framed UTF-8 label wire format with descriptive unknown-label failures), `boundedEnumCodec` / `boundedEnumLabels` deriving snake_case labels from `[minBound .. maxBound]`, `camelToSnake`, and the `enumColDef` column-descriptor smart constructor.
- `Hylo.Core.Table`: `enumCol` and `sumTypeCol` combinators defining native PostgreSQL `ENUM` columns backed by label-table codecs.
- `Hylo.Core.Context`: `ColDef` gains backward-compatible `colIsEnum :: Bool` and `colEnumLabels :: [Text]` fields (defaults `False` / `[]`).
- `Hylo.Migrate.AST`: `EnumType !Text ![Text]` column type and `CreateEnumTypeStmt` / `AlterEnumTypeAddValueStmt` / `DropEnumTypeStmt` reversible DDL statements with PostgreSQL rendering.
- `Hylo.Migrate.Diff`: enum-aware catalog diffing — `CREATE TYPE ... AS ENUM` emitted before the owning `CREATE TABLE`, `ALTER TYPE ... ADD VALUE` for additive label evolution, `catalogEnums` registry maintenance, and `diffEnumLabelHazards` flagging label removals as `UnsafeEnumLabelRemoved` linter hazards (PostgreSQL cannot drop enum values).
- `Hylo.Core.TypeError` / `Hylo.Core.Expr`: `[Hylo Error E017] Unknown enum label` compile-time diagnostic (`AssertEnumLabel`, `CheckEnumLabel`, `LabelIn`, `HasEnumLabels`) with the `enumLit @"label"` validated enum literal combinator.
- `Hylo.PG.Codec.Binary`: `pgEnumCodec` binary wire round-trips for enum values via the text-protocol OID fallback.
- `Hylo.AI.Context.Exporter`: pure, deterministic `AGENTS.md` and `llms.txt` renderers over an `[AnyTableSchema]` registry (`exportAgentsMarkdown`, `exportLlmsTxt`) with deterministic token estimation and budget enforcement (`estimateTokens`, `fitTokenBudget`).
- `hylo ai-context` CLI sub-command with `--dir`, `--tables`, and `--budget` flags writing `AGENTS.md` and `llms.txt` and printing a token summary.
- `TableSchema` gains a backward-compatible `tableDescription :: Maybe Text` field (default `Nothing`) and the `withTableDescription` combinator.

## 0.1.0.0 - YYYY-MM-DD
