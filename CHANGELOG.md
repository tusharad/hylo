# Changelog for `hylo`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- `Hylo.AI.Context.Exporter`: pure, deterministic `AGENTS.md` and `llms.txt` renderers over an `[AnyTableSchema]` registry (`exportAgentsMarkdown`, `exportLlmsTxt`) with deterministic token estimation and budget enforcement (`estimateTokens`, `fitTokenBudget`).
- `hylo ai-context` CLI sub-command with `--dir`, `--tables`, and `--budget` flags writing `AGENTS.md` and `llms.txt` and printing a token summary.
- `TableSchema` gains a backward-compatible `tableDescription :: Maybe Text` field (default `Nothing`) and the `withTableDescription` combinator.

## 0.1.0.0 - YYYY-MM-DD
