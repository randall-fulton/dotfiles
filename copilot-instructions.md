Keep it brief. Don't fucking yap.

## Task Completion

Before telling the user a task is done, always run the tests for every package you modified and confirm they pass, and run the linter and fix any issues it reports. Do not declare completion without evidence of passing tests and a clean lint.

# System-wide Copilot Instructions

- All repositories use `jj` (Jujutsu) for version control instead of `git`. Use `jj` commands for all VCS operations (e.g. `jj log`, `jj diff`, `jj describe`, `jj new`, `jj squash`, etc.).

## Token Optimization (RTK)
This environment has a CLI proxy called `rtk` installed, which drastically reduces token consumption for standard commands.

Whenever you need to use the `bash` tool to gather context, you MUST prefix the commands with `rtk`. Apply the following mappings:

| Raw Command | Rewritten To |
| --- | --- |
| `git status/diff/log/add/commit/push/pull` | `rtk git ...` |
| `gh pr/issue/run` | `rtk gh ...` |
| `cargo test/build/clippy` | `rtk cargo ...` |
| `cat/head/tail <file>` | `rtk read <file>` |
| `rg/grep <pattern>` | `rtk grep <pattern>` |
| `ls` | `rtk ls` |
| `vitest/jest` | `rtk vitest run` |
| `tsc` | `rtk tsc` |
| `eslint/biome` | `rtk lint` |
| `prettier` | `rtk prettier` |
| `playwright` | `rtk playwright` |
| `prisma` | `rtk prisma` |
| `ruff check/format` | `rtk ruff ...` |
| `pytest` | `rtk pytest` |
| `pip list/install` | `rtk pip ...` |
| `go test/build/vet` | `rtk go ...` |
| `golangci-lint` | `rtk golangci-lint` |
| `docker ps/images/logs` | `rtk docker ...` |
| `kubectl get/logs` | `rtk kubectl ...` |
| `curl` | `rtk curl` |
| `pnpm list/outdated` | `rtk pnpm ...` |

## SQL

Always create PostgreSQL indexes concurrently.
