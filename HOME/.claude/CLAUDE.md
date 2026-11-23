# CLAUDE.md

## Project Overview

A workflow management system for Claude Code and humans to work while viewing the same state and deliverables.
Use by creating a symlink from `dotclaude/` folder to `~/.claude`.

## Principles (PRINCIPLES.md)

**Most Important**: `PRINCIPLES.md` is the fundamental principle that takes precedence over all rules.
Always read `PRINCIPLES.md` at the start of a session.

## Constitution (CONSTITUTION.md)

**Important**: Always refer to `CONSTITUTION.md` when adding or modifying files.

## Feature Development Workflow

Use the `dev` plugin from the personal marketplace [ShotaIuchi/ip](https://github.com/ShotaIuchi/ip) (the feature-dev plugin is no longer required).

- `/dev:spec <what to build>` — pin down the details with the user and write an approved `SPEC.md`
- `/dev:build [<SPEC.md>]` — implement from the spec, then check / match the spec / review / fix until clean (max 3 rounds). Asks only when it would deviate from the spec
