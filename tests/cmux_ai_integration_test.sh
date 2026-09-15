#!/bin/sh

set -eu

readonly TEST_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
readonly REPO_DIR="$(dirname -- "$TEST_DIR")"
readonly CODEX_HOOKS_FILE="$REPO_DIR/HOME/.codex/hooks.json"
readonly CMUX_HOOK_FILE="$REPO_DIR/HOME/.codex/hooks/cmux-state.sh"

assert_contains() {
    file="$1"
    expected="$2"

    if ! grep -Fq -- "$expected" "$file"; then
        printf 'FAIL: %s does not contain: %s\n' "$file" "$expected" >&2
        exit 1
    fi
}

assert_contains "$CODEX_HOOKS_FILE" '"SessionStart"'
assert_contains "$CODEX_HOOKS_FILE" '"UserPromptSubmit"'
assert_contains "$CODEX_HOOKS_FILE" '"PermissionRequest"'
assert_contains "$CODEX_HOOKS_FILE" '"Stop"'
assert_contains "$CODEX_HOOKS_FILE" '"SessionEnd"'
assert_contains "$CODEX_HOOKS_FILE" '~/.codex/hooks/cmux-state.sh'

assert_contains "$CMUX_HOOK_FILE" 'cmux set-status'
assert_contains "$CMUX_HOOK_FILE" 'cmux notify'
assert_contains "$CMUX_HOOK_FILE" 'CMUX_WORKSPACE_ID'

printf 'PASS: cmux AI integration is wired correctly.\n'
