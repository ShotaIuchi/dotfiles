#!/bin/sh

set -eu

readonly TEST_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
readonly REPO_DIR="$(dirname -- "$TEST_DIR")"
readonly INSTALL_SCRIPT="$REPO_DIR/install.sh"
readonly UNINSTALL_SCRIPT="$REPO_DIR/uninstall.sh"
readonly README_FILE="$REPO_DIR/README.md"
readonly CODEX_README_FILE="$REPO_DIR/README.codex.md"
readonly SHELL_COMMON_FILE="$REPO_DIR/HOME/.shell_common"
readonly SHELL_README_FILE="$REPO_DIR/README.shell.md"

assert_contains() {
    file="$1"
    expected="$2"

    if ! grep -Fq -- "$expected" "$file"; then
        printf 'FAIL: %s does not contain: %s\n' "$file" "$expected" >&2
        exit 1
    fi
}

assert_contains "$INSTALL_SCRIPT" "readonly CODEX_INSTALL_URL='https://chatgpt.com/codex/install.sh'"
assert_contains "$INSTALL_SCRIPT" 'INSTALL_DECISIONS[codex]=0'
assert_contains "$INSTALL_SCRIPT" 'ALREADY_INSTALLED[codex]=0'
assert_contains "$INSTALL_SCRIPT" 'UPDATE_DECISIONS[codex]=0'
assert_contains "$INSTALL_SCRIPT" 'codex) echo "Codex CLI" ;;'
assert_contains "$INSTALL_SCRIPT" 'codex-cli|codex_cli) echo "codex" ;;'
assert_contains "$INSTALL_SCRIPT" 'if command_exists codex; then'
assert_contains "$INSTALL_SCRIPT" 'prompt_codex() {'
assert_contains "$INSTALL_SCRIPT" 'install_codex() {'
assert_contains "$INSTALL_SCRIPT" 'update_codex() {'
assert_contains "$INSTALL_SCRIPT" 'curl -fsSL "$CODEX_INSTALL_URL" | CODEX_NON_INTERACTIVE=true sh'
assert_contains "$INSTALL_SCRIPT" 'codex update'
assert_contains "$INSTALL_SCRIPT" 'fcodex エイリアスが設定されています'

assert_contains "$UNINSTALL_SCRIPT" 'UNINSTALL_DECISIONS[codex]=0'
assert_contains "$UNINSTALL_SCRIPT" 'IS_INSTALLED[codex]=0'
assert_contains "$UNINSTALL_SCRIPT" 'if command_exists codex; then'
assert_contains "$UNINSTALL_SCRIPT" 'prompt_uninstall_tool "codex" "Codex CLI" "OpenAI coding agent CLI"'
assert_contains "$UNINSTALL_SCRIPT" 'uninstall_codex() {'
assert_contains "$UNINSTALL_SCRIPT" 'npm uninstall -g @openai/codex'
assert_contains "$UNINSTALL_SCRIPT" 'brew uninstall --cask codex'
assert_contains "$UNINSTALL_SCRIPT" 'rm -rf "$standalone_root"'

assert_contains "$README_FILE" '| Codex CLI | OpenAI coding agent CLI |'
assert_contains "$CODEX_README_FILE" './install.sh'
assert_contains "$SHELL_COMMON_FILE" "alias fcodex='codex --dangerously-bypass-approvals-and-sandbox'"
assert_contains "$SHELL_README_FILE" '| `fcodex` | Codex CLI（承認・サンドボックス無効モード） |'

printf 'PASS: Codex install definitions are wired correctly.\n'
