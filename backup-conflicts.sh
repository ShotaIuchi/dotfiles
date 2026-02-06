#!/usr/bin/env bash
# ==============================================================================
# backup-conflicts.sh
# ==============================================================================
# amu add 実行前に、競合する既存ファイル（シンボリックリンクでない実ファイル）を
# .backup/ に退避するスクリプト
#
# Usage:
#   ./backup-conflicts.sh [--dry-run]

set -euo pipefail

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
SOURCE_DIR="${DOTFILES_DIR}/HOME"
TARGET_DIR="$HOME"
TIMESTAMP="$(date +%Y%m%d_%H%M%S)"
BACKUP_DIR="${DOTFILES_DIR}/.backup/${TIMESTAMP}"

DRY_RUN=0

# ------------------------------------------------------------------------------
# Colors
# ------------------------------------------------------------------------------

readonly GREEN=$'\033[0;32m'
readonly YELLOW=$'\033[0;33m'
readonly BLUE=$'\033[0;34m'
readonly DIM=$'\033[2m'
readonly BOLD=$'\033[1m'
readonly NC=$'\033[0m'

# ------------------------------------------------------------------------------
# Argument Parsing
# ------------------------------------------------------------------------------

for arg in "$@"; do
    case "$arg" in
        --dry-run)
            DRY_RUN=1
            ;;
        --help|-h)
            echo "Usage: $0 [--dry-run]"
            echo ""
            echo "Options:"
            echo "  --dry-run  Show conflicts without moving files"
            echo "  --help     Show this help"
            exit 0
            ;;
        *)
            echo "Unknown option: $arg" >&2
            exit 1
            ;;
    esac
done

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

if [[ ! -d "$SOURCE_DIR" ]]; then
    echo "ERROR: HOME directory not found: $SOURCE_DIR" >&2
    exit 1
fi

conflicts=()

while IFS= read -r -d '' source_file; do
    # Get relative path from SOURCE_DIR
    rel_path="${source_file#"${SOURCE_DIR}/"}"
    target_file="${TARGET_DIR}/${rel_path}"

    # Skip if target doesn't exist
    [[ ! -e "$target_file" ]] && continue

    # Skip if target is a symlink (already managed by amu)
    [[ -L "$target_file" ]] && continue

    # Target is a real file — conflict found
    conflicts+=("$rel_path")
done < <(find "$SOURCE_DIR" -type f -print0)

# ------------------------------------------------------------------------------
# Results
# ------------------------------------------------------------------------------

if [[ ${#conflicts[@]} -eq 0 ]]; then
    printf " ${GREEN}${BOLD}No conflicts found.${NC}\n"
    exit 0
fi

printf " ${YELLOW}${BOLD}Found %d conflict(s):${NC}\n" "${#conflicts[@]}"
echo

for rel_path in "${conflicts[@]}"; do
    printf "   ${YELLOW}→${NC} ~/%s\n" "$rel_path"
done

echo

if [[ $DRY_RUN -eq 1 ]]; then
    printf " ${DIM}(dry-run: no files were moved)${NC}\n"
    exit 0
fi

# ------------------------------------------------------------------------------
# Backup
# ------------------------------------------------------------------------------

printf " ${BLUE}Backing up to:${NC} %s\n" ".backup/${TIMESTAMP}/"
echo

for rel_path in "${conflicts[@]}"; do
    target_file="${TARGET_DIR}/${rel_path}"
    backup_file="${BACKUP_DIR}/${rel_path}"

    # Create parent directory in backup
    mkdir -p "$(dirname "$backup_file")"

    # Move file to backup
    mv "$target_file" "$backup_file"
    printf "   ${GREEN}✓${NC} ~/%s → .backup/%s/%s\n" "$rel_path" "$TIMESTAMP" "$rel_path"
done

echo
printf " ${GREEN}${BOLD}Backup complete.${NC} %d file(s) moved.\n" "${#conflicts[@]}"
