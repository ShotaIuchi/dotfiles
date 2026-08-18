#!/usr/bin/env bash
# tmux ステータスバー用: linked worktree名 と PR状態 を出力する
# 使い方: status-git-extra.sh <pane_current_path>
# 出力例: "#[fg=#bb9af7]wt:feature-x #[fg=#9ece6a]PR#123 approved "
#
# PR情報は gh pr view のネットワーク呼び出しを伴うため、
# リポジトリ×ブランチ単位で TTL 付きキャッシュする（PRなしの結果もキャッシュ）
set -u

CACHE_TTL=120

dir="${1:-}"
[ -n "$dir" ] && [ -d "$dir" ] || exit 0

git_dir=$(git -C "$dir" rev-parse --absolute-git-dir 2>/dev/null) || exit 0
common_dir=$(git -C "$dir" rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
toplevel=$(git -C "$dir" rev-parse --show-toplevel 2>/dev/null)

out=""

# --- linked worktree（メインの作業ツリーでは git-dir と git-common-dir が一致する）---
if [ -n "$common_dir" ] && [ "$git_dir" != "$common_dir" ]; then
    out+="#[fg=#bb9af7]wt:$(basename "$toplevel") "
fi

# --- PR状態 ---
branch=$(git -C "$dir" branch --show-current 2>/dev/null)
if [ -n "$branch" ] && command -v gh >/dev/null 2>&1; then
    cache_key=$(printf '%s' "${toplevel}:${branch}" | shasum | cut -c1-16)
    cache="${TMPDIR:-/tmp}/tmux-pr-status-${cache_key}"

    now=$(date +%s)
    if [ -f "$cache" ]; then
        mtime=$(stat -f %m "$cache" 2>/dev/null || stat -c %Y "$cache" 2>/dev/null)
        age=$((now - mtime))
    else
        age=$((CACHE_TTL + 1))
    fi

    if [ "$age" -gt "$CACHE_TTL" ]; then
        (cd "$toplevel" && gh pr view --json number,isDraft,reviewDecision --jq '
            if .isDraft then "\(.number) draft"
            elif .reviewDecision == "APPROVED" then "\(.number) approved"
            elif .reviewDecision == "CHANGES_REQUESTED" then "\(.number) changes"
            else "\(.number) open"
            end' 2>/dev/null) > "$cache"
    fi

    pr=$(cat "$cache" 2>/dev/null)
    if [ -n "$pr" ]; then
        num=${pr%% *}
        state=${pr#* }
        case "$state" in
            approved) color="#9ece6a" ;;  # green
            changes)  color="#f7768e" ;;  # red
            draft)    color="#565f89" ;;  # gray
            *)        color="#e0af68" ;;  # yellow (open/review待ち)
        esac
        out+="#[fg=${color}]PR#${num} ${state} "
    fi
fi

printf '%s' "$out"
