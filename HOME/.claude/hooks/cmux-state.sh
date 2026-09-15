#!/bin/bash
# Claude Code の hooks から呼ばれ、cmux のタブに実行状態と通知を表示する
# cmux 同梱の claude ラッパーと同じ `cmux claude-hook` を使う
# （~/.local/bin の claude が PATH で先に見つかりラッパーは経由しないため自前で呼ぶ）

# tmux 内のペインは tmux サーバーを起動したタブの CMUX_* を引き継ぐため、
# ペインの session に attach している tmux クライアント（最後に操作されたもの）の
# 環境変数から実際のタブを特定する
resolve_cmux_ids_from_tmux() {
    CMUX_WORKSPACE_ID=""
    CMUX_SURFACE_ID=""

    command -v tmux >/dev/null 2>&1 || return 0

    local session client_pid client_env
    session=$(tmux display-message -p -t "$TMUX_PANE" '#{session_id}' 2>/dev/null)
    [ -n "$session" ] || return 0

    client_pid=$(tmux list-clients -t "$session" -F '#{client_activity} #{client_pid}' 2>/dev/null \
        | sort -rn | awk 'NR == 1 { print $2 }')
    [ -n "$client_pid" ] || return 0

    # `ps eww` はコマンドラインの後ろにプロセスの環境変数を付けて出力する
    client_env=$(ps eww -o command= -p "$client_pid" 2>/dev/null | tr ' ' '\n')
    CMUX_WORKSPACE_ID=$(printf '%s\n' "$client_env" | sed -n 's/^CMUX_WORKSPACE_ID=//p' | head -n 1)
    CMUX_SURFACE_ID=$(printf '%s\n' "$client_env" | sed -n 's/^CMUX_SURFACE_ID=//p' | head -n 1)
}

input=$(cat)
event=$(printf '%s' "$input" | jq -r '.hook_event_name // empty' 2>/dev/null)

case "$event" in
    SessionStart) subcommand=session-start ;;
    Stop)         subcommand=stop ;;
    Notification) subcommand=notification ;;
    *)            exit 0 ;;
esac

if [ -n "${TMUX:-}" ] && [ -n "${TMUX_PANE:-}" ]; then
    resolve_cmux_ids_from_tmux
fi

if [ -z "${CMUX_WORKSPACE_ID:-}" ] || [ -z "${CMUX_SURFACE_ID:-}" ] || ! command -v cmux >/dev/null 2>&1; then
    exit 0
fi

printf '%s' "$input" \
    | cmux claude-hook "$subcommand" --workspace "$CMUX_WORKSPACE_ID" --surface "$CMUX_SURFACE_ID" >/dev/null 2>&1
exit 0
