#!/bin/bash
# Claude Code の hooks から呼ばれ、セッションの実行状態を状態ファイルに記録する
# 状態ファイル: ~/.claude/state/<session_id>
# 形式: <state>|<tool>|<epoch>|<tmux_pane>  (state: running / waiting / idle)
# 読み手: ~/.claude/statusline.sh（Claude Code側）,
#         ~/.config/tmux/status-claude-state.sh（tmux側ハング検知）
dir="$HOME/.claude/state"
mkdir -p "$dir"

# 空フィールドを保持するため1行1フィールドで読む
vals=()
while IFS= read -r line; do vals+=("$line"); done < <(jq -r '
    [(.session_id // ""), (.hook_event_name // ""), (.tool_name // "")] | .[]')
sid="${vals[0]}" event="${vals[1]}" tool="${vals[2]}"
[ -n "$sid" ] || exit 0

f="$dir/$sid"
now=$(date +%s)
pane="${TMUX_PANE:-}"

write() { printf '%s|%s|%s|%s\n' "$1" "$2" "$now" "$pane" > "$f"; }

case "$event" in
    UserPromptSubmit)               write running "" ;;
    PreToolUse)                     write running "$tool" ;;
    PostToolUse|PostToolUseFailure) write running "" ;;
    PermissionRequest|Notification) write waiting "" ;;
    Stop|StopFailure)               write idle "" ;;
    SessionEnd)                     rm -f "$f" ;;
esac
exit 0
