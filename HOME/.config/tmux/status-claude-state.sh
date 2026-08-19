#!/usr/bin/env bash
# tmux ステータスバー用: このペインで動く Claude Code の実行状態を表示する
# 使い方: status-claude-state.sh <pane_id>
#
# 状態ファイル(~/.claude/state/<session_id>)は Claude Code の hooks
# (~/.claude/hooks/state-track.sh) が更新する。
# tmux は status-interval ごとに無条件で再描画するため、Claude Code 本体が
# 完全にハングしても経過時間の表示は伸び続ける（＝ハング検知に使える）。
# 経過時間による色: 緑 <2分 / 黄 <10分 / 赤 >=10分
set -u

pane="${1:-}"
dir="$HOME/.claude/state"
[ -n "$pane" ] && [ -d "$dir" ] || exit 0

now=$(date +%s)
best_ts=0
best_st=""
best_tool=""

for f in "$dir"/*; do
    [ -f "$f" ] || continue
    IFS='|' read -r st tool ts fpane < "$f" || true
    case "$ts" in ''|*[!0-9]*) continue ;; esac
    # 異常終了したセッションの残骸(24h超)を掃除
    if [ $((now - ts)) -gt 86400 ]; then rm -f "$f"; continue; fi
    [ "$fpane" = "$pane" ] || continue
    # 同一ペインで複数セッション履歴がある場合は最新を採用
    if [ "$ts" -gt "$best_ts" ]; then
        best_ts=$ts; best_st=$st; best_tool=$tool
    fi
done

[ -n "$best_st" ] || exit 0
age=$((now - best_ts))

fmt_dur() {
    local s=$1
    if [ "$s" -ge 3600 ]; then printf '%dh%02dm' $((s / 3600)) $((s % 3600 / 60))
    elif [ "$s" -ge 60 ]; then printf '%dm%02ds' $((s / 60)) $((s % 60))
    else printf '%ds' "$s"; fi
}

case "$best_st" in
    running)
        color="#9ece6a"
        [ "$age" -ge 120 ] && color="#e0af68"
        [ "$age" -ge 600 ] && color="#f7768e"
        printf '#[fg=%s]▶%s %s ' "$color" "${best_tool:+ ${best_tool}}" "$(fmt_dur "$age")"
        ;;
    waiting)
        printf '#[fg=#e0af68]⏸ 入力待ち %s ' "$(fmt_dur "$age")"
        ;;
    idle)
        printf '#[fg=#565f89]■ '
        ;;
esac
