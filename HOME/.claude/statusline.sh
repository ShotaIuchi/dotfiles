#!/bin/bash
# Claude Code ステータスライン
# stdin で渡されるセッション情報JSONから1行を組み立てる:
#   [セッション名] モデル(+effort/think/fast) │ コンテキスト残% │
#   レート制限(5h→リセット時刻 / 7d) │ +追加行 -削除行 │ コスト │ Vimモード
#
# ブランチ・リポジトリ・worktree・PR状態は tmux 側で表示するため扱わない
# （~/.config/tmux/status-git-extra.sh）

input=$(cat)

# 空フィールドを保持するため、jqで1行1フィールドに展開して読み取る
vals=()
while IFS= read -r line; do vals+=("$line"); done < <(printf '%s' "$input" | jq -r '[
    (.session_name // ""),
    (.model.display_name // "?"),
    (.effort.level // ""),
    (if .thinking.enabled then "1" else "" end),
    (if .fast_mode then "1" else "" end),
    ((.context_window.remaining_percentage // "") | if . == "" then "" else round | tostring end),
    ((.rate_limits.five_hour.used_percentage // "") | if . == "" then "" else round | tostring end),
    (.rate_limits.five_hour.resets_at // ""),
    ((.rate_limits.seven_day.used_percentage // "") | if . == "" then "" else round | tostring end),
    (.cost.total_cost_usd // ""),
    (.cost.total_lines_added // 0),
    (.cost.total_lines_removed // 0),
    (.vim.mode // ""),
    (.session_id // "")
] | .[] | tostring')

session="${vals[0]}"   model="${vals[1]}"    effort="${vals[2]}"
thinking="${vals[3]}"  fast="${vals[4]}"     ctx_rem="${vals[5]}"
rl5_used="${vals[6]}"  rl5_reset="${vals[7]}" rl7_used="${vals[8]}"
cost="${vals[9]}"      added="${vals[10]}"   removed="${vals[11]}"
vim_mode="${vals[12]}" session_id="${vals[13]}"

# Tokyo Night
BLUE=$'\e[38;2;122;162;247m'
FG=$'\e[38;2;192;202;245m'
GREEN=$'\e[38;2;158;206;106m'
YELLOW=$'\e[38;2;224;175;104m'
RED=$'\e[38;2;247;118;142m'
PURPLE=$'\e[38;2;187;154;247m'
GRAY=$'\e[38;2;120;124;153m'
DIM=$'\e[38;2;86;95;137m'
BOLD=$'\e[1m'
RESET=$'\e[0m'

# 残量・使用率に応じた色（残: 多いほど緑 / 使用: 多いほど赤）
color_by_remaining() { [ "$1" -ge 50 ] && echo "$GREEN" || { [ "$1" -ge 20 ] && echo "$YELLOW" || echo "$RED"; }; }
color_by_used()      { [ "$1" -lt 50 ] && echo "$GRAY"  || { [ "$1" -lt 80 ] && echo "$YELLOW" || echo "$RED"; }; }

# 秒数を "42s" / "3m05s" / "1h02m" に整形
fmt_dur() {
    local s=$1
    if [ "$s" -ge 3600 ]; then printf '%dh%02dm' $((s / 3600)) $((s % 3600 / 60))
    elif [ "$s" -ge 60 ]; then printf '%dm%02ds' $((s / 60)) $((s % 60))
    else printf '%ds' "$s"; fi
}

segs=()

# 実行状態（hooks が書く ~/.claude/state/<session_id> を参照）
# statusLine.refreshInterval により定期再実行されるため経過時間は動き続ける。
# 「▶」のまま経過時間だけが異常に伸びていればハングの疑い。
state_file="$HOME/.claude/state/${session_id}"
if [ -n "$session_id" ] && [ -f "$state_file" ]; then
    IFS='|' read -r st st_tool st_ts _ < "$state_file"
    if [ -n "$st_ts" ]; then
        st_sec=$(($(date +%s) - st_ts))
        st_age=$(fmt_dur "$st_sec")
        case "$st" in
            running) segs+=("${GREEN}▶ ${st_tool:-応答中} ${st_age}${RESET}") ;;
            waiting) segs+=("${RED}⏸ 入力待ち ${st_age}${RESET}") ;;
            idle)
                # tmux側(status-claude-state.sh)と同じ基準: 完了直後(5分以内)は「完了」
                if [ "$st_sec" -lt 300 ]; then
                    segs+=("${GREEN}■ 完了 ${st_age}${RESET}")
                else
                    segs+=("${GRAY}■ 待機${RESET}")
                fi
                ;;
        esac
    fi
fi

# セッション名（/rename や --name 指定時のみ）
[ -n "$session" ] && segs+=("${BOLD}${BLUE}${session}${RESET}")

# モデル + 推論モード（effort / think / fast）
mode=""
[ -n "$effort" ] && mode="$effort"
[ -n "$thinking" ] && mode="${mode:+${mode}·}think"
[ -n "$fast" ] && mode="${mode:+${mode}·}fast"
segs+=("${BOLD}${FG}${model}${RESET}${mode:+ ${DIM}${mode}${RESET}}")

# コンテキスト残%
[ -n "$ctx_rem" ] && segs+=("$(color_by_remaining "$ctx_rem")ctx ${ctx_rem}%${RESET}")

# レート制限（Pro/Maxのみ。5hはリセット時刻付き）
if [ -n "$rl5_used" ]; then
    rl5="$(color_by_used "$rl5_used")5h ${rl5_used}%"
    [ -n "$rl5_reset" ] && rl5+="${DIM}→$(date -r "$rl5_reset" +%H:%M 2>/dev/null)"
    rl5+="${RESET}"
    rl7=""
    [ -n "$rl7_used" ] && rl7=" ${DIM}·${RESET} $(color_by_used "$rl7_used")7d ${rl7_used}%${RESET}"
    segs+=("${rl5}${rl7}")
fi

# セッション中の変更行数
segs+=("${GREEN}+${added}${RESET} ${RED}-${removed}${RESET}")

# コスト
[ -n "$cost" ] && segs+=("${DIM}$(printf '$%.2f' "$cost")${RESET}")

# Vimモード（vim モード有効時のみ）
if [ -n "$vim_mode" ]; then
    case "$vim_mode" in
        INSERT) vcolor="$GREEN" ;;
        VISUAL*) vcolor="$PURPLE" ;;
        *) vcolor="$BLUE" ;;
    esac
    segs+=("${vcolor}${vim_mode}${RESET}")
fi

# セパレータで連結
out=""
for seg in "${segs[@]}"; do
    out+="${out:+ ${DIM}│${RESET} }${seg}"
done
printf '%s' "$out"
