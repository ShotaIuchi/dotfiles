#!/bin/sh
# Report Codex lifecycle events to the containing cmux workspace.

set -eu

# Inside tmux, pane environments inherit CMUX_* from whichever cmux tab started
# the tmux server, so they can point at the wrong tab (or none). Read the IDs
# from the most recently active tmux client attached to this pane's session,
# which is the process actually running in the cmux tab.
resolve_cmux_ids_from_tmux() {
    CMUX_WORKSPACE_ID=""
    CMUX_SURFACE_ID=""

    command -v tmux >/dev/null 2>&1 || return 0

    session="$(tmux display-message -p -t "$TMUX_PANE" '#{session_id}' 2>/dev/null || true)"
    [ -n "$session" ] || return 0

    client_pid="$(tmux list-clients -t "$session" -F '#{client_activity} #{client_pid}' 2>/dev/null \
        | sort -rn | awk 'NR == 1 { print $2 }')"
    [ -n "$client_pid" ] || return 0

    # `ps eww` appends the process environment to its command line.
    client_env="$(ps eww -o command= -p "$client_pid" 2>/dev/null | tr ' ' '\n' || true)"
    CMUX_WORKSPACE_ID="$(printf '%s\n' "$client_env" | sed -n 's/^CMUX_WORKSPACE_ID=//p' | head -n 1)"
    CMUX_SURFACE_ID="$(printf '%s\n' "$client_env" | sed -n 's/^CMUX_SURFACE_ID=//p' | head -n 1)"
}

if [ -n "${TMUX:-}" ] && [ -n "${TMUX_PANE:-}" ]; then
    resolve_cmux_ids_from_tmux
fi

if [ -z "${CMUX_WORKSPACE_ID:-}" ] || ! command -v cmux >/dev/null 2>&1; then
    exit 0
fi

# Scope the key per surface so multiple Codex sessions in one workspace don't collide.
readonly STATUS_KEY="codex${CMUX_SURFACE_ID:+-$CMUX_SURFACE_ID}"

set_status() {
    cmux set-status "$STATUS_KEY" "$1" --icon "$2" --color "$3" --workspace "$CMUX_WORKSPACE_ID" >/dev/null 2>&1 || true
}

notify() {
    if [ -n "${CMUX_SURFACE_ID:-}" ]; then
        cmux notify --title "Codex" --body "$1" --workspace "$CMUX_WORKSPACE_ID" --surface "$CMUX_SURFACE_ID" >/dev/null 2>&1 || true
    else
        cmux notify --title "Codex" --body "$1" --workspace "$CMUX_WORKSPACE_ID" >/dev/null 2>&1 || true
    fi
}

input="$(cat)"
event="$(printf '%s' "$input" | jq -r '.hook_event_name // empty' 2>/dev/null || true)"

case "$event" in
    SessionStart)
        set_status "Codex ready" "circle" "#7aa2f7"
        ;;
    UserPromptSubmit)
        set_status "Codex running" "sparkles" "#9ece6a"
        ;;
    PermissionRequest)
        set_status "Codex waiting" "hand.raised" "#e0af68"
        notify "Approval required"
        ;;
    Stop)
        set_status "Codex done" "checkmark.circle" "#9ece6a"
        notify "Turn completed"
        ;;
    Interrupt)
        set_status "Codex interrupted" "stop.circle" "#f7768e"
        notify "Turn interrupted"
        ;;
    SessionEnd)
        cmux clear-status "$STATUS_KEY" --workspace "$CMUX_WORKSPACE_ID" >/dev/null 2>&1 || true
        ;;
esac

exit 0
