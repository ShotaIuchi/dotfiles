# ==============================================================================
# Bash Configuration
# ==============================================================================

# Non-interactive shell: exit early
[[ $- != *i* ]] && return

# ------------------------------------------------------------------------------
# Common Settings
# ------------------------------------------------------------------------------

if [[ -f "$HOME/.shell_common" ]]; then
    source "$HOME/.shell_common"
fi

# ------------------------------------------------------------------------------
# Bash Options
# ------------------------------------------------------------------------------

# History
HISTSIZE=100000
HISTFILESIZE=100000
HISTCONTROL=ignoreboth:erasedups
shopt -s histappend

# Check window size after each command
shopt -s checkwinsize

# Autocorrect typos in cd
shopt -s cdspell

# ------------------------------------------------------------------------------
# Pane Title (bash hook)
# ------------------------------------------------------------------------------

# PROMPT_COMMAND: called before prompt (bash-specific)
# Functions _pane_title_dir and _pane_set_title are defined in .shell_common
# Set before starship init so starship can prepend its own command
PROMPT_COMMAND='
if [[ -n "$TMUX" || -n "$ZELLIJ" ]]; then
    _pane_set_title "$(_pane_title_dir)"
fi
'

# ------------------------------------------------------------------------------
# Prompt (Starship)
# ------------------------------------------------------------------------------

eval "$(starship init bash)"

# ------------------------------------------------------------------------------
# Completion
# ------------------------------------------------------------------------------

if [[ -f /opt/homebrew/etc/profile.d/bash_completion.sh ]]; then
    source /opt/homebrew/etc/profile.d/bash_completion.sh
elif [[ -f /etc/bash_completion ]]; then
    source /etc/bash_completion
fi

# ------------------------------------------------------------------------------
# fzf Key Bindings
# ------------------------------------------------------------------------------

if command -v fzf &>/dev/null; then
    eval "$(fzf --bash)"
fi

# ------------------------------------------------------------------------------
# zoxide (smart cd)
# ------------------------------------------------------------------------------

if command -v zoxide &>/dev/null; then
    eval "$(zoxide init bash --cmd cd)"
fi

# ------------------------------------------------------------------------------
# direnv (per-directory environment variables)
# ------------------------------------------------------------------------------

if command -v direnv &>/dev/null; then
    eval "$(direnv hook bash)"
fi
