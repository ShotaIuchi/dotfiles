# ==============================================================================
# Zsh Configuration
# ==============================================================================

# ------------------------------------------------------------------------------
# Common Settings
# ------------------------------------------------------------------------------

if [[ -f "$HOME/.shell_common" ]]; then
    source "$HOME/.shell_common"
fi

# ------------------------------------------------------------------------------
# Zsh Options
# ------------------------------------------------------------------------------

# History
HISTSIZE=10000
SAVEHIST=20000
HISTFILE=~/.zsh_history
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS
setopt SHARE_HISTORY
setopt APPEND_HISTORY

# Directory
setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS

# Correction
setopt CORRECT

# ------------------------------------------------------------------------------
# Prompt (Starship)
# ------------------------------------------------------------------------------

eval "$(starship init zsh)"

# ------------------------------------------------------------------------------
# Pane Title (zsh hook)
# ------------------------------------------------------------------------------

# precmd: called before prompt (zsh-specific)
# Functions _pane_title_dir and _pane_set_title are defined in .shell_common
precmd() {
    if [[ -n "$TMUX" || -n "$ZELLIJ" ]]; then
        _pane_set_title "$(_pane_title_dir)"
    fi
}

# ------------------------------------------------------------------------------
# Completion
# ------------------------------------------------------------------------------

autoload -Uz compinit && compinit

# Case insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# Menu selection
zstyle ':completion:*' menu select

# ------------------------------------------------------------------------------
# Key Bindings
# ------------------------------------------------------------------------------

# Emacs-style key bindings
bindkey -e

# History search
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward

# ------------------------------------------------------------------------------
# fzf Key Bindings
# ------------------------------------------------------------------------------

if command -v fzf &>/dev/null; then
    eval "$(fzf --zsh)"
fi

# ------------------------------------------------------------------------------
# Go
# ------------------------------------------------------------------------------

export PATH="$HOME/go/bin:$PATH"

# ------------------------------------------------------------------------------
# wtp (Git Worktree Manager)
# ------------------------------------------------------------------------------

if command -v wtp &>/dev/null; then
    eval "$(wtp shell-init zsh)"
fi
