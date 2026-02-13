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
HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.zsh_history
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_FIND_NO_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

# Directory
setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS

# Misc
setopt NO_BEEP
setopt EXTENDED_GLOB
setopt INTERACTIVE_COMMENTS
unsetopt CORRECT

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
# Emacs vterm integration
# ------------------------------------------------------------------------------

if [[ "$INSIDE_EMACS" = *vterm* ]]; then
    vterm_printf() { printf "\e]%s\e\\" "$1" }
    _vterm_sync_dir() { vterm_printf "51;A$(whoami)@$(hostname):$(pwd)" }
    autoload -Uz add-zsh-hook
    add-zsh-hook precmd _vterm_sync_dir
fi

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

# ------------------------------------------------------------------------------
# zoxide (smart cd)
# ------------------------------------------------------------------------------

if command -v zoxide &>/dev/null; then
    eval "$(zoxide init zsh --cmd cd)"
fi

# ------------------------------------------------------------------------------
# direnv (per-directory environment variables)
# ------------------------------------------------------------------------------

if command -v direnv &>/dev/null; then
    eval "$(direnv hook zsh)"
fi

# ------------------------------------------------------------------------------
# Plugins (load last to ensure correct behavior)
# ------------------------------------------------------------------------------

# zsh-autosuggestions
if [[ -f "$(brew --prefix 2>/dev/null)/share/zsh-autosuggestions/zsh-autosuggestions.zsh" ]]; then
    source "$(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
elif [[ -f /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh ]]; then
    source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
elif [[ -f /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]]; then
    source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
fi

# zsh-syntax-highlighting (must be loaded last)
if [[ -f "$(brew --prefix 2>/dev/null)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]]; then
    source "$(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
elif [[ -f /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
    source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
elif [[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi
