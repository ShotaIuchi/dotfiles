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
# Prompt
# ------------------------------------------------------------------------------

autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats ' (%F{yellow}%b%f)'

setopt PROMPT_SUBST
PROMPT='%F{green}%n@%m%f:%F{blue}%~%f${vcs_info_msg_0_}%# '

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
