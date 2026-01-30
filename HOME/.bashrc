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
HISTSIZE=10000
HISTFILESIZE=20000
HISTCONTROL=ignoreboth:erasedups
shopt -s histappend

# Check window size after each command
shopt -s checkwinsize

# Autocorrect typos in cd
shopt -s cdspell

# ------------------------------------------------------------------------------
# Prompt
# ------------------------------------------------------------------------------

# Git branch in prompt
__git_branch() {
    git branch 2>/dev/null | sed -n 's/^\* //p'
}

PS1='\[\e[32m\]\u@\h\[\e[0m\]:\[\e[34m\]\w\[\e[0m\]$(__git_branch && echo " (\[\e[33m\]$(__git_branch)\[\e[0m\])")\$ '

# ------------------------------------------------------------------------------
# Completion
# ------------------------------------------------------------------------------

if [[ -f /opt/homebrew/etc/profile.d/bash_completion.sh ]]; then
    source /opt/homebrew/etc/profile.d/bash_completion.sh
elif [[ -f /etc/bash_completion ]]; then
    source /etc/bash_completion
fi
