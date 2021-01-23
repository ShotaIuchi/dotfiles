alias ll="ls -al"

## PS1
## see) https://github.com/olivierverdier/zsh-git-prompt
## see) https://formulae.brew.sh/formula/zsh-git-prompt#default
source "/opt/homebrew/opt/zsh-git-prompt/zshrc.sh"
PROMPT='%B%m.%n%\:%~%b$(git_super_status)%# '

### HOMEBREW
# $ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# see) https://brew.sh/index_ja

# homebrew
export PATH="/opt/homebrew/bin/:$PATH"

# git
export PATH="/opt/homebrew/opt/git/bin:$PATH"

# coreutils
export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"
export MANPATH="/opt/homebrew/opt/coreutils/libexec/gnuman:$MANPATH"

# ed
export PATH="/opt/homebrew/opt/ed/libexec/gnubin:$PATH"
export MANPATH="/opt/homebrew/opt/ed/libexec/gnuman:$MANPATH"

# findutils
export PATH="/opt/homebrew/opt/findutils/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/binutils/lib"
export CPPFLAGS="-I/opt/homebrew/opt/binutils/include"

# sed
export PATH="/opt/homebrew/opt/gnu-sed/libexec/gnubin:$PATH"
export MANPATH="/opt/homebrew/opt/gnu-sed/libexec/gnuman:$MANPATH"

# tar
export PATH="/opt/homebrew/opt/gnu-tar/libexec/gnubin:$PATH"
export MANPATH="/opt/homebrew/opt/gnu-tar/libexec/gnuman:$MANPATH"

# grep
export PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
export MANPATH="/opt/homebrew/opt/grep/libexec/gnuman:$MANPATH"

# ssh
export PATH="/opt/homebrew/opt/openssl@1.1/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/openssl@1.1/lib"
export CPPFLAGS="-I/opt/homebrew/opt/openssl@1.1/include"
