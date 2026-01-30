# ==============================================================================
# Bash Profile
# ==============================================================================
# ログインシェル用。.bashrcを読み込む。

# Homebrew bashがあればそちらで再実行
if [[ -x /opt/homebrew/bin/bash ]] && [[ $BASH != /opt/homebrew/bin/bash ]]; then
    exec /opt/homebrew/bin/bash --login
fi

if [[ -f "$HOME/.bashrc" ]]; then
    source "$HOME/.bashrc"
fi
