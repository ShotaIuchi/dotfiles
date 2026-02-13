# dotfiles

個人環境設定ファイルの管理リポジトリ。

## 構成

```
dotfiles/
├── HOME/           # ホームディレクトリに配置するファイル
│   ├── .bashrc
│   ├── .gitconfig
│   └── .config/    # アプリ設定（neovim, zellij, ghostty等）
├── install.sh      # 対話式インストールスクリプト
├── uninstall.sh    # 対話式アンインストールスクリプト
└── .gitignore
```

## クイックスタート

```bash
git clone https://github.com/yourname/dotfiles ~/dot/dotfiles
cd ~/dot/dotfiles
./install.sh
```

対話式でツールのインストールとdotfilesの適用を行います。

## インストール

### install.sh（推奨）

対話式インストールスクリプト。各ツールのインストール可否を選択できます。

```bash
./install.sh
```

**対応OS:**
- macOS（Homebrew）
- Linux（apt / dnf / pacman）
- Windows（WSL, Git Bash）

**インストール可能なツール:**

| ツール | 説明 |
|--------|------|
| Homebrew | パッケージマネージャー（macOS） |
| Neovim | モダンなVimエディタ |
| Zellij | ターミナルマルチプレクサ |
| Ghostty | 高速ターミナルエミュレータ（macOS） |
| amu | シンボリックリンク管理 |
| gh | GitHub CLI |
| bash-completion | タブ補完強化 |
| Claude Code | Anthropic AI CLI |

### 手動セットアップ

[amu](https://github.com/ShotaIuchi/amu) を使用してシンボリックリンクを作成する。

#### amuのインストール

```bash
# Homebrew
brew install ShotaIuchi/tap/amu

# またはシェルスクリプト
curl -fsSL https://raw.githubusercontent.com/ShotaIuchi/amu/main/install.sh | sh
```

#### dotfilesの適用

```bash
# HOMEディレクトリの内容を~/にリンク
amu add ~/dot/dotfiles/HOME ~/

# 状態確認
amu status

# プレビュー（実際には適用しない）
amu add ~/dot/dotfiles/HOME ~/ --dry-run
```

## アンインストール

### uninstall.sh

対話式でdotfilesのリンク解除とツールの削除を行います。

```bash
./uninstall.sh
```

**機能:**
- dotfilesのシンボリックリンク解除（`amu rm`）
- 各ツールの個別アンインストール

### 手動でのリンク解除

```bash
# amuでリンク解除
amu rm ~/dot/dotfiles/HOME ~/

# 設定更新後の再適用
amu sync
```

## 各ツールの設定・キーバインド

| ツール | ドキュメント |
|---|---|
| Emacs | [README.emacs.md](README.emacs.md) |
| Neovim | [README.neovim.md](README.neovim.md) |
| Git | [README.git.md](README.git.md) |
| Ghostty | [README.ghostty.md](README.ghostty.md) |
| tmux / Zellij | [README.tmux.md](README.tmux.md) |
| シェル (bash/zsh) | [README.shell.md](README.shell.md) |

## キーバインド

### fzf拡張キーバインド

fzfの標準キーバインドをカスタマイズしています。

| キー | 機能 | 備考 |
|------|------|------|
| `Ctrl+R` | コマンド履歴検索 | fzfでインクリメンタル検索 |
| `Ctrl+T` | ファイル検索 | カレントディレクトリ以下を検索、プレビュー付き |
| `Alt+C` | ディレクトリ移動 | fzfでディレクトリを選択してcd |

**カスタマイズ内容（`.shell_common`）:**
- `fd`コマンドがある場合、`find`より高速な`fd`を使用
- `Ctrl+T`でファイルプレビュー表示（`head -100`）
- 隠しファイル対応、`.git`除外

## ファイル追加時の注意

- 機密情報（トークン、パスワード等）は絶対にコミットしない
- 新しい設定ファイルは `HOME/` 配下に配置する
- 設定ファイルにはセクションごとにコメントを付与する
