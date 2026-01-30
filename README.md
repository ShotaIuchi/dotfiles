# dotfiles

個人環境設定ファイルの管理リポジトリ。

## 構成

```
dotfiles/
├── HOME/           # ホームディレクトリに配置するファイル
│   └── .gitconfig
└── .gitignore
```

## セットアップ

### 推奨ツール: amu

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

#### リンクの管理

```bash
# リンク削除
amu remove ~/dot/dotfiles/HOME ~/

# 設定更新後の再適用
amu sync
```

## ファイル追加時の注意

- 機密情報（トークン、パスワード等）は絶対にコミットしない
- 新しい設定ファイルは `HOME/` 配下に配置する
- 設定ファイルにはセクションごとにコメントを付与する
