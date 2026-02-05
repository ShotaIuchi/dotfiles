---
name: dotfile-tools-install
description: dotfiles install.sh の更新・メンテナンス
argument-hint: "[add <tool>] [update] [list] [test]"
---

# /dotfile-tools-install

dotfilesのインストールスクリプトを管理・更新する。

## 目的

- install.sh に新しいツールを追加
- 既存ツールの設定を更新
- スクリプトの動作テスト
- 対応ツール一覧の表示

## 使い方

```
/dotfile-tools-install [コマンド] [オプション]
```

## コマンド

### list

install.sh に含まれる全ツールとOS対応状況を表示。

```
/dotfile-tools-install list
```

### add <tool>

新しいツールを対話式で追加。

```
/dotfile-tools-install add <ツール名>
```

入力項目:
- ツールの説明（日本語）
- 各パッケージマネージャーでのパッケージ名（brew, apt, dnf, pacman, winget, scoop）
- dotfilesとの関連説明
- OS制限（macOSのみ、Linuxのみ等）

### update

install.sh をレビュー・更新:
- インストールURL/スクリプトの最新確認（Homebrew、Claude Code等）
- 古いパッケージ名のチェック
- パッケージマネージャーコマンドの更新
- 不足しているOS対応の追加

```
/dotfile-tools-install update
```

### test

構文チェックとドライランテストを実行。

```
/dotfile-tools-install test
```

## 処理内容

### `list` の場合:

1. install.sh を読み込む
2. `install_package` 呼び出しとプロンプト関数をパース
3. テーブル形式で表示:

```
| ツール | brew | apt | dnf | pacman | winget | scoop |
|--------|------|-----|-----|--------|--------|-------|
| neovim | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| zellij | ✓ | - | - | ✓ | - | - |
...
```

### `add <tool>` の場合:

1. ツール情報を質問:
   - 表示名
   - 説明（日本語）
   - 各パッケージマネージャーでのパッケージ名（空欄=非対応）
   - caskかどうか（brew専用）
   - dotfilesとの関連説明
   - OS制限

2. 追加するコードを生成:
   - `INSTALL_DECISIONS` と `ALREADY_INSTALLED` 配列に追加
   - `prompt_<tool>()` 関数を追加
   - `detect_installed()` に検出処理を追加
   - `install_package` またはカスタムインストール関数の呼び出しを追加
   - 必要に応じて `show_summary()` のスキップ条件を更新

3. install.sh の適切な場所に挿入

### `update` の場合:

1. 現在の install.sh を読み込む
2. インストールURL/スクリプトの最新確認:
   - Homebrew: `https://raw.githubusercontent.com/Homebrew/dotfile-tools-install/HEAD/dotfile-tools-install.sh`
   - Claude Code (macOS/Linux): `https://claude.ai/dotfile-tools-install.sh`
   - Claude Code (Windows): `https://claude.ai/dotfile-tools-install.ps1`
   - Scoop: `irm get.scoop.sh | iex`
3. 各ツールのパッケージ可用性をチェック
4. パッケージ名変更があれば更新を提案
5. 非推奨パターンを更新（npm install等）

### `test` の場合:

1. `bash -n install.sh` で構文チェック
2. モック入力でフローを検証
3. 問題があれば報告

## ファイル場所

```
~/dot/dotfiles/dotfile-tools-install.sh
```

## 使用例

```bash
# 対応ツール一覧
/dotfile-tools-install list

# ripgrep を追加
/dotfile-tools-install add ripgrep

# install.sh を更新
/dotfile-tools-install update

# install.sh をテスト
/dotfile-tools-install test
```

## 注意事項

- 大きな変更前は install.sh をバックアップ
- 変更後は対象OSでテスト
- 既存のコードスタイルとパターンに従う
