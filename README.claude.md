# Claude Code 設定

`HOME/.claude/` 配下のファイルは amu により `~/.claude/` へ個別に symlink される。

## 構成

| ファイル | 内容 |
|---|---|
| `CLAUDE.md` | 全プロジェクト共通の指示 |
| `PRINCIPLES.md` | 基本原則（全ルールに優先） |
| `CONSTITUTION.md` | ファイル追加・変更時の絶対ルール |
| `rules/` | 個別ルール |
| `skills/` | カスタムスキル |
| `statusline.sh` | ステータスラインスクリプト |

## ステータスライン

`statusline.sh` が Claude Code のコンソール下部に以下を1行で表示する
（`~/.claude/settings.json` の `statusLine` で有効化、要 jq）:

| セグメント | 内容 | 表示条件 |
|---|---|---|
| セッション名 | `--name` / `/rename` で付けた名前 | 設定時のみ |
| モデル | 表示名 + effortレベル / think / fast | 常時 |
| ctx | コンテキストウィンドウ残%（緑≥50 / 黄≥20 / 赤<20） | 常時 |
| 5h / 7d | レート制限使用率（5hはリセット時刻付き、灰<50 / 黄<80 / 赤≥80） | Pro/Maxのみ |
| +N -N | セッション中の追加・削除行数 | 常時 |
| $ | セッションの推定コスト | 常時 |
| Vimモード | NORMAL / INSERT / VISUAL | vimモード有効時のみ |

ブランチ・リポジトリ・worktree・PR状態は tmux ステータスバー側で表示するため
ここでは扱わない（[README.tmux.md](README.tmux.md) 参照）。

テーマは Tokyo Night（tmux と統一）。
