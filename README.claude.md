# Claude Code 設定

`HOME/.claude/` 配下のファイルは amu により `~/.claude/` へ個別に symlink される。

## 構成

| ファイル | 内容 |
|---|---|
| `CLAUDE.md` | 全プロジェクト共通の指示 |
| `PRINCIPLES.md` | 基本原則（全ルールに優先） |
| `CONSTITUTION.md` | ファイル追加・変更時の絶対ルール |
| `settings.json` | ユーザー設定（hooks / statusLine / プラグイン等、下記参照） |
| `statusline.sh` | ステータスラインスクリプト |
| `hooks/state-track.sh` | セッション実行状態の記録（下記参照） |
| `hooks/cmux-state.sh` | cmuxのタブへの実行状態・通知の表示（下記参照） |

## settings.json

hooks・statusLine・プラグイン等のユーザー設定。全マシンで共通になる。

- Claude Code はユーザー設定に限り symlink を辿って実体へ書き込むため、
  `/config` やプラグイン追加などの変更はリポジトリ側に差分として現れる（必要に応じてコミットする）
- hooks のコマンドは `~/.claude/hooks/...` で指定し、マシン固有の絶対パスを書かない
- `model`（`/model`）と `modelSettings`（`/effort`・`/autocompact`）はマシンごとの好みとして管理しない。
  `.gitattributes` の clean フィルタ `dropmodel` が git に入る前に jq で取り除く（実ファイルには残る）。
  フィルタの定義は `HOME/.gitconfig` にあり、settings.json と一緒にリンクされるので、
  Claude Code がリポジトリへ書き込める状態なら必ず効く（要 jq。`required` にしてあるので、
  jq が無いと `git add` が失敗し、キーが黙ってコミットされることはない）
  書き出し側の smudge は `cat`（素通し）。`required` のフィルタは smudge が未定義だと失敗扱いになり、
  pull・checkout・stash の適用で settings.json が書き出されずに消えるため、定義が要る

## ステータスライン

`statusline.sh` が Claude Code のコンソール下部に以下を1行で表示する
（`~/.claude/settings.json` の `statusLine` で有効化、要 jq）:

| セグメント | 内容 | 表示条件 |
|---|---|---|
| 実行状態 | `▶ Bash 1m35s`（実行中） / `⏸ 入力待ち` / `■ 完了`（5分以内） / `■ 待機` | hooks設定時 |
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

## 実行状態の追跡（ハング検知）

「止まっているのか裏で処理中なのか分からない」問題への対策。

- `hooks/state-track.sh` が Claude Code の hooks
  （UserPromptSubmit / PreToolUse / PostToolUse / PermissionRequest /
  Notification / Stop / SessionEnd 等）から呼ばれ、
  `~/.claude/state/<session_id>` に `状態|ツール名|時刻|tmuxペインID` を記録する
- hooks の設定は `settings.json` の `hooks` キー
- ステータスラインは `statusLine.refreshInterval: 5` により5秒ごとに再実行され、
  経過時間が動き続ける。`▶` のまま経過時間だけが異常に伸びていればハングの疑い
- 状態ファイルには tmux ペインIDも記録しているため、
  必要になれば tmux 側からも同じ状態を読める（現在は Claude Code 側のみで表示）

## cmux連携

`hooks/cmux-state.sh` が SessionStart / Stop / Notification hook から
`cmux claude-hook` を呼び、cmuxのタブに実行中・入力待ちと通知を表示する。
cmux外では何もしない。hooks の設定は `~/.claude/settings.json` の `hooks` キー。

- cmux同梱の `claude` ラッパーも同じhookを差し込むが、PATHで `~/.local/bin` の `claude` が
  先に見つかるため経由しない。そのため自前で設定している
- tmux内で使う場合は、cmuxのSettingsで **Socket Control Mode** を `automation` にする
  （既定の `cmux processes only` ではtmuxサーバー配下のhookが拒否される）
- tmuxのペインは tmux サーバーを起動したタブの `CMUX_WORKSPACE_ID` / `CMUX_SURFACE_ID` を引き継ぐため、
  ペインのsessionにattachしているtmuxクライアントの環境変数から実際のタブを特定する
  （Codex側の `hooks/cmux-state.sh` と同じ方式、[README.codex.md](README.codex.md) 参照）
