# Codex CLI 設定

`~/.codex/config.toml` は API キー・trusted プロジェクト一覧を含み Codex 自身が書き換えるため、
リポジトリでは管理しない（Claude Code の `~/.claude/settings.json` と同じ扱い）。
ここには手動で追加した設定のみ記録する。
`hooks.json` とhookスクリプトは、秘密情報を含まないため `HOME/.codex/` で管理する。

## インストール

リポジトリの対話式インストーラーから Codex CLI を選択できる。

```bash
./install.sh
```

個別に更新する場合:

```bash
./install.sh --update codex
```

macOS/Linux では[公式スタンドアロンインストーラー](https://developers.openai.com/codex/cli/)を使用する。
Windows（Git Bash）では Node.js が導入済みの場合に npm でインストールする。

## エイリアス

`fcodex`で承認とサンドボックスを無効化したCodex CLIを起動する。

```bash
fcodex
```

実体は`codex --dangerously-bypass-approvals-and-sandbox`。
OSのroot権限へ自動昇格はしないが、実行ユーザーがアクセス可能なファイルとネットワークに
確認なしでアクセスできるため、信頼できるリポジトリでのみ使用する。

## cmux連携

cmux内でCodexを実行すると、`hooks/cmux-state.sh` がライフサイクルイベントを受け取り、
ワークスペースのステータスに実行中・承認待ち・完了・中断を表示する。
承認待ち・ターン完了・中断時にはcmuxの通知も送る。cmux外では何もしない。
ステータスはタブ（surface）単位で分けるため、同じワークスペースで複数のCodexを実行しても干渉しない。

Codex初回起動時にhookの信頼確認が表示された場合は、内容を確認して許可する。

### tmux内で使う場合

cmuxのソケットは既定（`cmux processes only`）では、cmuxのターミナルから起動したプロセスしか接続できない。
tmux内のhookはtmuxサーバー配下で動くため拒否される。
cmuxのSettingsで **Socket Control Mode** を `automation`（同じmacOSユーザーのプロセスを許可）に変更する。

また、tmuxのペインは tmux サーバーを起動したタブの `CMUX_WORKSPACE_ID` / `CMUX_SURFACE_ID` を引き継ぐため、
hookはペインのsessionにattachしているtmuxクライアント（最後に操作されたもの）の環境変数から
実際のタブを特定する。sessionがどのタブにもattachされていない場合は何もしない。

## ステータスライン

Codex は Claude Code のような外部スクリプト実行には非対応で、
組み込み項目 ID を `[tui].status_line` に並べる方式のみ。
Codex 内の `/statusline` で対話的に設定しても同じキーに書き込まれる。

```toml
[tui]
status_line = ["model-with-reasoning", "run-state", "context-remaining", "five-hour-limit", "weekly-limit", "estimated-thread-cost"]
```

| 項目 ID | 内容 | Claude Code 側の対応 |
|---|---|---|
| `model-with-reasoning` | モデル + reasoning effort | モデル + effort |
| `run-state` | 実行中 / 待機などの状態 | 実行状態（経過時間なし） |
| `context-remaining` | コンテキストウィンドウ残% | ctx |
| `five-hour-limit` / `weekly-limit` | レート制限使用率 | 5h / 7d |
| `estimated-thread-cost` | スレッドの推定コスト | $ |

変更行数・Vimモード・配色指定は Codex では不可。
色はテーマ由来で `status_line_use_colors` の ON/OFF のみ。

項目 ID の集合は Codex のバージョンで変わる（0.153.4 で確認）。
他に `context-used`, `used-tokens`, `total-input-tokens`, `total-output-tokens`,
`thread-credits`, `current-dir`, `git-branch`, `thread-title`, `thread-id`, `codex-version`,
`activity`, `task-progress` などがある。
正確な一覧は `/statusline` のピッカーで確認する。

## スクロールで流れる件（未解決）

Codex の会話画面は常にインライン描画（通常バッファ）で、入力欄とステータスラインは
画面最下部の数行として描かれる。tmux でホイールを回すと copy-mode に入り pane の履歴ごと
スクロールするため、Claude Code（代替スクリーン + マウスキャプチャ）と違い
ステータスラインは画面外へ流れる。
`tui.alternate_screen = "always"` は resume ピッカーや Ctrl+T のトランスクリプト等の
オーバーレイにしか効かず、会話画面には影響しない（0.153.4 で tmux 上で確認、
openai/codex#24552 と同じ挙動）。pane 最下部へ戻れば表示される。
