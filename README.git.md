# Git 設定

pager は delta（行番号付き diff 表示）。

## 主要設定

| 設定 | 値 | 説明 |
|---|---|---|
| defaultBranch | `main` | - |
| pull.rebase | `true` | pull 時に merge ではなく rebase |
| fetch.prune | `true` | 削除済みリモートブランチを自動削除 |
| rerere.enabled | `true` | コンフリクト解消を記憶して再利用 |
| rebase.autoStash | `true` | rebase 前に自動 stash |
| core.commentChar | `;` | `#` の代わりに `;` を使用 |

## エイリアス

### 情報

| エイリアス | コマンド | 説明 |
|---|---|---|
| `git aliass` | `config --get-regexp alias` | エイリアス一覧 |
| `git me` | `config --get-regexp user` | ユーザー情報表示 |

### Add

| エイリアス | コマンド | 説明 |
|---|---|---|
| `git a` | `add` | - |
| `git aa` | `add --all` | 全ファイル追加 |
| `git ap` | `add --patch` | 対話的に部分追加 |
| `git au` | `add --update` | 変更・削除のみ追加 |

### Branch

| エイリアス | コマンド | 説明 |
|---|---|---|
| `git b` | `branch` | - |
| `git ba` | `branch --all` | リモート含む全ブランチ |

### Log

| エイリアス | コマンド | 説明 |
|---|---|---|
| `git l` | `log --oneline -20` | 直近20件 |
| `git ll` | `log --oneline` | 全件（1行表示） |
| `git lt` | `log --oneline --topo-order` | ブランチの流れ順 |
| `git ld` | `log --oneline --author-date-order` | 作業した時刻順 |

### Status

| エイリアス | コマンド |
|---|---|
| `git s` | `status` |

### Checkout

| エイリアス | コマンド | 説明 |
|---|---|---|
| `git c` | `checkout` | - |
| `git cm` | `checkout main` | main に切替 |
| `git cempty` | `checkout --orphan` | 履歴なしブランチ作成 |

### Commit

| エイリアス | コマンド | 説明 |
|---|---|---|
| `git co` | `commit` | - |
| `git com` | `commit --message` | メッセージ指定 |
| `git coa` | `commit --amend` | 直前のコミットを修正 |
| `git coan` | `commit --amend --no-edit` | メッセージ変更なしで修正 |

### Diff

| エイリアス | コマンド | 説明 |
|---|---|---|
| `git d` | `diff` | - |
| `git ds` | `diff --cached` | ステージ済みの差分 |
| `git db` | `diff --stat` | 統計情報のみ |
| `git dd` | `diff HEAD~ HEAD` | 直前コミットの差分 |
| `git dn` | `diff --name-only HEAD~ HEAD` | 変更ファイル名のみ |

### State Change

| エイリアス | コマンド | 説明 |
|---|---|---|
| `git unstage` | `reset HEAD --` | ステージ取り消し |
| `git pick` | `cherry-pick` | - |

### Remote

| エイリアス | コマンド | 説明 |
|---|---|---|
| `git pushme` | `push origin <current-branch>` | 現在のブランチを push |
| `git pushmef` | `push --force-with-lease origin <current-branch>` | force push（安全版） |
| `git cofirst` | `commit --allow-empty -m 'Initial commit'` | 空の初回コミット |

### 破壊的操作

| エイリアス | コマンド | 説明 |
|---|---|---|
| `git fback` | `reset --soft HEAD~1` | コミット取消（変更は保持） |
| `git fclean` | `reset --hard HEAD && clean -fd` | ローカル変更を全削除 |
| `git faclean` | `fclean` + untracked 削除 | 完全クリーン |

## シェル拡張コマンド

`.shell_common` で定義されたシェル関数による Git サブコマンド。

| コマンド | 機能 |
|---|---|
| `git cw` | fzf で worktree / ブランチを選択して切替 |
| `git cw <name>` | 指定ブランチの worktree に移動（なければ作成） |
| `git cw -b <name>` | 新規ブランチの worktree を作成して移動 |
| `git cb` | fzf でブランチ選択して checkout |
| `git cb <name>` | 指定ブランチに checkout（なければ新規作成） |
| `git cb -b <name>` | 新規ブランチを作成して checkout |
| `git repo` | fzf で ghq 管理のリポジトリを選択して cd |
| `git wt` | wtp コマンドへのパススルー |

## Delta (diff viewer)

| 設定 | 値 |
|---|---|
| line-numbers | 有効 |
| navigate | 有効（n/N で差分間移動） |
| side-by-side | 無効 |
| conflictstyle | diff3 |
