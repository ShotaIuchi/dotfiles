# Commit Schema

## 言語

コミットメッセージは**日本語**で記述する。

## フォーマット

```
<type>: <subject>

<body>

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
```

## Type

| Type | 用途 |
|------|------|
| `feat` | 新機能追加 |
| `fix` | バグ修正 |
| `docs` | ドキュメントのみの変更 |
| `style` | コードの意味に影響しない変更（空白、フォーマット等） |
| `refactor` | バグ修正でも機能追加でもないコード変更 |
| `chore` | ビルドプロセスや補助ツールの変更 |
| `test` | テストの追加・修正 |

## Subject

- 50文字以内
- 末尾に句点を付けない
- 何をしたかを簡潔に記述

## Body（任意）

- 変更の理由や背景を説明
- 1行72文字以内で折り返し

## 例

```
feat: .gitignoreを追加

機密ファイルや一時ファイルをリポジトリから除外する。

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
```

```
docs: READMEにセットアップ手順を追記

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
```
