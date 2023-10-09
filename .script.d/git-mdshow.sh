#!/bin/bash

commit_ref="${1:-HEAD}"

# 最新コミットの各種情報を取得
commit_hash_short=$(git log "$commit_ref" -n 1 --format="%h")
commit_hash=$(git log "$commit_ref" -n 1 --format="%H")
author_name=$(git log "$commit_ref" -n 1 --format="%an")
author_mail=$(git log "$commit_ref" -n 1 --format="%ae")
author_date=$(git log "$commit_ref" -n 1 --format="%ad")
committer_name=$(git log "$commit_ref" -n 1 --format="%cn")
committer_mail=$(git log "$commit_ref" -n 1 --format="%ce")
committer_date=$(git log "$commit_ref" -n 1 --format="%cd")
commit_message=$(git log "$commit_ref" -n 1 --format="%B")
#commit_subject=$(git log -n 1 --format="%s")

# 出力
echo "commit $commit_hash_short ($commit_hash)"
echo "Auther:   $author_name ($author_mail)"
echo "Date:     $author_date"
echo "Commiter: $committer_name ($committer_mail)"
echo "Date:     $committer_date"
echo ""
echo "$commit_message"
echo ""
echo "\`\`\`diff"
git diff-tree -p "$commit_hash"
echo "\`\`\`"
