-- キーマップ設定
local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }

-- Escapeの改善
keymap("i", "jk", "<Esc>", opts)
keymap("i", "jj", "<Esc>", opts)

-- 検索ハイライトをクリア
keymap("n", "<Esc>", "<cmd>nohlsearch<CR>", opts)

-- ウィンドウ移動
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- ウィンドウリサイズ
keymap("n", "<C-Up>", "<cmd>resize +2<CR>", opts)
keymap("n", "<C-Down>", "<cmd>resize -2<CR>", opts)
keymap("n", "<C-Left>", "<cmd>vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", "<cmd>vertical resize +2<CR>", opts)

-- バッファ移動
keymap("n", "<S-h>", "<cmd>bprevious<CR>", opts)
keymap("n", "<S-l>", "<cmd>bnext<CR>", opts)
keymap("n", "<leader>bd", "<cmd>bdelete<CR>", { desc = "バッファを削除" })

-- 行の移動
keymap("v", "J", ":m '>+1<CR>gv=gv", opts)
keymap("v", "K", ":m '<-2<CR>gv=gv", opts)

-- インデントモードを維持
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- 改善されたペースト（レジスタを上書きしない）
keymap("v", "p", '"_dP', opts)

-- ファイル保存
keymap("n", "<leader>w", "<cmd>w<CR>", { desc = "ファイルを保存" })
keymap("n", "<leader>q", "<cmd>q<CR>", { desc = "終了" })
keymap("n", "<leader>Q", "<cmd>qa!<CR>", { desc = "すべて終了" })

-- 診断キーマップ
keymap("n", "[d", vim.diagnostic.goto_prev, { desc = "前の診断" })
keymap("n", "]d", vim.diagnostic.goto_next, { desc = "次の診断" })
keymap("n", "<leader>d", vim.diagnostic.open_float, { desc = "診断を表示" })
