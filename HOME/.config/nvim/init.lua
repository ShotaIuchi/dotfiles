-- Neovim設定のエントリーポイント
-- Leaderキーはlazy.nvimより前に設定する必要がある
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- コア設定
require("core.options")
require("core.keymaps")
require("core.autocmds")

-- プラグインマネージャー (lazy.nvim)
require("plugins")
