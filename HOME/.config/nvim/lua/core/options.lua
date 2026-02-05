-- コアオプション（.vimrcから移行）
local opt = vim.opt

-- 文字エンコーディング
opt.fileformat = "unix"
opt.encoding = "utf-8"
opt.fileencoding = "utf-8"

-- 表示
opt.laststatus = 2
opt.termguicolors = true
opt.list = true
opt.listchars = { tab = "»-", trail = "␠", extends = "»", precedes = "«", nbsp = "%" }
opt.title = true
opt.number = true
opt.relativenumber = true
opt.cursorline = true
opt.showmatch = true
opt.showcmd = true
opt.signcolumn = "yes"
opt.scrolloff = 8
opt.sidescrolloff = 8

-- ファイル操作
opt.autoread = true
opt.backup = false
opt.swapfile = false
opt.undofile = true

-- 編集
opt.backspace = { "indent", "eol", "start" }
opt.clipboard = "unnamedplus"
opt.mouse = "a"
opt.wrap = false

-- 検索
opt.hlsearch = true
opt.incsearch = true
opt.ignorecase = true
opt.smartcase = true

-- タブ・インデント
opt.tabstop = 4
opt.shiftwidth = 4
opt.softtabstop = 4
opt.expandtab = true
opt.autoindent = true
opt.smartindent = true

-- サウンド
opt.visualbell = true
opt.errorbells = false

-- ウィンドウ分割
opt.splitbelow = true
opt.splitright = true

-- 補完
opt.completeopt = { "menu", "menuone", "noselect" }

-- パフォーマンス
opt.updatetime = 250
opt.timeoutlen = 300
