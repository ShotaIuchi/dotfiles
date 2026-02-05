-- プラグインマネージャー: lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable",
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

-- プラグイン仕様
require("lazy").setup({
    -- カラースキーム
    require("plugins.colorscheme"),

    -- ステータスライン
    require("plugins.lualine"),

    -- Git変更表示
    require("plugins.gitsigns"),

    -- Treesitter（シンタックスハイライト）
    require("plugins.treesitter"),

    -- Gitコマンド
    {
        "tpope/vim-fugitive",
        cmd = { "Git", "G", "Gdiffsplit", "Gread", "Gwrite", "Ggrep", "GMove", "GDelete", "GBrowse" },
        keys = {
            { "<leader>gs", "<cmd>Git<CR>", desc = "Gitステータス" },
            { "<leader>gb", "<cmd>Git blame<CR>", desc = "Git blame" },
            { "<leader>gd", "<cmd>Gdiffsplit<CR>", desc = "Git差分" },
        },
    },

    -- コメントトグル
    {
        "numToStr/Comment.nvim",
        event = { "BufReadPre", "BufNewFile" },
        opts = {},
    },

    -- 自動括弧
    {
        "windwp/nvim-autopairs",
        event = "InsertEnter",
        opts = {},
    },
}, {
    -- lazy.nvimオプション
    install = {
        colorscheme = { "tokyonight" },
    },
    checker = {
        enabled = true,
        notify = false,
    },
    change_detection = {
        notify = false,
    },
})

return {}
