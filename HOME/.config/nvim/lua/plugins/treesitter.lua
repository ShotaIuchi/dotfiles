-- シンタックスハイライト: treesitter
return {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    event = { "VeryLazy" },
    cmd = { "TSInstall", "TSUpdate", "TSUpdateSync" },
    config = function()
        -- Neovim 0.10以降ではTreesitterハイライトがデフォルトで有効
        -- zshをbashとして登録してシンタックスハイライトを適用
        vim.treesitter.language.register("bash", "zsh")
    end,
}
