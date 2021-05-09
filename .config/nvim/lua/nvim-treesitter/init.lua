local ts_config = require("nvim-treesitter.configs")

ts_config.setup {
    ensure_installed = {
        "javascript",
        "html",
        "css",
        "bash",
        "cpp",
        "rust",
        "lua",
        "python",
        "haskell"
    },
    highlight = {
        enable = true,
        use_languagetree = true
    },
    indent = {
	    enable = true
    }
}
