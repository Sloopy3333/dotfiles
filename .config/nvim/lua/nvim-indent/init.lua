local indent = 2

vim.g.indentLine_enabled = 1
vim.g.indent_blankline_char = "▏"

vim.cmd("hi IndentBlanklineChar guifg=#373b43 gui=nocombine")

vim.g.indent_blankline_filetype_exclude = {"help", "terminal", "dashboard"}
vim.g.indent_blankline_show_trailing_blankline_indent = false
vim.g.indent_blankline_show_first_indent_level = false
vim.g.indent_blankline_use_treesitter = true
