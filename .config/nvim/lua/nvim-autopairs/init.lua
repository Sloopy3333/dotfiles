require('nvim-autopairs').setup({
	pairs_map = {
	    ["'"] = "'",
	    ['"'] = '"',
	    ['('] = ')',
	    ['['] = ']',
	    ['{'] = '}',
	    ['`'] = '`',
	},
	disable_filetype = { "FZFPrompt" },
	break_line_filetype = nil, -- mean all file type
	html_break_line_filetype = {'html' , 'vue' , 'typescriptreact' , 'svelte' , 'javascriptreact'},
	ignored_next_char = "%w",
	check_line_pair = true,
	})
