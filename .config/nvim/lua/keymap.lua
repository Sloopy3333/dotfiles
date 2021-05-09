local map = vim.api.nvim_set_keymap

-- leader
map('n', '<Space>', '<NOP>', {noremap = true, silent = true})
local wk = require("whichkey_setup")
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
local which_key_map = {}

which_key_map['.'] = {':Files<CR>', 'search current directory'}
which_key_map['['] = {':bn<CR>', 'next buffer'}
which_key_map[']'] = {':bn<CR>', 'previous buffer'}

-- Files
which_key_map.f = {
	name = '+Files',
	f = {':Files ~/<CR>', 'search all files'},
	g = {':GFiles <CR>', 'git files'},
	w = {':Rg<CR>', 'find words'},
	r = {':History<CR>', 'recent files'},
	n = {':e', 'new file'},
}

which_key_map.b = {
	name = '+Buffers',
	b = {':Buffers<CR>', 'switch buffer'},
	d = {':bd<CR>', 'kill buffer'},
	D = {':%bd | Dashboard<CR>', 'kill all buffer'},
	f = {':BLines<CR>', 'search in buffer'},
	k = {':bd<CR>', 'kill buffer'},
	K = {':%bd | Dashboard<CR>', 'kill all buffer'},
	n = {':bn<CR>', 'next buffer'},
	p = {':bp<CR>', 'previous buffer'},
	s = {':w<CR>', 'save buffer'},
	S = {':wa<CR>', 'save all buffer'},
	Tab = {':bn<CR>', 'next buffer'},
}
-- vimwiki
which_key_map.v = {
	name = '+vimwiki',
	v = {':VimwikiIndex<CR>', 'open vimwiki'}
}

which_key_map.w = {
	name = '+window',
	v = {':vsplit<CR>', 'vertical split'},
	s = {':split<CR>', 'horizontal split'},
	w = {':Windows<CR>', 'switch window'},
	j = {':wincmd j<CR>', 'focus window down'},
	k = {':wincmd k<CR>', 'focus window up'},
	l = {':wincmd l<CR>', 'focus window right'},
	h = {':wincmd h<CR>', 'focus window left'},
	L = {':vertical resize +5<CR>', 'increase window width'},
	H = {':vertical resize -5<CR>', 'decrease window width'},
	J = {':resize +5<CR>', 'increase window height'},
	K = {':resize -5<CR>', 'decrease window height'},
}

---- omnicomplete
vim.cmd([[
    inoremap <expr> <c-j> ("\<C-n>")
    inoremap <expr> <c-k> ("\<C-p>")
    inoremap <expr> <CR> pumvisible() ? "\<C-y>""\<C-g>u\<CR>"
]])

-- scession
which_key_map.s = {
	name = "+scession",
	s = {':ScessionSave<CR>', 'save scession'},
	l = {':ScessionLoad<CR>', 'load scession'},
}

-- settings
which_key_map.h = {
	name = '+settings',
	e = {':Files ~/.config/nvim/<CR>', 'edit neovim configs'},
	t = {':Colors<CR>', 'change colors'},
	h = {':Helptags<CR>', 'help tags'},
	c = {':Commands<CR>', 'help commands'},
}

-- fixing some annoying keys
vim.cmd([[
	cmap WQ wq
	cmap Wq wq
	cmap W  w
	cmap Q  q
   ]])

--lsp
which_key_map.c = {
	name = '+code',
	h = {':lua vim.lsp.buf.hover()<CR>', 'go to definition'},
	d = {':lua vim.lsp.buf.declaration()<CR>', 'go to declaration'},
	D = {':lua vim.lsp.buf.definition()<CR>', 'go to definition'},
	i = {':lua vim.lsp.buf.implementation()<CR>', 'go to implementation'},
	r = {':lua vim.lsp.buf.references()<CR>', 'go to references'},
	f = {':lua vim.lsp.buf.formatting()<CR>', 'format code'},
	a = {':lua vim.lsp.buf.code_action()<CR>', 'code actions'},
}

local custom_lsp_attach = function(client)
  vim.api.nvim_buf_set_keymap(0, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', {noremap = true})
  vim.api.nvim_buf_set_keymap(0, 'n', '<c-]>', '<cmd>lua vim.lsp.buf.definition()<CR>', {noremap = true})
  vim.api.nvim_buf_set_option(0, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
end

wk.register_keymap('leader', which_key_map)
