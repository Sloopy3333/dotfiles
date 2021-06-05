local map = vim.api.nvim_set_keymap
local opts = {noremap = true, silent = true}

map('n', '<Space>', '<NOP>', opts)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

map('n', '<leader>.', ":lua require'telescope.builtin'.find_files{}<CR>", opts)
map('n', '<leader>/', ":lua require'telescope.builtin'.current_buffer_fuzzy_find{}<CR>", opts)

-- buffers
map('n', '<A-b>', ":lua require'telescope.builtin'.buffers{}<CR>", opts)
map('n', '<A-tab>', ':bn<CR>', opts)
map('n', '<S-tab>', ':bp<CR>', opts)
map('n', '<A-q>', ':bp|sp|bn|bd<CR>', opts)

-- window
-- split
map('n', '<A-v>', ':vsplit<CR>', opts)
map('n', '<A-s>', ':split<CR>', opts)
-- window movement
map('n', '<A-j>', ':wincmd j<CR>', opts)
map('n', '<A-k>', ':wincmd k<CR>', opts)
map('n', '<A-h>', ':wincmd h<CR>', opts)
map('n', '<A-l>', ':wincmd l<CR>', opts)
-- window resize
map('n', '<A-J>', ':resize +5 j<CR>', opts)
map('n', '<A-K>', ':resize -5 k<CR>', opts)
map('n', '<A-L>', ':vertical resize +5<CR>', opts)
map('n', '<A-H>', ':vertical resize -5<CR>', opts)

-- files
map('n', '<leader>ff', ':Files ~/<CR>', opts)
map('n', '<leader>fg', ':GFiles<CR>', opts)
map('n', '<leader>fw', ':Rg<CR>', opts)
map('n', '<leader>fr', ':History<CR>', opts)
map('n', '<leader>fn', ':e', opts)

-- config & help
map('n', '<leader>he', ':Files ~/.config/nvim/<CR>', opts)
map('n', '<leader>ht', ':Colors<CR>', opts)
map('n', '<leader>hh', 'Helptags<CR>', opts)
map('n', '<leader>hc', 'Commands<CR>', opts)

-- fixing some annoying keys
vim.cmd([[
	cmap WQ wq
	cmap Wq wq
	cmap W  w
	cmap Q  q
   ]])

-- lsp
--vim.cmd("nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>")
--vim.cmd("nnoremap <silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>")
--vim.cmd("nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>")
--vim.cmd("nnoremap <silent> gi <cmd>lua vim.lsp.buf.implementation()<CR>")
--vim.cmd("nnoremap <silent> ca :Lspsaga code_action<CR>")
--vim.cmd("nnoremap <silent> K :Lspsaga hover_doc<CR>")
---- vim.cmd('nnoremap <silent> <C-k> <cmd>lua vim.lsp.buf.signature_help()<CR>')
--vim.cmd("nnoremap <silent> <C-p> :Lspsaga diagnostic_jump_prev<CR>")
--vim.cmd("nnoremap <silent> <C-n> :Lspsaga diagnostic_jump_next<CR>")
---- scroll down hover doc or scroll in definition preview
--vim.cmd("nnoremap <silent> <C-f> <cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<CR>")
---- scroll up hover doc
--vim.cmd("nnoremap <silent> <C-b> <cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<CR>")
--vim.cmd('command! -nargs=0 LspVirtualTextToggle lua require("lsp/virtual_text").toggle()')
--
--
--
