vim.cmd('syntax on')					--enable syntax highlight
vim.cmd('filetype plugin indent on')                    --indent based on file type
vim.cmd('filetype plugin on')                           --file type plugins on
vim.o.encoding = "Utf-8"				--encoding to display
vim.o.fileencoding = "Utf-8"				--encoding to write to file
vim.o.hidden = true					--stop unloading of hiden buffers
vim.o.autochdir = true					--cd to pwd
vim.bo.tabstop = 4				        --set 4 space for tab
vim.bo.shiftwidth = 4					--number spaces to use for each step of auto indent
vim.o.smarttab = true					--when tab is infront of line respects shidtwidth, tabstop, softtabstop
vim.bo.softtabstop = 4
vim.bo.expandtab = true					--convert tabs to spaces
vim.bo.autoindent = true	         		--automatic indentation
vim.bo.smartindent = true				--smart auto indentation
vim.wo.wrap = false					-- disable line wrap
vim.o.ruler = true					--show line and column number always
vim.wo.relativenumber = true				--show relative nuberline
vim.o.incsearch = true                                  --enable incremental search
vim.o.hlsearch = false                                  --disable search highlight
vim.o.ignorecase = true                                 --ignore case while mathing
vim.o.smartcase = true                                  --use case sensitive matching if search term has a upper case letter
vim.wo.cursorline = true                                --highlight selected line
vim.o.cmdheight = 1                                     --height of the command line
vim.o.laststatus = 2	                                --always display statusline
vim.o.showmode = false                                  --disable showing -- mode --
vim.o.clipboard = "unnamedplus"                         --set clipboard to system primary clipboard
vim.o.path = ".,,**"                                    --for file search
vim.b.noswapfile = true	         	                --disable swapfile
vim.cmd('set noswapfile')
vim.o.backup = false			                --disable backup file
vim.bo.undofile = true			                --use undofile
vim.bo.modifiable = true			        --make buffer modifiable
vim.o.termguicolors = true                              --true color support
vim.o.background = "dark"                               --set dark background
vim.o.lazyredraw = true                                 --screen will not redraw while executing macros
vim.o.pumheight= 10                                     --max number of items to show in popup menu
vim.o.wildmenu = true                                   --better tab completion in command mode
vim.o.wildmode="longest:full,full"                      --first tab to complete longest string second to show matched list
vim.o.updatetime = 300                                  --faster completion
vim.o.shortmess = 'c'					--remove pattern not found in lsp
vim.o.completeopt = "menuone,noselect"                  --first tab to complete longest string second to show matched list
