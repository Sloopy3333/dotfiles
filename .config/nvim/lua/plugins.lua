---@diagnostic disable: undefined-global
-- auto install packer if doesnt exists
local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
  execute 'packadd packer.nvim'
end

--requried if packer is marked as opt
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
	--packer
	use {'wbthomason/packer.nvim', opt = true}
	--colors
	use {'dracula/vim'}
	use {'morhetz/gruvbox'}
	use {"nvim-treesitter/nvim-treesitter"}
	--fzf
        use {'junegunn/fzf'}
        use {'junegunn/fzf.vim'}
	--misc
--        use {'glepnir/dashboard-nvim'}
	use {"windwp/nvim-autopairs"}
        use {'vimwiki/vimwiki',opt = true}
        use {'kyazdani42/nvim-web-devicons'}
        use {'glepnir/galaxyline.nvim'}
        use {'kyazdani42/nvim-tree.lua'}
	use {'akinsho/nvim-bufferline.lua'}
        use {'AckslD/nvim-whichkey-setup.lua', requires = {'liuchengxu/vim-which-key'}}
	use {'glacambre/firenvim', run = ':call firenvim#install(0)',opt = false}

	--lsp
        use {'neovim/nvim-lspconfig'}
        use {'hrsh7th/nvim-compe'}
end)
