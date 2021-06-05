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
	use {'morhetz/gruvbox'}
	use {'altercation/vim-colors-solarized'}
	use {"nvim-treesitter/nvim-treesitter"}
        use {'junegunn/fzf'}
        use {'junegunn/fzf.vim'}
	use {"windwp/nvim-autopairs"}
        use {'kyazdani42/nvim-web-devicons'}
        use {'glepnir/galaxyline.nvim'}
        use {'kyazdani42/nvim-tree.lua'}
	use {'glacambre/firenvim', run = ':call firenvim#install(0)',opt = false}
        use {'neovim/nvim-lspconfig'}
        use {'hrsh7th/nvim-compe'}
        use { 'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
        }
end)
