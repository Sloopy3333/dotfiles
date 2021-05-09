--local custom_lsp_attach = function(client)
--  vim.api.nvim_buf_set_keymap(0, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', {noremap = true})
--  vim.api.nvim_buf_set_keymap(0, 'n', '<c-]>', '<cmd>lua vim.lsp.buf.definition()<CR>', {noremap = true})
--  vim.api.nvim_buf_set_option(0, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
--end

require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;

    source = {
       path = {kind = "  "},
        buffer = {kind = "  "},
        calc = {kind = "  "},
        vsnip = {kind = "  "},
        nvim_lsp = {kind = "  "},
        nvim_lua = {kind = "  "},
        spell = {kind = "  "},
        tags = {kind = "  "},
--        treesitter = {kind = "  "},
        emoji = {kind = " ﲃ ", filetypes={"markdown"}}
    }
}

--Diagnostic
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = true,
    signs = true,
  }
)

local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  elseif vim.fn.call("vsnip#jumpable", {-1}) == 1 then
    return t "<Plug>(vsnip-jump-prev)"
  else
    return t "<S-Tab>"
  end
end


local lua_lsp = '/home/sam/.local/share/nvim/lsp/lua/extension/server/bin/Linux/lua-language-server'
local lua_main = '/home/sam/.local/share/nvim/lsp/lua/extension/server/main.lua'

local lspconfig = require'lspconfig'

-- bash
-- npm i bash-language-server
require'lspconfig'.bashls.setup {
    cmd = {bash_lsp, 'start'},
}

--clang
--https://clangd.llvm.org/installation.html
require'lspconfig'.clangd.setup {
    cmd = {"ccls", '--background-index'},
    filetypes = { 'c', 'cpp', 'objc', 'objcpp' },
    root_dir = lspconfig.util.root_pattern("compile_commands.json", "compile_flags.txt", ".git")
}

--haskell
--https://github.com/haskell/haskell-language-server/releases
require'lspconfig'.ghcide.setup {
    cmd = {"haskell-language-server-wrapper", '--lsp'},
    filetypes = { 'haskell', 'lhaskell' },
    root_dir = lspconfig.util.root_pattern('stack.yaml', 'hie-bios', 'BUILD.bazel', 'cabal.config', 'package.yaml', 'xmonad.hs')

}

--python
--pip install python-language-server
require'lspconfig'.pyright.setup {
	cmd = {"pyright", '--stdio'},
	filetypes = { "python" },
	settings = {
      python = {
        analysis = {
          autoSearchPaths = true,
          useLibraryCodeForTypes = true
        }
      }
    }
    }

--lua
--https://github.com/sumneko/lua-language-server/wiki/Build-and-Run-(Standalone)
--https://api.github.com/repos/sumneko/vscode-lua/releases/latest
require'lspconfig'.sumneko_lua.setup {
 cmd = {lua_lsp, "-E", lua_main};
 settings = {
    Lua = {
      runtime = {
        version = 'LuaJIT',
        path = vim.split(package.path, ';'),
      },
      diagnostics = {
        globals = {'vim'},
      },
      workspace = {
        library = {
          [vim.fn.expand('$VIMRUNTIME/lua')] = true,
          [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
        },
      },
      telemetry = {
        enable = false,
      },
    },
  },
}
