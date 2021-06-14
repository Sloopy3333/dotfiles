local lspconfig = require"lspconfig"

--clang
require"lspconfig".clangd.setup {
    cmd = {"clangd", "--background-index"},
    filetypes = { "c", "cpp", "objc", "objcpp" },
}

--haskell
require"lspconfig".ghcide.setup {
    cmd = {"haskell-language-server-wrapper", "--lsp"},
    filetypes = { "haskell", "lhaskell" },
    root_dir = lspconfig.util.root_pattern("stack.yaml", "hie-bios", "BUILD.bazel", "cabal.config", "package.yaml", "xmonad.hs")

}

--python
require"lspconfig".pyright.setup {
	cmd = {"pyright-langserver", "--stdio"},
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
--local sumneko_root_path = "/usr/share/lua-language-server"
--local sumneko_binary = "/usr/bin/lua-language-server"
--require"lspconfig".sumneko_lua.setup {
--  cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"};
--  settings = {
--    Lua = {
--      runtime = {
--        -- Tell the language server which version of Lua you"re using (most likely LuaJIT in the case of Neovim)
--        version = "LuaJIT",
--        -- Setup your lua path
--        path = vim.split(package.path, ";"),
--      },
--      diagnostics = {
--        -- Get the language server to recognize the `vim` global
--        globals = {"vim"},
--      },
--      workspace = {
--        -- Make the server aware of Neovim runtime files
--        library = {
--          [vim.fn.expand("$VIMRUNTIME/lua")] = true,
--          [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
--        },
--      },
--      -- Do not send telemetry data containing a randomized but unique identifier
--      telemetry = {
--        enable = false,
--      },
--    },
--  },
--}
