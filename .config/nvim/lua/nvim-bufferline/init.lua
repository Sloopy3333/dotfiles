vim.o.termguicolors = true
local colors = {
    -- bg = '#2E2E2E',
    bg = '#282828',
    fg = "fbf1c7",
    bg2 = "3c3836",
    red = '#fb4934',
    green = "#b8bb26",
    yellow = '#fabd2f',
    blue = '#83a598',
    purple = '#d3869b',
    cyan = '#8ec076',
    orange = '#fe8019',
    grey = '#928374',
}
-- colors for active , inactive buffer tabs
require "bufferline".setup {
    options = {
        buffer_close_icon = "",
        modified_icon = "●",
        left_trunc_marker = "",
        right_trunc_marker = "",
        max_name_length = 14,
        max_prefix_length = 13,
        tab_size = 18,
        enforce_regular_tabs = true,
        view = "multiwindow",
        show_buffer_close_icons = false,
        separator_style = "thin"
    },
    highlights = {
        background = {
            guifg = colors.white,
            guibg = colors.bg
        },
        fill = {
            guifg = colors.bg,
            guibg = colors.bg
        },
        buffer_selected = {
            guifg = colors.yellow,
            guibg = colors.bg,
            gui = "bold"
        },
        separator_visible = {
            guifg = colors.fg,
            guibg = colors.bg
        },
        separator_selected = {
            guifg = colors.red,
            guibg = colors.bg
        },
        separator = {
            guifg = colors.bg,
            guibg = colors.bg
        },
        indicator_selected = {
            guifg = colors.bg,
            guibg = colors.bg
        },
        modified_selected = {
            guifg = colors.fg,
            guibg = colors.bg
        }
    }
}

