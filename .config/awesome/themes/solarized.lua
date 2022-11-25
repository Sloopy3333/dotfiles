---------------------------------------
-- solarized dark theme by eric juma --
---------------------------------------

theme = {}
local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

theme.font          = "IBM Plex Mono Medium 11"

-- solarized colors
theme.base03   = "#002b36"
theme.base02   = "#073642"
theme.base01   = "#586e75"
theme.base00   = "#657b83"
theme.base0    = "#839496"
theme.base1    = "#93a1a1"
theme.base2    = "#eee8d5"
theme.base3    = "#fdf6e3"
theme.yellow   = "#b58900"
theme.orange   = "#cb4b16"
theme.red      = "#dc322f"
theme.magenta  = "#d33682"
theme.violet   = "#6c71c4"
theme.blue     = "#268bd2"
theme.cyan     = "#288198"
theme.green    = "#859900"
theme.oldfocus = "#2aa198"

-- normal colors
theme.bg_normal     = theme.base03
theme.fg_normal     = theme.base00
theme.border_normal = theme.base03

-- focused colors
theme.bg_focus      = theme.base03
theme.fg_focus      = theme.base3
theme.border_focus  = theme.oldfocus

-- urgent colors
theme.bg_urgent     = theme.red
theme.fg_urgent     = theme.base03

-- minimized colors
theme.bg_minimize   = theme.base01
theme.fg_minimize   = theme.base03

-- border colors
theme.border_width  = 2
theme.border_marked = theme.yellow

-- systray colors
theme.bg_systray    = theme.bg_normal

-- disable tasklist icons
theme.tasklist_disable_icon = true

-- menu size
theme.menu_height = 18
theme.menu_width  = 100

theme.layout_max = themes_path.."default/layouts/maxw.png"
theme.layout_tile = themes_path.."default/layouts/tilew.png"
theme.layout_floating  = themes_path.."default/layouts/floatingw.png"

-- You can use your own layout icons like this:

return theme
