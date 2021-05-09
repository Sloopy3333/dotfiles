# Imports

from os.path import expanduser   
from libqtile import qtile, layout
from libqtile.bar import Bar
from libqtile.lazy import lazy
from libqtile.layout import MonadTall, MonadWide, Max, Floating
from libqtile.config import Click, Drag, Group, Key, Screen, Match, KeyChord
from libqtile import widget

# user variables

mod = "mod4"
terminal = "st"
terminal_alt = "xterm"
browser = "librewolf"
browser_alt = "librewolf -p Logins"
filemanager = "st -e lf"
filemanager_alt = "pcmanfm"
ide = "emacs"
email = "st -e neomutt"
musicplayer = "st -e cmus"
rss = "st -e newsboat"

#colors

bar_colors = {
    "black"            "#282a36",  # black
    "red"              "#ff5555",  # red
    "green"            "#5af78e",  # green
    "yellow"           "#f1fa8c",  # yellow
    "blue"             "#57c7ff",  # blue
    "magenta"          "#ff6ac1",  # magenta
    "cyan"             "#8be9fd",  # cyan
    "white"            "#f1f1f0",  # white
    "orange"           "#ffb86c",  # orange
    "purple"           "#bd9cf9",  # purple
}

#settings

follow_mouse_focus = True
bring_front_click = True
cursor_warp = False
wmname = "Qtile"

#keybidings

keys = [

    # launch and kill programs
    Key([mod],                      "t",                        lazy.spawn(terminal),                                desc="Launch terminal"),
    Key([mod,"shift"],              "t",                        lazy.spawn(terminal_alt),                            desc="Launch alternative terminal"),
    Key([mod],                      "f",                        lazy.spawn(filemanager),                             desc="Launch filemanager"),
    Key([mod,"shift"],              "f",                        lazy.spawn(filemanager_alt),                         desc="Launch alternative filemanager",),
    Key([mod],                      "b",                        lazy.spawn(browser),                                 desc="Launch browser"),
    Key([mod,"shift"],              "b",                        lazy.spawn(browser_alt),                             desc="Launch alternative browser"),
    Key([mod,],                     "m",                        lazy.spawn(email),                                   desc="Launch neomutt"),
    Key([mod,"shift"],              "m",                        lazy.spawn(musicplayer),                             desc="Launch musicplayer"),
    Key([mod],                      "e",                        lazy.spawn(ide),                                     desc="Launch doom emacs"),
    Key([mod],                      "r",                        lazy.spawn(rss),                                     desc="Launch doom emacs"),
    Key([mod],                      "q",                        lazy.window.kill(),                                  desc="Kill focused window"),

    # qtile commands
    Key([mod],                      "c",                        lazy.restart(),                                      desc="Restart qtile"),
    Key([mod, "shift"],             "c",                        lazy.shutdown(),                                     desc="Shutdown qtile"),


    # shift window focus
    Key([mod],                      "j",                        lazy.layout.down(),                                  desc="Shift focus down"),
    Key([mod],                      "k",                        lazy.layout.up(),                                    desc="Shift focus up"),
    Key([mod],                      "h",                        lazy.layout.left(),                                  desc="Shift focus left"),
    Key([mod],                      "l",                        lazy.layout.right(),                                 desc="Shift focus right"),
    Key([mod],                      "Tab",                      lazy.screen.toggle_group(),                          desc="Toggle prev workspace"),

    # move windows
    Key([mod, "shift"],             "j",                        lazy.layout.shuffle_down(),                          desc="Move focused window down"),
    Key([mod, "shift"],             "k",                        lazy.layout.shuffle_up(),                            desc="Move focused window up"),
    Key([mod, "shift"],             "h",                        lazy.layout.shuffle_left(),                          desc="Move focused window left"),
    Key([mod, "shift"],             "l",                        lazy.layout.shuffle_right(),                         desc="Move focused window right"),
    Key([mod, "control"],           "Return",                   lazy.layout.flip(),                                  desc="Flip windows"),

    # resize windows
    Key([mod, "control"],           "h",                        lazy.layout.shrink(),                                desc="Increase Master window size"),
    Key([mod, "control"],           "l",                        lazy.layout.grow(),                                  desc="Decrease Master window size"),
    Key([mod, "control"],           "m",                        lazy.layout.maximize(),                              desc="Mazimize Master window size"),
    Key([mod, "control"],           "r",                        lazy.layout.reset(),                                 desc="Reset window size"),

    # layout modifires
    Key([mod],                      "n",                        lazy.next_layout(),                                  desc="Toggle next layout"),
    Key([mod],                      "p",                        lazy.prev_layout(),                                  desc="Toggle prev layout"),
    Key([mod, "control"],           "f",                        lazy.window.toggle_fullscreen(),                     desc="Toggle Full Screen"),
    Key([mod, "control"],           "t",                        lazy.window.toggle_floating(),                       desc="Toggle Full Screen"),

    # screenshots
    Key( [mod],                     "Print",                    lazy.spawn(expanduser("~/scripts/sc")),              desc="Take full screen shot"),
    Key( [mod, "shift"],            "Print",                    lazy.spawn(expanduser("~/scripts/sc -s")),           desc="Take screenshot of selected area"),
    Key( [mod, "control"],          "Print",                    lazy.spawn(expanduser("~/scripts/sc -cs")),          desc="Cpoy selected area to clipboard"),

    # run prompts and menu           return
    Key([mod],                      "space",                    lazy.spawn("dmenu_run"),                             desc="run dmenu"),
    Key([mod],                      "x",                        lazy.spawn(expanduser('~/.config//xmenu/xmenu.sh')), desc="Run xmenu"),

    # brightness
    Key([],                         "XF86MonBrightnessUp",      lazy.spawn("xbacklight -inc +5"),                    desc="Increase backlight by 5%"),
    Key([mod],                      "XF86MonBrightnessUp",      lazy.spawn("xbacklight -inc +15"),                   desc="Increase backlight by 15%"),
    Key([],                         "XF86MonBrightnessDown",    lazy.spawn("xbacklight -dec +5"),                    desc="Decrease backlight by 5%"),
    Key([mod],                      "XF86MonBrightnessDown",    lazy.spawn("xbacklight -dec +15"),                   desc="Decrease backlight by 15%"),

    # Volume
    Key([],                         "XF86AudioMute",            lazy.spawn("amixer sset Master toggle"),             desc="Toggle mute"),
    Key([],                         "XF86AudioRaiseVolume",     lazy.spawn("amixer sset Master 5%+"),                desc="Increase volume by 5%"),
    Key([mod],                      "XF86AudioRaiseVolume",     lazy.spawn("amixer sset Master 15%+"),               desc="Increase volume by 15%"),
    Key([],                         "XF86AudioLowerVolume",     lazy.spawn("amixer sset Master 5%-"),                desc="Decrease volume by 5%"),
    Key([mod],                      "XF86AudioLowerVolume",     lazy.spawn("amixer sset Master 15%-"),               desc="Decrease volume by 15%"),

    KeyChord([mod],  "d", [
             Key([], "s", lazy.spawn(expanduser("~/scripts/dpower"))),
             Key([], "p", lazy.spawn(expanduser("~/scripts/dpass"))),
             Key([], "m", lazy.spawn(expanduser("~/scripts/dman"))),
             Key([], "k", lazy.spawn(expanduser("~/scripts/dkill"))),
             Key([], "c", lazy.spawn(expanduser("~/scripts/dcol"))),
         ])

]

groups = [Group(i) for i in "123456789"]
for i in groups:
    keys.extend(
        [
            # mod1 + number of group = switch to group
            Key( [mod], i.name, lazy.group[i.name].toscreen(), desc=f"Switch to group {i.name}"),
            # mod1 + shift + number of group = switch to & move focused window to group
            Key( [mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True), desc=f"Switch to & move focused window to group {i.name}"),
        ]
    )

#mouse

mouse = [
    Drag( [mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position(),),
    Drag( [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

#layout

my_layout = {"border_focus"bar_colors["magenta"],
            "border_width"2,
            "margin"5,
            "single_border_width"0,
            "single_margin"5,}

layouts = [
    MonadTall(**my_layout, name="Tall",),
    Max(**my_layout, name="Full"),
    MonadWide(**my_layout, name="wide"),
]

floating_layout = Floating(**my_layout, name = "Float",
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(title='mpv'),
        Match(title='gimp'),
        Match(title='virt-manager'),
        Match(title='mpv'),
    ]
)

groups = [
    Group("1"),
    Group("2"),
    Group("3"),
    Group("4"),
    Group("5"),
    Group("6"),
    Group("7"),
    Group("8"),
    Group("9"),
]

#widgets

widget_defaults = dict(
    font="Hack Nerd Font Bold",
    fontsize=14,
    padding=4,
)

#screens = []
screens = [
    Screen(
        top=Bar(
            [
                widget.TextBox(text="",
                    fontsize=18,
                    foreground=bar_colors["blue"],
                    background=bar_colors["black"],
                    mouse_callbacks = {"Button1":lambdaqtile.cmd_spawn(expanduser("~/.config/xmenu/xmenu.sh"))},
                    padding = 5,
                    ),
                widget.Sep(
                    linewidth=10,
                    foreground=bar_colors["black"],
                    background=bar_colors["black"],
                    size_percent=100,
                    padding = 0,
                ),
                widget.GroupBox(
                    margin_y=5,
                    padding_y=5,
                    padding_x=3,
                    borderwidth=3,
                    highlight_method="line",
                    background=bar_colors["black"],
                    block_highlight_text_color=bar_colors["green"],
                    highlight_color=bar_colors["black"],
                    this_current_screen_border=bar_colors["purple"],
                    active=bar_colors["yellow"],
                    inactive=bar_colors["purple"],
                    markup=True,
                    center_aligned=True,
                    disable_drag=True,
                ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 0,
                    ),
                widget.CurrentLayout(
                    foreground=bar_colors["magenta"],
                    background=bar_colors["black"],
                    padding = 4,
                ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 0,
                    ),
                widget.WindowName(
                    foreground=bar_colors["blue"],
                    background=bar_colors["black"],
                    padding = 4,
                    ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 2,
                    ),
                widget.TextBox(text="",
                    foreground=bar_colors["magenta"],
                    background=bar_colors["black"],
                    padding = 8,
                    ),
                widget.CPU(
                    format = '{load_percent}% {freq_current}GHz',
                    foreground = bar_colors["magenta"],
                    background = bar_colors["black"],
                    update_interval = 10,
                    padding = 4,
                    ),
                widget.ThermalSensor(
                    foreground = bar_colors["magenta"],
                    background = bar_colors["black"],
                    update_interval = 10,
                    padding = 4,
                        ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 4,
                    ),
                widget.TextBox(text="",
                    foreground=bar_colors["yellow"],
                    background=bar_colors["black"],
                    padding = 6,
                    ),
                widget.Memory(
                    format = '{MemUsed} MB',
                    measure_mem = 'M',
                    foreground = bar_colors["yellow"],
                    background = bar_colors["black"],
                    update_interval = 10,
                    padding = 4,
                    ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 4,
                    ),
                widget.TextBox(text="",
                    foreground=bar_colors["cyan"],
                    background=bar_colors["black"],
                    padding = 8,
                    ),
                widget.Wlan(
                    format = '{essid} ({quality}/70)',
                    interface = 'wlp0s20f3',
                    disconnected_message = 'disconected',
                    foreground = bar_colors["blue"],
                    background = bar_colors["black"],
                    update_interval = 10,
                    padding = 4,
                    mouse_callbacks={"Button1":lambdaqtile.cmd_spawn("st -e nmtui"),
                                     "Button3":lambdaqtile.cmd_spawn("st -e nmcli d wifi list --rescan yes &")}
                    ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 4,
                    ),
                widget.Battery(
                    format = '{char} {percent:2.0%} ({hour:d}:{min:02d} {watt:.2f} W) ',
                    charge_char = '',
                    discharge_char = '',
                    foreground=bar_colors["green"],
                    background=bar_colors["black"],
                    padding = 4,
                    update_interval = 5
                        ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 4,
                    ),
                widget.TextBox(text="",
                    foreground=bar_colors["purple"],
                    background=bar_colors["black"],
                    padding = 6,
                    ),
                widget.Backlight(
                    format = '{percent:0.0%}',
                    backlight_name = 'intel_backlight',
                    brightness_file = 'actual_brightness',
                    max_brightness_file= 'max_brightness',
                    foreground=bar_colors["purple"],
                    background=bar_colors["black"],
                    padding = 4,
                    update_interval = 5,
                    mouse_callbacks={"Button1":lambdaqtile.cmd_spawn("xbacklight -inc 5"),
                                     "Button3":lambdaqtile.cmd_spawn("xbacklight -dec 5"),
                                     "Button2":lambdaqtile.cmd_spawn("xbacklight -set 5")}
                ),
                widget.TextBox(text="",
                    foreground=bar_colors["purple"],
                    background=bar_colors["black"],
                    padding = 6,
                    ),
                widget.Volume(
                    foreground=bar_colors["purple"],
                    background=bar_colors["black"],
                    padding = 4,
                    update_interval = 5,
                    mouse_callbacks={"Button1":lambdaqtile.cmd_spawn("amixer sset Master 5%+"),
                                     "Button3":lambdaqtile.cmd_spawn("amixer sset Master 5%-"),
                                     "Button2":lambdaqtile.cmd_spawn("amixer sset Master toggle")}
                ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 4,
                    ),
                widget.TextBox(text="",
                    foreground=bar_colors["cyan"],
                    background=bar_colors["black"],
                    padding = 4,
                    ),
                widget.Clock(
                    format = '%a %b %d %l:%M %p',
                    update_interval=60,
                    foreground=bar_colors["blue"],
                    background=bar_colors["black"],
                    padding = 8,
                ),
                widget.Systray(
                    background=bar_colors["black"],
                    padding = 4,
                        )
            ],
            size=22,
            opacity=1.0,
            margin=[0,0,0,0]
        ),
    ),
]

config.load_autoconfig(False)
# Which cookies to accept. With QtWebEngine, this setting also controls
# Type: String
# Valid values:
#   - all: Accept all cookies.
#   - no-3rdparty: Accept cookies from the same origin only. This is known to break some sites, such as GMail.
#   - no-unknown-3rdparty: Accept cookies from the same origin only, unless a cookie is already set for the domain. On QtWebEngine, this is the same as no-3rdparty.
#   - never: Don't accept cookies at all.
config.set("content.cookies.accept", "never", "chrome-devtools://*")
config.set("content.cookies.accept", "never", "devtools://*")

# User agent to send.  The following placeholders are defined:  *
# JavaScript requires a restart.
# Type: FormatString
config.set(
    "content.headers.user_agent",
    "Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}",
    "https://web.whatsapp.com/",
)
config.set(
    "content.headers.user_agent",
    "Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version} Edg/{upstream_browser_version}",
    "https://accounts.google.com/*",
)
config.set(
    "content.headers.user_agent",
    "Mozilla/5.0 ({os_info}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99 Safari/537.36",
    "https://*.slack.com/*",
)

# Load images automatically in web pages.
# Type: Bool
config.set("content.images", True, "chrome-devtools://*")
config.set("content.images", True, "devtools://*")

# Enable JavaScript.
# Type: Bool
config.set("content.javascript.enabled", True, "chrome-devtools://*")
config.set("content.javascript.enabled", True, "devtools://*")
config.set("content.javascript.enabled", True, "chrome://*/*")
config.set("content.javascript.enabled", True, "qute://*/*")

# preferences
c.url.start_pages = "/home/sam/.config/startpage/index.html"
c.tabs.show = "multiple"
c.url.searchengines = {
    "DEFAULT": "https://www.google.com/search?q={}",
    "dd": "https://duckduckgo.com/?q={}",
    "aw": "https://wiki.archlinux.org/?search={}",
    "go": "https://www.google.com/search?q={}",
    "re": "https://www.reddit.com/r/{}",
    "ub": "https://www.urbandictionary.com/define.php?term={}",
    "yt": "https://www.youtube.com/results?search_query={}",
}

palette = {
    'black'            '#282a36',
    'red'              '#ff5555',
    'green'            '#5af78e',
    'yellow'           '#f1fa8c',
    'blue'             '#57c7ff',
    'magenta'          '#ff6ac1',
    'cyan'             '#8be9fd',
    'white'            '#f1f1f0',
    'orange'           '#ffb86c',
    'purple'           '#bd9cf9',
    'pink'             '#ff79c6',
}

spacing = {
    'vertical' 5,
    'horizontal': 5
}

padding = {
    'top'   spacing['vertical'],
    'right' spacing['horizontal'],
    'bottom'spacing['vertical'],
    'left'  spacing['horizontal']
}

## Background color of the completion widget category headers.
c.colors.completion.category.bg = palette['black']

## Bottom border color of the completion widget category headers.
c.colors.completion.category.border.bottom = palette['black']

## Top border color of the completion widget category headers.
c.colors.completion.category.border.top = palette['black']

## Foreground color of completion widget category headers.
c.colors.completion.category.fg = palette['orange']

## Background color of the completion widget for even rows.
c.colors.completion.even.bg = palette['black']

## Background color of the completion widget for odd rows.
c.colors.completion.odd.bg = palette['black']

## Text color of the completion widget.
c.colors.completion.fg = palette['white']

## Background color of the selected completion item.
c.colors.completion.item.selected.bg = palette['purple']

## Bottom border color of the selected completion item.
c.colors.completion.item.selected.border.bottom = palette['black']

## Top border color of the completion widget category headers.
c.colors.completion.item.selected.border.top = palette['black']

## Foreground color of the selected completion item.
c.colors.completion.item.selected.fg = palette['black']

## Foreground color of the matched text in the completion.
c.colors.completion.match.fg = palette['green']

## Color of the scrollbar in completion view
c.colors.completion.scrollbar.bg = palette['black']

## Color of the scrollbar handle in completion view.
c.colors.completion.scrollbar.fg = palette['white']

## Background color for the download bar.
c.colors.downloads.bar.bg = palette['black']

## Background color for downloads with errors.
c.colors.downloads.error.bg = palette['black']

## Foreground color for downloads with errors.
c.colors.downloads.error.fg = palette['red']

## Color gradient stop for download backgrounds.
c.colors.downloads.stop.bg = palette['black']

## Color gradient interpolation system for download backgrounds.
## Type: ColorSystem
## Valid values:
##   - rgb: Interpolate in the RGB color system.
##   - hsv: Interpolate in the HSV color system.
##   - hsl: Interpolate in the HSL color system.
##   - none: Don't show a gradient.
c.colors.downloads.system.bg = 'none'

## Background color for hints. Note that you can use a `rgba(...)` value
## for transparency.
c.colors.hints.bg = palette['black']

## Font color for hints.
c.colors.hints.fg = palette['yellow']

## Hints
c.hints.border = '1px solid ' + palette['purple']

## Font color for the matched part of hints.
c.colors.hints.match.fg = palette['green']

## Background color of the keyhint widget.
c.colors.keyhint.bg = palette['black']

## Text color for the keyhint widget.
c.colors.keyhint.fg = palette['purple']

## Highlight color for keys to complete the current keychain.
c.colors.keyhint.suffix.fg = palette['purple']

## Background color of an error message.
c.colors.messages.error.bg = palette['black']

## Border color of an error message.
c.colors.messages.error.border = palette['black']

## Foreground color of an error message.
c.colors.messages.error.fg = palette['red']

## Background color of an info message.
c.colors.messages.info.bg = palette['black']

## Border color of an info message.
c.colors.messages.info.border = palette['black']

## Foreground color an info message.
c.colors.messages.info.fg = palette['black']

## Background color of a warning message.
c.colors.messages.warning.bg = palette['black']

## Border color of a warning message.
c.colors.messages.warning.border = palette['black']

## Foreground color a warning message.
c.colors.messages.warning.fg = palette['red']

## Background color for prompts.
c.colors.prompts.bg = palette['black']

# ## Border used around UI elements in prompts.
c.colors.prompts.border = '1px solid ' + palette['black']

## Foreground color for prompts.
c.colors.prompts.fg = palette['cyan']

## Background color for the selected item in filename prompts.
c.colors.prompts.selected.bg = palette['purple']

## Background color of the statusbar in caret mode.
c.colors.statusbar.caret.bg = palette['black']

## Foreground color of the statusbar in caret mode.
c.colors.statusbar.caret.fg = palette['orange']
## Background color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.bg = palette['black']

## Foreground color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.fg = palette['orange']

## Background color of the statusbar in command mode.
c.colors.statusbar.command.bg = palette['black']

## Foreground color of the statusbar in command mode.
c.colors.statusbar.command.fg = palette['pink']

## Background color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.bg = palette['black']

## Foreground color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.fg = palette['white']

## Background color of the statusbar in insert mode.
c.colors.statusbar.insert.bg = palette['red']

## Foreground color of the statusbar in insert mode.
c.colors.statusbar.insert.fg = palette['red']

## Background color of the statusbar.
c.colors.statusbar.normal.bg = palette['black']

## Foreground color of the statusbar.
c.colors.statusbar.normal.fg = palette['white']

## Background color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.bg = palette['black']

## Foreground color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.fg = palette['orange']

## Background color of the statusbar in private browsing mode.
c.colors.statusbar.private.bg = palette['black']

## Foreground color of the statusbar in private browsing mode.
c.colors.statusbar.private.fg = palette['white']

## Background color of the progress bar.
c.colors.statusbar.progress.bg = palette['black']

## Foreground color of the URL in the statusbar on error.
c.colors.statusbar.url.error.fg = palette['red']

## Default foreground color of the URL in the statusbar.
c.colors.statusbar.url.fg = palette['white']

## Foreground color of the URL in the statusbar for hovered links.
c.colors.statusbar.url.hover.fg = palette['cyan']

## Foreground color of the URL in the statusbar on successful load
c.colors.statusbar.url.success.http.fg = palette['green']

## Foreground color of the URL in the statusbar on successful load
c.colors.statusbar.url.success.https.fg = palette['green']

## Foreground color of the URL in the statusbar when there's a warning.
c.colors.statusbar.url.warn.fg = palette['yellow']

## Status bar padding
c.statusbar.padding = padding

## Background color of the tab bar.
## Type: QtColor
c.colors.tabs.bar.bg = palette['black']

## Background color of unselected even tabs.
## Type: QtColor
c.colors.tabs.even.bg = palette['black']

## Foreground color of unselected even tabs.
## Type: QtColor
c.colors.tabs.even.fg = palette['white']

## Color for the tab indicator on errors.
## Type: QtColor
c.colors.tabs.indicator.error = palette['red']

## Color gradient start for the tab indicator.
## Type: QtColor
c.colors.tabs.indicator.start = palette['orange']

## Color gradient end for the tab indicator.
## Type: QtColor
c.colors.tabs.indicator.stop = palette['green']

## Color gradient interpolation system for the tab indicator.
## Type: ColorSystem
## Valid values:
##   - rgb: Interpolate in the RGB color system.
##   - hsv: Interpolate in the HSV color system.
##   - hsl: Interpolate in the HSL color system.
##   - none: Don't show a gradient.
c.colors.tabs.indicator.system = 'none'

## Background color of unselected odd tabs.
## Type: QtColor
c.colors.tabs.odd.bg = palette['black']

## Foreground color of unselected odd tabs.
## Type: QtColor
c.colors.tabs.odd.fg = palette['white']

# ## Background color of selected even tabs.
# ## Type: QtColor
c.colors.tabs.selected.even.bg = palette['black']

# ## Foreground color of selected even tabs.
# ## Type: QtColor
c.colors.tabs.selected.even.fg = palette['green']

# ## Background color of selected odd tabs.
# ## Type: QtColor
c.colors.tabs.selected.odd.bg = palette['black']

# ## Foreground color of selected odd tabs.
# ## Type: QtColor
c.colors.tabs.selected.odd.fg = palette['green']

## Tab padding
c.tabs.padding = padding
c.tabs.indicator.width = 1
c.tabs.favicons.scale = 1

c.aliases['ZZ'] = 'quit --save'
# Bindings for normal mode
c.bindings.commands = {
    "normal": {
        "xb": "config-cycle statusbar.show always never",
        "xt": "config-cycle tabs.show always never",
        "xx": "config-cycle statusbar.show always never ;; config-cycle tabs.show always never",
    }
}
