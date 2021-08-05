#Imports

from os.path import expanduser
from libqtile import layout
from libqtile.bar import Bar
from libqtile.lazy import lazy
from libqtile import hook
from libqtile.layout import MonadTall, MonadWide, Max, Floating
from libqtile.config import Click, Drag, Group, Key, Screen, Match, KeyChord
from libqtile import widget

# user variables

mod = "mod4"
terminal = "alacritty"
terminal_alt = "xterm"
browser = "brave"
browser_alt = "vimb"
filemanager = "emacsclient -c -a '' --eval '(dired nil)'"
filemanager_alt = "pcmanfm"
ide = "emacsclient -c -a emacs"
email = "emacsclient -c -a '' --eval '(mu4e)'"
musicplayer = "st -e ncmpcpp"
rss = "emacsclient -c -a '' --eval '(elfeed)'"


#colors
bar_colors = {
    "black": "#282a36",  # black
    "red": "#ff5555",  # red
    "green": "#5af78e",  # green
    "yellow": "#f1fa8c",  # yellow
    "blue": "#57c7ff",  # blue
    "magenta": "#ff6ac1",  # magenta
    "cyan": "#8be9fd",  # cyan
    "white": "#f1f1f0",  # white
    "orange": "#ffb86c",  # orange
    "purple": "#bd9cf9",  # purple
}

# settings
follow_mouse_focus = True
bring_front_click = True
cursor_warp = False
wmname = "Qtile"

# keybidings
keys = [

    # launch and kill programs
    Key([mod],                      "t",                        lazy.spawn(terminal),                                desc="Launch terminal"),
    Key([mod,"shift"] ,             "t" ,                       lazy.spawn(terminal_alt) ,                           desc="Launch alternative terminal"),
    Key([mod],                      "f",                        lazy.spawn(filemanager),                             desc="Launch filemanager"),
    Key([mod,"shift"],              "f",                        lazy.spawn(filemanager_alt),                         desc="Launch alternative filemanager",),
    Key([mod],                      "b",                        lazy.spawn(browser),                                 desc="Launch browser"),
    Key([mod,"shift"],              "b",                        lazy.spawn(browser_alt),                             desc="Launch alternative browser"),
    Key([mod,],                     "m",                        lazy.spawn(email),                                   desc="Launch neomutt"),
    Key([mod,"shift"],              "m",                        lazy.spawn(musicplayer),                             desc="Launch musicplayer"),
    Key([mod],                      "e",                        lazy.spawn(ide),                                     desc="Launch emacs"),
    Key([mod],                      "r",                        lazy.spawn(rss),                                     desc="Launch elfeed"),
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
    Key([mod],                      "space",                    lazy.spawn("rofi -show run"),                        desc="run dmenu"),

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
             Key([], "p", lazy.spawn(expanduser("~/scripts/bw"))),
             Key([], "m", lazy.spawn(expanduser("~/scripts/dman"))),
             Key([], "k", lazy.spawn(expanduser("~/scripts/dkill"))),
             Key([], "c", lazy.spawn(expanduser("~/scripts/dcol"))),
             Key([], "w", lazy.spawn(expanduser("~/scripts/dsearch"))),
         ]),
    KeyChord([mod],  "s", [
             Key([], "a", lazy.spawn(expanduser("~/scripts/dsearch archwiki"))),
             Key([], "p", lazy.spawn(expanduser("~/scripts/dsearch aur"))),
             Key([], "d", lazy.spawn(expanduser("~/scripts/dsearch duckduckgo"))),
             Key([], "g", lazy.spawn(expanduser("~/scripts/dsearch google"))),
             Key([], "r", lazy.spawn(expanduser("~/scripts/dsearch reddit"))),
             Key([], "s", lazy.spawn(expanduser("~/scripts/dsearch startpage"))),
             Key([], "u", lazy.spawn(expanduser("~/scripts/dsearch urbandictionary"))),
             Key([], "y", lazy.spawn(expanduser("~/scripts/dsearch youtube")))
    ])

]


groups = [Group(i) for i in "123456789"]
for i in groups:
    keys.extend(
        [
            # mod1 + number of group = switch to group
            Key( [mod], i.name, lazy.group[i.name].toscreen(), desc=f"Switch to group {i.name}"),
            # mod1 + shift + number of group = switch to & move focused window to group
            Key( [mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=False), desc=f"Move focused window to group {i.name}"),
        ]
    )

#mouse

mouse = [
    Drag( [mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position(),),
    Drag( [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

#layout

my_layout = {"border_focus" : bar_colors["magenta"],
            "border_width" : 2,
            "margin" : 0,
            "single_border_width" : 0,
            "single_margin" : 0,}

layouts = [
    MonadTall(**my_layout, name="Tall",),
    MonadWide(**my_layout, name="wide"),
    Max(**my_layout, name="Full"),
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

@hook.subscribe.client_new
def floating_dialogs(window):
    dialog = window.window.get_wm_type() == 'dialog'
    transient = window.window.get_wm_transient_for()
    if dialog or transient:
        window.floating = True

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
                    margin_y=4,
                    padding_y=0,
                    padding_x=0,
                    borderwidth=3,
                    highlight_method="line",
                    background=bar_colors["black"],
                    block_highlight_text_color=bar_colors["green"],
                    highlight_color=bar_colors["black"],
                    this_current_screen_border=bar_colors["purple"],
                    active=bar_colors["yellow"],
                    inactive=bar_colors["white"],
                    markup=True,
                    center_aligned=True,
                    disable_drag=True,
                ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 2,
                    ),
                widget.CurrentLayout(
                    foreground=bar_colors["magenta"],
                    background=bar_colors["black"],
                    padding = 2,
                ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 2,
                    ),
                widget.WindowName(
                    foreground=bar_colors["blue"],
                    background=bar_colors["black"],
                    padding = 2,
                    ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 0,
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
                    padding = 2,
                    ),
                widget.ThermalSensor(
                    foreground = bar_colors["magenta"],
                    background = bar_colors["black"],
                    update_interval = 10,
                    padding = 2,
                        ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 2,
                    ),
                widget.TextBox(text="",
                    foreground=bar_colors["yellow"],
                    background=bar_colors["black"],
                    padding = 6,
                    ),
                widget.Memory(
                    format = '{MemUsed: .0f}{mm}',
                    measure_mem = 'M',
                    foreground = bar_colors["yellow"],
                    background = bar_colors["black"],
                    update_interval = 10,
                    padding = 2,
                    ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 2,
                    ),
                widget.Battery(
                    format = '{char} {percent:2.0%} ({hour:d}:{min:02d} {watt:.2f} W)',
                    charge_char = '',
                    discharge_char = '',
                    foreground=bar_colors["green"],
                    background=bar_colors["black"],
                    padding = 2,
                    update_interval = 5
                        ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 2,
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
                    padding = 2,
                    update_interval = 5,
                ),
                widget.TextBox(text="",
                    foreground=bar_colors["purple"],
                    background=bar_colors["black"],
                    padding = 2,
                    ),
                widget.Volume(
                    foreground=bar_colors["purple"],
                    background=bar_colors["black"],
                    padding = 2,
                    update_interval = 5,
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
                    padding = 2,
                ),
                widget.TextBox(text="|",
                    background=bar_colors["black"],
                    padding = 2,
                    ),
                widget.Systray(
                    background=bar_colors["black"],
                    padding = 2,
                        )
            ],
            size=22,
            opacity=1.0,
            margin=[0,0,0,0]
        ),
    ),
]
