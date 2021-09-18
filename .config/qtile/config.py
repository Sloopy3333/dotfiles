#lacImports
from libqtile import qtile
from os.path import expanduser
from libqtile import layout
from libqtile.bar import Bar
from libqtile.lazy import lazy
from libqtile import hook
from libqtile.layout import MonadTall, MonadWide, Max, Floating
from libqtile.config import Click, Drag, Group, Key, Screen, Match, KeyChord, ScratchPad, DropDown
from libqtile import widget

# color schemes
my_gruvbox_dark = {
    "background"  : "#32302f",
    "foreground"  : "#ebdbb2",
    "gray"        : "#928374",
    "yellow"      : "#fabd2f",
    "orange"      : "#fe8019",
    "red"         : "#fb4934",
    "purple"      : "#d3869b",
    "blue"        : "#83a598",
    "cyan"        : "#8ec07c",
    "green"       : "#b8bb26"
}

my_gruvbox_light = {
    "background"  : "#fbf1c7",
    "foreground"  : "#3c3836",
    "gray"        : "#7c6f64",
    "yellow"      : "#b57614",
    "orange"      : "#af3a03",
    "red"         : "#9d0006",
    "purple"      : "#8f3f71",
    "blue"        : "#876678",
    "cyan"        : "#427b58",
    "green"       : "#79740e"
}

my_solarized_dark = {
    "background"  : "#002b36",
    "foreground"  : "#fdf6e3",
    "gray"        : "#073642",
    "yellow"      : "#b58900",
    "orange"      : "#cb4b16",
    "red"         : "#d30102",
    "purple"      : "#d33682",
    "blue"        : "#268bd2",
    "cyan"        : "#2aa198",
    "green"       : "#859900"
}

my_solarized_light = {
    "background"  : "#fdf6e3",
    "foreground"  : "#002b36",
    "gray"        : "#073642",
    "yellow"      : "#b58900",
    "orange"      : "#cb4b16",
    "red"         : "#d30102",
    "purple"      : "#d33682",
    "blue"        : "#268bd2",
    "cyan"        : "#2aa198",
    "green"       : "#859900"
}

# user variables
my_mod            = "mod4"
my_colors         = my_solarized_dark
my_terminal       = "st"
my_terminalalt    = "emacsclient -c -a '' --eval '(eshell)'"
my_browser        = "chromium --disable-software-rasterizer --profile-directory='Profile 1'"
my_browseralt     = "chromium --disable-software-rasterizer --profile-directory='Profile 2'"
my_browserwork     = "chromium --disable-software-rasterizer --profile-directory='Profile 2'"
my_filemanager    = "emacsclient -c -a '' --eval '(dired nil)'"
my_filemanageralt = "pcmanfm"
my_editor         = "emacsclient -c -a emacs"
my_email          = "emacsclient -c -a '' --eval '(mu4e)'"
my_rss            = "emacsclient -c -a '' --eval '(elfeed)'"

# settings
follow_mouse_focus = True
bring_front_click = True
cursor_warp = False
wmname = "Qtile"


# keybidings
keys = [
    # launch and kill programs
    Key([my_mod],                      "q",                        lazy.window.kill()),
    Key([my_mod],                      "t",                        lazy.spawn(my_terminal)),
    Key([my_mod,"shift"] ,             "t" ,                       lazy.spawn(my_terminalalt)),
    Key([my_mod],                      "f",                        lazy.spawn(my_filemanager)),
    Key([my_mod,"shift"],              "f",                        lazy.spawn(my_filemanageralt)),
    Key([my_mod],                      "b",                        lazy.spawn(my_browser)),
    Key([my_mod,"shift"],              "b",                        lazy.spawn(my_browseralt)),
    Key([my_mod,],                     "m",                        lazy.spawn(my_email)),
    Key([my_mod],                      "e",                        lazy.spawn(my_editor)),
    Key([my_mod],                      "r",                        lazy.spawn(my_rss)),

    # qtile commands
    Key([my_mod],                      "c",                        lazy.restart()),
    Key([my_mod, "shift"],             "c",                        lazy.shutdown()),

    # shift window focus
    Key([my_mod],                      "j",                        lazy.layout.down()),
    Key([my_mod],                      "k",                        lazy.layout.up()),
    Key([my_mod],                      "h",                        lazy.layout.left()),
    Key([my_mod],                      "l",                        lazy.layout.right()),

    # move windows
    Key([my_mod, "shift"],             "j",                        lazy.layout.shuffle_down()),
    Key([my_mod, "shift"],             "k",                        lazy.layout.shuffle_up()),
    Key([my_mod, "shift"],             "h",                        lazy.layout.shuffle_left()),
    Key([my_mod, "shift"],             "l",                        lazy.layout.shuffle_right()),
    Key([my_mod],                      "Return",                   lazy.layout.flip()),

    # resize windows
    Key([my_mod, "control"],           "j",                        lazy.layout.shrink()),
    Key([my_mod, "control"],           "k",                        lazy.layout.grow()),
    Key([my_mod, "control"],           "h",                        lazy.layout.shrink_main()),
    Key([my_mod, "control"],           "l",                        lazy.layout.grow_main()),
    Key([my_mod, "control"],           "m",                        lazy.layout.maximize()),
    Key([my_mod, "control"],           "n",                        lazy.layout.reset()),

    # layout my_modifires
    Key([my_mod],                      "n",                        lazy.next_layout()),
    Key([my_mod],                      "p",                        lazy.prev_layout()),
    Key([my_mod, "control"],           "f",                        lazy.hide_show_bar("all")),
    Key([my_mod, "control"],           "t",                        lazy.window.toggle_floating()),

    # screenshots
    Key( [my_mod],                     "Print",                    lazy.spawn(expanduser("sc"))),
    Key( [my_mod, "shift"],            "Print",                    lazy.spawn(expanduser("~/scripts/sc -s"))),
    Key( ["mod1",],                    "Print",                    lazy.spawn(expanduser("sc -r"))),
    Key( ["mod1", "shift"],            "Print",                    lazy.spawn(expanduser("~/scripts/sc -s -r"))),
    Key( ["mod1", "control"],          "Print",                    lazy.spawn(expanduser("~/scripts/sc -cs -r"))),

    # run prompts and menu
    Key([my_mod],                      "space",                    lazy.spawn("rofi -show run")),
    Key([my_mod],                      "Tab",                      lazy.spawn("rofi -show window")),
    Key([my_mod, "shift"],             "Tab",                      lazy.spawn("rofi -show windowcd")),

    # brightness
    Key([],                         "XF86MonBrightnessUp",        lazy.spawn("xbacklight -inc +1")),
    Key([my_mod],                   "XF86MonBrightnessUp",        lazy.spawn("xbacklight -inc +5")),
    Key([],                         "XF86MonBrightnessDown",      lazy.spawn("xbacklight -dec +1")),
    Key([my_mod],                   "XF86MonBrightnessDown",      lazy.spawn("xbacklight -dec +5")),

    # Volume
    Key([],                         "XF86AudioMute",              lazy.spawn("amixer sset Master toggle")),
    Key([],                         "XF86AudioRaiseVolume",       lazy.spawn("amixer sset Master 5%+")),
    Key([],                         "XF86AudioLowerVolume",       lazy.spawn("amixer sset Master 5%-")),

    # prompt submaps
    KeyChord([my_mod], "s", [
        Key([], "p", lazy.spawn(expanduser("~/scripts/dmenu_power.sh"))),
        Key([], "k", lazy.spawn(expanduser("~/scripts/dmenu_kill.sh"))),
        Key([], "m", lazy.spawn(expanduser("~/scripts/dmenu_man.sh")))
    ]),

    # scratchpad submaps
    KeyChord([my_mod], "d", [
        Key([], 'h',   lazy.group['scratchpad'].dropdown_toggle('htop')),
        Key([], 't',   lazy.group['scratchpad'].dropdown_toggle('term')),
    ])
]

groups = [Group(i) for i in "123456789"]
for i in groups:
    keys.extend(
        [
            # my_mod1 + number of group = switch to group
            Key( [my_mod], i.name, lazy.group[i.name].toscreen()),
            # my_mod1 + shift + number of group = switch to & move focused window to group
            Key( [my_mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=False)),
        ]
    )

#mouse
mouse = [
    Drag( [my_mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position(),),
    Drag( [my_mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([my_mod], "Button2", lazy.window.bring_to_front()),
]

# layouts
my_layout = {"border_focus" : my_colors["yellow"],
             "border_width" : 2,
             "margin" : 0,
             "single_border_width" : 0,
             "single_margin" : 0,}

layouts = [
    Max(**my_layout, name="Full"),
    MonadTall(**my_layout, name="Tall",),
]

# floating windows
floating_layout = Floating(**my_layout, name = "Float",
                           float_rules=[
                               *layout.Floating.default_float_rules,
                               Match(title='mpv'),
                               Match(title='gimp'),
                               Match(title='virt-manager'),
                               Match(title='mpv')])

@hook.subscribe.client_new
def floating_dialogs(window):
    dialog = window.window.get_wm_type() == 'dialog'
    transient = window.window.get_wm_transient_for()
    if dialog or transient:
        window.floating = True

# scratchpads
my_sctarchpads = ScratchPad("scratchpad", [
    DropDown("term", "st",
             x=0.05, y=0.05, width=0.9, height=0.9, opacity=1,
             on_focus_lost_hide=True),
    DropDown("htop", "st --hold -e htop",
             x=0.05, y=0.05, width=0.9, height=0.9, opacity=1,
             on_focus_lost_hide=True),
])

# groups
groups = [
    my_sctarchpads,
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
    font="IBM Plex Mono semibold",
    fontsize=13,
    padding=2,
    foreground=my_colors["foreground"],
    background=my_colors["background"],
    update_interval = 5,
)

screens = [
    Screen(
        bottom=Bar(
            [
                widget.GroupBox(
                    borderwidth=3,
                    highlight_method="line",
                    block_highlight_text_color=my_colors["green"],
                    highlight_color=my_colors["background"],
                    this_current_screen_border=my_colors["foreground"],
                    active=my_colors["yellow"],
                    inactive=my_colors["foreground"],
                    disable_drag=True,
                ),
                widget.TextBox(text="|",),
                widget.CurrentLayout(),
                widget.TextBox(text="|",),
                widget.WindowName(),
                widget.TextBox(text="|",),
                widget.CPU(format = 'CPU: {load_percent}% {freq_current}GHz'),
                widget.ThermalSensor(threshold=70, foreground_alert=my_colors["red"],foreground=my_colors["foreground"]),
                widget.TextBox(text="|",),
                widget.Memory(format = 'MEM: {MemUsed:.0f}{mm}', measure_mem = 'M'),
                widget.TextBox(text="|",),
                widget.Battery(
                    format = '{char} {percent:2.0%} ({hour:d}:{min:02d} {watt:.2f} W)',
                    charge_char = 'AC',
                    discharge_char = 'BAT',
                    low_percentage = 0.4,
                    low_foreground = my_colors["red"]
                ),
                widget.TextBox(text="|",),
                widget.Clock(
                    format = '%a %b %d %l:%M %p',
                    update_interval=60,
                ),
                widget.TextBox(text="|",),
                widget.Systray()
            ],
            size=20,
            opacity=1.0,
            margin=[0,0,0,0]
        ),
    ),
]
