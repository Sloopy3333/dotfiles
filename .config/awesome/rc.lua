-- If LuaRocks is installed, make 3ure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
-- pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_configuration_dir() .. "/themes/solarized.lua")


-- User variables
modkey = "Mod4"
terminal       = "alacritty"
terminalalt    = "emacsclient -c -a '' --eval '(eshell)'"
browser        = "firefox"
browseralt     = "chromium --profile-directory='Profile 2'"
filemanager    = "emacsclient -c -a '' --eval '(dired nil)'"
filemanageralt = "pcmanfm"
editor         = "emacsclient -c -a emacs"
email          = "thunderbird"
rss            = "emacsclient -c -a '' --eval '(elfeed)'"
editor_cmd = "emacsclient -c -a emacs"


-- Layouts
awful.layout.layouts = {
    awful.layout.suit.max,
    awful.layout.suit.tile,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.floating
}

-- Menu
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

mypowermenu = {
   {"sleep", "systemctl suspend"},
   {"restart", "systemctl restart"},
   {"shutdown","systemctl poweroff"},
}


mymainmenu = awful.menu({ items = { { "Awesome", myawesomemenu},
                                    { "Power", mypowermenu },
                                    { "Terminal", terminal }
                                  }
                        })

-- Menubar configuration
menubar.utils.terminal = terminal

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = gears.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  c:emit_signal(
                                                      "request::activate",
                                                      "tasklist",
                                                      {raise = true}
                                                  )
                                              end
                                          end),
                     awful.button({ }, 3, function()
                                              awful.menu.client_list({ theme = { width = 250 } })
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- smart borders
screen.connect_signal("arrange", function (s)
    local only_one = #s.tiled_clients == 1
    local is_full = awful.layout.get (s) == awful.layout.suit.max
    for _, c in pairs(s.clients) do
       if only_one and not c.floating or c.maximized or c.fullscreen or is_full then
            c.border_width = 0
        else
            c.border_width = beautiful.border_width
        end
    end
end)

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
--screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    --set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9"}, s, awful.layout.layouts[1])

    -- Layout widget
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Tags widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons
    }

    -- Tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.focused,
        buttons = tasklist_buttons
    }

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "bottom", height = 20,screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            s.mytaglist,
            wibox.widget.textbox(" | "),
            s.mylayoutbox,
            wibox.widget.textbox(" | "),
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            wibox.widget.textbox(" | "),
            awful.widget.watch('bash -c "~/scripts/bar/cpu.sh"', 10),
            wibox.widget.textbox(" | "),
            awful.widget.watch('bash -c "~/scripts/bar/memory.sh"', 10),
            wibox.widget.textbox(" | "),
            awful.widget.watch('bash -c "~/scripts/bar/battery.sh"', 10),
            wibox.widget.textbox(" | "),
            awful.widget.watch('bash -c "~/scripts/bar/clock.sh"', 60),
            wibox.widget.textbox(" | "),
            wibox.widget.systray(),
        },
    }
end)

root.buttons(gears.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))

-- Key bindings
clientkeys = gears.table.join(
   -- kill
    awful.key({ modkey,          }, "q", function (c) c:kill() end, {description = "kill program", group = "client"}),

    -- fulscteen, toggle flaot
    awful.key({ modkey, "Control"}, "f", function (c) c.fullscreen = not c.fullscreen c:raise() end, {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Control"}, "t",  awful.client.floating.toggle, {description = "toggle floating", group = "client"}),

    -- switch client focus
    awful.key({ modkey,          }, "j", function (c) awful.client.focus.byidx(1) end, {description = "focus next window up", group = "client"}),
    awful.key({ modkey,          }, "k", function (c) awful.client.focus.byidx(-1) end, {description = "focus next window up", group = "client"}),
    awful.key({ modkey,          }, "l", function (c) awful.client.focus.global_bydirection("right") c:lower() end, {description = "focus next window right", group = "client"}),
    awful.key({ modkey,          }, "h", function (c) awful.client.focus.global_bydirection("left") c:lower() end, {description = "focus next window left", group = "client"}),

    -- move client
    awful.key({ modkey, "Shift"  }, "h", function (c) awful.client.swap.global_bydirection("left") c:raise() end, {description = "swap with left client", group = "client"}),
    awful.key({ modkey, "Shift"  }, "l", function (c) awful.client.swap.global_bydirection("right") c:raise() end, {description = "swap with right client", group = "client"}),
    awful.key({ modkey, "Shift"  }, "j", function (c) awful.client.swap.global_bydirection("down") c:raise() end, {description = "swap with down client", group = "client"}),
    awful.key({ modkey, "Shift"  }, "k", function (c) awful.client.swap.global_bydirection("up") c:raise() end, {description = "swap with up client", group = "client"}),
    awful.key({ modkey, "Control"}, "Return", function (c) c:swap(awful.client.getmaster()) end, {description = "move to master", group = "client"}),
    awful.key({ modkey, "Shift"  }, "m",      function (c) c:move_to_screen() end, {description = "move to screen", group = "client"}),
    awful.key({ modkey, "Control"}, "m", function (c) c.maximized = not c.maximized c:raise() end , {description = "(un)maximize", group = "client"})
)

globalkeys = gears.table.join(
   -- launch programs
    awful.key({ modkey,          }, "t", function () awful.spawn(terminal) end, {description = "open terminal", group = "launcher"}),
    awful.key({ modkey, "Shift"  }, "t", function () awful.spawn(terminalalt) end, {description = "open eshell", group = "launcher"}),
    awful.key({ modkey,          }, "e", function () awful.spawn(editor) end, {description = "open emacs", group = "launcher"}),
    awful.key({ modkey,          }, "b", function () awful.spawn(browser) end, {description = "open firefox", group = "launcher"}),
    awful.key({ modkey,          }, "f", function () awful.spawn(filemanager) end, {description = "open dired", group = "launcher"}),
    awful.key({ modkey, "Shift"  }, "f", function () awful.spawn(filemanageralt) end, {description = "open pcmanfm", group = "launcher"}),
    awful.key({ modkey,          }, "m", function () awful.spawn(email) end, {description = "open thunderbird", group = "launcher"}),
    awful.key({ modkey,          }, "r", function () awful.spawn(rss) end, {description = "open elfeed", group = "launcher"}),
    awful.key({ modkey,          }, "p", function () awful.spawn("bash -c '~/scripts/dmenu/power.sh'") end, {description = "open elfeed", group = "launcher"}),

    -- prompts
    awful.key({ modkey,          }, "space", function () awful.spawn("rofi -show drun") end, {description = "open rofi run", group = "launcher"}),
    awful.key({ modkey,          }, "Tab", function () awful.spawn("rofi -show window") end, {description = "open rofi run", group = "launcher"}),
    awful.key({ modkey, "Shift"  }, "Tab", function () awful.spawn("rofi -show windowcd") end, {description = "open rofi run", group = "launcher"}),

    -- quit, restart
    awful.key({ modkey,          }, "c", awesome.restart, {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"  }, "c", awesome.quit, {description = "quit awesome", group = "awesome"}),

    -- resize windows
    awful.key({ modkey, "Control"}, "l", function () awful.tag.incmwfact( 0.05) end, {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey, "Control"}, "h", function () awful.tag.incmwfact(-0.05) end, {description = "decrease master width factor", group = "layout"}),

    -- layout modifires
    awful.key({ modkey,          }, "n", function () awful.layout.inc( 1) end, {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"  }, "n", function () awful.layout.inc(-1) end, {description = "select previous", group = "layout"}),

    --screens
    awful.key({ modkey,}, "m", function () awful.screen.focus_relative(1) end, {description = "focus the next screen", group = "screen"}),

    -- Backlight keys
    awful.key({                  }, "XF86MonBrightnessUp", function () awful.util.spawn("light -A 5") end),
    awful.key({                  }, "XF86MonBrightnessDown", function () awful.util.spawn("light -U 5") end),
    awful.key({modkey,           }, "Up", function() os.execute("light -A 5") end),
    awful.key({modkey,           }, "Down", function() os.execute("light -U 5") end),

    -- Volume
    awful.key({                  }, "XF86AudioMute", function () awful.util.spawn("pamixer -t") end),
    awful.key({                  }, "XF86AudioRaiseVolume", function () awful.util.spawn("pamixer -i 5") end),
    awful.key({                  }, "XF86AudioLowerVolume", function () awful.util.spawn("pamixer -d 5") end),
    awful.key({modkey,           }, "Right", function() os.execute("pamixer -i 5") end),
    awful.key({modkey,           }, "Left", function() os.execute("pamixer -d 5") end)


)


-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
         instance = {
            "DTA",  -- Firefox addon DownThemAll.
            "copyq",  -- Includes session name in class.
            "pinentry",
         },
         class = {
            "Arandr",
            "Blueman-manager",
            "Sxiv",
            "Tor Browser"},
         name = {
            "Event Tester",
         },
         role = {
            "AlarmWindow",
            "ConfigManager",
            "pop-up",
         }
    }, properties = { floating = true, optop = true}},

    -- Add titlebars to  dialogs
    { rule_any = {type = { "dialog" }
      }, properties = { titlebars_enabled = true }
    },

    -- Move some windows to specified tags
    { rule_any = {
         class = {
            "Steam",
            "Bottles",
            "heroic",
            "Lutris",
            "epicgameslauncher.exe"

         }
    }, properties = {tag = "8"}},
    ---}, properties = {tag = "8", fullscreen = false, ontop = true, floating = false,  titlebars_enabled = flase }},
    { rule_any = {
         class = {
            "steam_.*",
            "*.exe$",
         }
    }, properties = {tag = "9", fullscreen = true, ontop = true, floating = true,  titlebars_enabled = flase }}
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
