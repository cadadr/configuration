-- rc.lua --- awesomewm setup

-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

local gears = require("gears") -- Standard awesome library
local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox") -- Widget and layout library
local beautiful = require("beautiful") -- Theme handling library
local naughty = require("naughty") -- Notification library
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")

-- Load Debian menu entries
local debian = require("debian.menu")
local has_fdo, freedesktop = pcall(require, "freedesktop")

--- Error handling:
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
   awesome.connect_signal(
      "debug::error",
      function (err)
         -- Make sure we don't go into an endless error loop
         if in_error then return end
         in_error = true

         naughty.notify({ preset = naughty.config.presets.critical,
                          title = "Oops, an error happened!",
                          text = tostring(err) })
         in_error = false
   end)
end


--- Variable definitions:
-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_themes_dir() .. "gtk/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "x-terminal-emulator"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
   awful.layout.suit.tile,
   awful.layout.suit.floating,
   awful.layout.suit.spiral,
   awful.layout.suit.max,
   awful.layout.suit.magnifier,
}


--- Menu:
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

local menu_awesome = { "awesome", myawesomemenu, beautiful.awesome_icon }
local menu_terminal = { "open terminal", terminal }

if has_fdo then
   mymainmenu = freedesktop.menu.build({
         before = { menu_awesome },
         after =  { menu_terminal }
   })
else
   mymainmenu = awful.menu({
         items = {
            menu_awesome,
            { "Debian", debian.menu.Debian_menu.Debian },
            menu_terminal,
         }
   })
end


mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it


-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

--- Wibar:
-- Create a textclock widget
mytextclock = wibox.widget.textclock()
mytextclock.align = "center"

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

awful.screen.connect_for_each_screen(function(s)
      -- Each screen has its own tag table.
      awful.tag({ "1/wrk", "2/www", "3", "4" }, s, awful.layout.layouts[1])

      -- Create a promptbox for each screen
      s.mypromptbox = awful.widget.prompt()
      -- Create an imagebox widget which will contain an icon indicating which layout we're using.
      -- We need one layoutbox per screen.
      s.mylayoutbox = awful.widget.layoutbox(s)
      s.mylayoutbox:buttons(
         gears.table.join(
            awful.button({ }, 1, function () awful.layout.inc( 1) end),
            awful.button({ }, 3, function () awful.layout.inc(-1) end),
            awful.button({ }, 4, function () awful.layout.inc( 1) end),
            awful.button({ }, 5, function () awful.layout.inc(-1) end)))
      -- Create a taglist widget
      s.mytaglist = awful.widget.taglist {
         screen  = s,
         filter  = awful.widget.taglist.filter.all,
         buttons = taglist_buttons
      }

      -- Create the wibox
      s.mywibox = awful.wibar({ position = "top", screen = s })

      -- Add widgets to the wibox
      s.mywibox:setup {
         layout = wibox.layout.align.horizontal,
         {
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
         },
         mytextclock,
         {
            layout = wibox.layout.fixed.horizontal,
            mykeyboardlayout,
            wibox.widget.systray(),
            s.mylayoutbox,
         },
      }
end)


--- Mouse bindings:
root.buttons(gears.table.join(
                awful.button({ }, 3, function () mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
))


--- Key bindings:
globalkeys = gears.table.join(
   awful.key({modkey, "Control"}, "h", hotkeys_popup.show_help,
      {description="show help", group="awesome"}),

   awful.key({modkey}, "n", function () awful.client.focus.byidx( 1) end,
      {description = "focus next by index", group = "client"}),

   awful.key({modkey}, "p", function () awful.client.focus.byidx(-1) end,
      {description = "focus previous by index", group = "client"}),

   awful.key({modkey}, "m", function () mymainmenu:show() end,
      {description = "show main menu", group = "awesome"}),

   -- Standard program
   awful.key({modkey}, "Return", function () awful.spawn("emacsclient -c") end,
      {description = "new emacs client frame", group = "launcher"}),

   awful.key({modkey, "Shift"}, "Return", function () awful.spawn(terminal) end,
      {description = "open a terminal", group = "launcher"}),

   awful.key({modkey, "Control"}, "w",
      function () awful.spawn("ffprofile.sh") end,
      {description = "open a terminal", group = "launcher"}),

   awful.key({modkey}, "g", awesome.restart,
      {description = "reload awesome", group = "awesome"}),

   awful.key({modkey, "Control"}, "b", awesome.quit,
      {description = "quit awesome", group = "awesome"}),

   -- Prompt
   awful.key({"Shift"}, "F12",
      function () awful.screen.focused().mypromptbox:run() end,
      {description = "run prompt", group = "launcher"}),

   awful.key({ modkey }, "x",
      function ()
         awful.prompt.run {
            prompt       = "Run Lua code: ",
            textbox      = awful.screen.focused().mypromptbox.widget,
            exe_callback = awful.util.eval,
            history_path = awful.util.get_cache_dir() .. "/history_eval"
         }
      end,
      {description = "lua execute prompt", group = "awesome"}),

   -- Menubar
   awful.key({}, "F12", function() menubar.show() end,
      {description = "show the menubar", group = "launcher"})
)

clientkeys = gears.table.join(
   awful.key({}, "F11",
      function (c)
         c.fullscreen = not c.fullscreen
         c:raise()
      end,
      {description = "toggle fullscreen", group = "client"}),

   awful.key({modkey}, "k", function (c) c:kill() end,
      {description = "close", group = "client"}),

   awful.key({modkey}, ";", awful.client.floating.toggle,
      {description = "toggle floating", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 4 do
   globalkeys =
      gears.table.join(
         globalkeys,
         -- View tag only.
         awful.key({}, "F" .. i,
            function ()
               local screen = awful.screen.focused()
               local tag = screen.tags[i]
               if tag then
                  tag:view_only()
               end
            end,
            {description = "view tag #"..i, group = "tag"}),
         -- Toggle tag display.
         awful.key({modkey}, "F" .. i,
            function ()
               local screen = awful.screen.focused()
               local tag = screen.tags[i]
               if tag then
                  awful.tag.viewtoggle(tag)
               end
            end,
            {description = "toggle tag #" .. i, group = "tag"}),
         -- Move client to tag.
         awful.key({"Shift" }, "F" .. i,
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
         awful.key({"Control"}, "F" .. i,
            function ()
               if client.focus then
                  local tag = client.focus.screen.tags[i]
                  if tag then
                     client.focus:toggle_tag(tag)
                  end
               end
            end,
            {description = "toggle focused client on tag #" .. i, group = "tag"}))
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


--- Rules:
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

   { rule_any = { instance = {"pinentry"} },
     properties = { floating = true } },

   { rule_any = {type = { "normal", "dialog" } },
     properties = { titlebars_enabled = true } },

   { rule = { class = "Firefox" },
     properties = { screen = 1, tag = "2/www" } },
}


--- Signals:
-- Signal function to execute when a new client appears.
client.connect_signal(
   "manage",
   function (c)
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
client.connect_signal(
   "request::titlebars",
   function(c)
      -- buttons for the titlebar
      local buttons = gears.table.join(
         awful.button({ }, 1, function()
               c:emit_signal("request::activate", "titlebar", {raise = true})
               awful.mouse.client.move(c)
         end),
         awful.button({ }, 3, function()
               c:emit_signal("request::activate", "titlebar", {raise = true})
               awful.mouse.client.resize(c)
      end))

      awful.titlebar(c) : setup
      {
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
            layout = wibox.layout.fixed.horizontal()
         },
         layout = wibox.layout.align.horizontal
      }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal(
   "mouse::enter",
   function(c)
      c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal(
   "focus",
   function(c)
      c.border_color = beautiful.border_focus
end)

client.connect_signal(
   "unfocus",
   function(c)
      c.border_color = beautiful.border_normal
end)
