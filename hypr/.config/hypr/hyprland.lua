-- Hyprland config, Lua format.
-- Refer to the wiki for more information.
-- https://wiki.hypr.land/Configuring/Start/

require("init")
require("style")
require("layout")
require("keys")

-- this is in gitignore
pcall(require, "monitors")
pcall(require, "local")

-- from dms
-- pcall(require, "dms.layout")

-- https://wiki.hypr.land/Configuring/Core/Config-options/
hl.config({
    misc = {
        font_family = "Iosevka Nerd Font",
        force_default_wallpaper = 0,     -- Set to 0 or 1 to disable the anime mascot wallpapers
        disable_hyprland_logo = true,    -- If true disables the random hyprland logo / anime girl background. :(
        focus_on_activate = true,
        anr_missed_pings = 5,
        enable_anr_dialog = true,
        disable_watchdog_warning = true,
    },

    xwayland = {
        force_zero_scaling = true,
    },
})

-- dms regenerates this one itself
pcall(require, "dms.cursor")
