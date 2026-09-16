-- Hyprland config, Lua format.
-- Refer to the wiki for more information.
-- https://wiki.hypr.land/Configuring/Start/

require("init")
require("style")
require("layout")
require("keys")

-- Per-machine config, kept out of the repo (see .gitignore):
--   monitors.lua  the monitor layout; nwg-displays still writes hyprlang
--                 (monitors.conf), so port its output here by hand
--   local.lua     anything else that only applies to this machine
--
-- Both are optional, hence the lookup instead of a plain require(). We also
-- load them by hand because Hyprland's require() swallows errors raised by the
-- file and hands back an empty table, so a typo would leave the machine
-- silently unconfigured.
local function require_local(name)
    local path = package.searchpath(name, package.path)
    if not path then return end

    local chunk, err = loadfile(path)
    if chunk then
        local ok, run_err = pcall(chunk)
        if ok then return end
        err = run_err
    end

    pcall(hl.notification.create, {
        text = "hyprland.lua: " .. tostring(err),
        timeout = 10000,
        color = "rgb(ff5555)",
    })
end

require_local("monitors")
require_local("local")

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
