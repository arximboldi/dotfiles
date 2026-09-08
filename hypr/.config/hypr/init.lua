-- Autostart necessary processes (like notifications daemons, status bars, etc.)
-- Or execute your favorite apps at launch like this:
-- https://wiki.hypr.land/Configuring/Core/Autostart/

hl.on("hyprland.start", function()
    hl.exec_cmd("dms run")
    hl.exec_cmd("~/usr/bin/startemacs")
    hl.exec_cmd("gsettings set org.gnome.desktop.wm.preferences button-layout 'appmenu:'")
    hl.exec_cmd("gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'")
    hl.exec_cmd("polkit-gnome-authentication-agent-1")
    hl.exec_cmd("mpdris2-rs -n")
    -- hl.exec_cmd("while true; do mpdris2-rs; sleep 0.1; done")

    -- custom more modular shell setup
    -- hl.exec_cmd("waybar")
    -- hl.exec_cmd("swaybg -i ~/pic/wallpaper/default.jpg")
    -- hl.exec_cmd("sunsetr")
    -- hl.exec_cmd("mako")
    -- hl.exec_cmd("nm-applet")
    -- hl.exec_cmd("swayosd-server")
    -- hl.exec_cmd("hypridle")

    -- for tablets
    -- hl.exec_cmd("otd-daemon")

    -- this seems to cause super-dot to open the prefs of the program the first time, weird
    -- hl.exec_cmd("bash -c 'while ! busctl --user --list | grep -q org.kde.StatusNotifierWatcher; do sleep 0.5; done; exec qsyncthingtray'")
end)

-- See https://wiki.hypr.land/Configuring/Core/Environment-variables/
-- hl.env("XDG_CURRENT_DESKTOP", "gnome")
hl.env("DE", "gnome") -- for xdg-open, can also set DESKTOP_SESSION
hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")
hl.env("GTK_CSD", "0")
hl.env("QT_QPA_PLATFORMTHEME", "qt6ct")
hl.env("_JAVA_AWT_WM_NONREPARENTING", "1")

-- hl.config({ misc = { suppress_portal_warnings = true } })
