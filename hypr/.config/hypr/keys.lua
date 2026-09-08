-- https://wiki.hypr.land/Configuring/Core/Config-options/
hl.config({
    input = {
        kb_layout = "us,es",
        kb_variant = "",
        kb_model = "",
        kb_options = "",
        kb_rules = "",

        follow_mouse = 1,

        sensitivity = 0, -- -1.0 - 1.0, 0 means no modification.

        touchpad = {
            natural_scroll = false,
        },
    },

    -- https://wiki.hypr.land/Configuring/Core/Binds/Gestures/
    gestures = {
        workspace_swipe_invert = false,
        workspace_swipe_cancel_ratio = 0.1,
        -- workspace_swipe_forever = true,
        workspace_swipe_create_new = true,
        -- workspace_swipe_min_speed_to_force = 10,
    },

    animations = {
        workspace_wraparound = true,
    },
})

hl.gesture({ fingers = 3, direction = "horizontal", action = "workspace" })

-- Example per-device config
-- See https://wiki.hypr.land/Configuring/Core/Devices/ for more
hl.device({
    name = "epic-mouse-v1",
    sensitivity = -0.5,
})

local mod = "SUPER" -- Sets "Windows" key as main modifier

-- defensive
hl.unbind("ALT + left")
hl.unbind("ALT + right")

-- Example binds, see https://wiki.hypr.land/Configuring/Core/Binds/ for more

-- quitting
-- hl.bind(mod .. " + Q", hl.dsp.exec_cmd("wlogout"))
hl.bind(mod .. " + Q", hl.dsp.exec_cmd("dms ipc call powermenu toggle"))
hl.bind(mod .. " + SHIFT + Q", hl.dsp.exit())

-- programs
hl.bind(mod .. " + M", hl.dsp.exec_cmd("~/usr/bin/getmail"))
hl.bind(mod .. " + SHIFT + M", hl.dsp.exec_cmd("~/usr/bin/email"))

hl.bind(mod .. " + backslash", hl.dsp.exec_cmd("~/usr/bin/hyprfocus org.gnome.Terminal || gnome-terminal"))
hl.bind(mod .. " + SHIFT + backslash", hl.dsp.exec_cmd("gnome-terminal"))

hl.bind(mod .. " + W", hl.dsp.exec_cmd("~/usr/bin/hyprfocus zen-beta || zen-beta"))
hl.bind(mod .. " + SHIFT + W", hl.dsp.exec_cmd("zen-beta"))

hl.bind(mod .. " + E", hl.dsp.exec_cmd("~/usr/bin/hyprfocus emacs || emacsclient -c -e '(ignore)'"))
hl.bind(mod .. " + SHIFT + E", hl.dsp.exec_cmd("emacsclient -c -e '(ignore)'"))

hl.bind(mod .. " + R", hl.dsp.exec_cmd("~/usr/bin/hyprmoji"))

hl.bind(mod .. " + N", hl.dsp.exec_cmd("nautilus"))
hl.bind(mod .. " + SHIFT + N", hl.dsp.exec_cmd("nautilus -w"))

hl.bind(mod .. " + B", hl.dsp.exec_cmd("~/usr/bin/hyprfocus dog.unix.cantata.Cantata || cantata"))
hl.bind(mod .. " + SHIFT + B", hl.dsp.exec_cmd("mpd-sima-gui"))

hl.bind(mod .. " + space", hl.dsp.exec_cmd("~/usr/bin/hyprlauncher"))
hl.bind(mod .. " + SHIFT + space", hl.dsp.exec_cmd("rofi -show file-browser-extended -file-browser-depth 4 -file-browser-no-descend"))

-- screenshots
hl.bind("SHIFT + print", hl.dsp.exec_cmd("hyprshot -m output"))
hl.bind(mod .. " + print", hl.dsp.exec_cmd("hyprshot -m window -m active"))
hl.bind("print", hl.dsp.exec_cmd("hyprshot -m region"))
hl.bind("ALT + print", hl.dsp.exec_cmd("kooha"))

hl.bind(mod .. " + C", hl.dsp.window.close())
hl.bind(mod .. " + O", hl.dsp.window.float())

-- switch layout
-- not nice, cuz doesn't work per workspace, we can look into this in the futre
-- https://github.com/zakk4223/hyprWorkspaceLayouts
hl.bind(mod .. " + 0", function()
    local current = hl.get_config("general.layout")
    hl.config({ ["general.layout"] = current == "dwindle" and "master" or "dwindle" })
end)

-- dwindle
hl.bind(mod .. " + P", hl.dsp.window.pseudo())
hl.bind(mod .. " + J", hl.dsp.layout("togglesplit"))

-- master
hl.bind(mod .. " + return", hl.dsp.layout("swapwithmaster master")) -- auto
hl.bind(mod .. " + J", hl.dsp.layout("swapnext"))
hl.bind(mod .. " + H", hl.dsp.layout("swapprev"))
hl.bind(mod .. " + L", hl.dsp.layout("mfact +0.05"))
hl.bind(mod .. " + K", hl.dsp.layout("mfact -0.05"))
hl.bind(mod .. " + equal", hl.dsp.layout("orientationcycle left top center"))
hl.bind(mod .. " + minus", hl.dsp.layout("orientationcycle center top left"))
hl.bind(mod .. " + P", hl.dsp.layout("addmaster"))
hl.bind(mod .. " + SHIFT + P", hl.dsp.layout("removemaster"))

-- window moving
hl.bind(mod .. " + F", hl.dsp.window.fullscreen())
hl.bind("ALT + tab", function()
    hl.dispatch(hl.dsp.window.cycle_next())
    hl.dispatch(hl.dsp.window.bring_to_top())
end)
hl.bind(mod .. " + tab", function()
    hl.dispatch(hl.dsp.layout("cyclenext"))
    hl.dispatch(hl.dsp.window.bring_to_top())
end)
hl.bind(mod .. " + SHIFT + tab", function()
    hl.dispatch(hl.dsp.layout("cycleprev"))
    hl.dispatch(hl.dsp.window.bring_to_top())
end)

-- Move focus with mod + arrow keys
hl.bind(mod .. " + left",  hl.dsp.focus({ direction = "left" }))
hl.bind(mod .. " + right", hl.dsp.focus({ direction = "right" }))
hl.bind(mod .. " + up",    hl.dsp.focus({ direction = "up" }))
hl.bind(mod .. " + down",  hl.dsp.focus({ direction = "down" }))

for i = 1, 9 do
    -- Switch workspaces with mod + [1-9]
    hl.bind(mod .. " + " .. i, hl.dsp.focus({ workspace = i }))
    -- move workspace to monitor
    hl.bind(mod .. " + CONTROL + " .. i, hl.dsp.focus({ workspace = i, on_current_monitor = true }))
    -- Move active window to a workspace with mod + SHIFT + [1-9]
    hl.bind(mod .. " + SHIFT + " .. i, hl.dsp.window.move({ workspace = i, follow = false }))
end

hl.bind(mod .. " + bracketleft",  hl.dsp.focus({ workspace = "e-1" }))
hl.bind(mod .. " + bracketright", hl.dsp.focus({ workspace = "e+1" }))

hl.bind(mod .. " + CONTROL + bracketleft",  hl.dsp.focus({ workspace = "e-1", on_current_monitor = true }))
hl.bind(mod .. " + CONTROL + bracketright", hl.dsp.focus({ workspace = "e+1", on_current_monitor = true }))

hl.bind(mod .. " + SHIFT + 0", hl.dsp.window.move({ workspace = 10, follow = false }))
hl.bind(mod .. " + SHIFT + bracketleft",  hl.dsp.window.move({ workspace = "-1" }))
hl.bind(mod .. " + SHIFT + bracketright", hl.dsp.window.move({ workspace = "+1" }))

-- Change monitor focus
hl.bind(mod .. " + A", hl.dsp.focus({ monitor = "-1" }))
hl.bind(mod .. " + S", hl.dsp.focus({ monitor = "+1" }))

-- Example special workspace (scratchpad)
-- hl.bind(mod .. " + D", hl.dsp.workspace.toggle_special("magic"))
-- hl.bind(mod .. " + SHIFT + D", hl.dsp.window.move({ workspace = "special:magic" }))

-- Scroll through existing workspaces with mod + scroll
hl.bind(mod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mod .. " + mouse_up",   hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mod + LMB/RMB and dragging
hl.bind(mod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mod .. " + SHIFT + mouse:272", hl.dsp.window.resize(), { mouse = true })
hl.bind(mod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- adapted from
-- https://github.com/AvengeMedia/DankMaterialShell/blob/143918bc5ec91a7d25643c17ec521664ec35bde1/core/internal/config/embedded/hypr-binds.conf
hl.bind("SUPER + comma", hl.dsp.exec_cmd("dms ipc call control-center toggle"))
hl.bind("SUPER + SHIFT + comma", hl.dsp.exec_cmd("dms ipc call settings focusOrToggle"))
hl.bind("SUPER + period", hl.dsp.exec_cmd("dms ipc call clipboard toggle"))
hl.bind("SUPER + SHIFT + period", hl.dsp.exec_cmd("dms ipc call processlist focusOrToggle"))
hl.bind("SUPER + slash", hl.dsp.exec_cmd("dms ipc call notifications toggle"))
hl.bind("SUPER + SHIFT + slash", hl.dsp.exec_cmd("dms ipc call notepad toggle"))

-- audio
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("dms ipc call audio increment 3"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("dms ipc call audio decrement 3"), { locked = true, repeating = true })
hl.bind("XF86AudioMute",    hl.dsp.exec_cmd("dms ipc call audio mute"),        { locked = true })
hl.bind("XF86AudioMicMute", hl.dsp.exec_cmd("dms ipc call audio micmute"),     { locked = true })
hl.bind("XF86AudioPause",   hl.dsp.exec_cmd("dms ipc call mpris playPause"),   { locked = true })
hl.bind("XF86AudioPlay",    hl.dsp.exec_cmd("dms ipc call mpris playPause"),   { locked = true })
hl.bind("XF86AudioPrev",    hl.dsp.exec_cmd("dms ipc call mpris previous"),    { locked = true })
hl.bind("XF86AudioNext",    hl.dsp.exec_cmd("dms ipc call mpris next"),        { locked = true })

-- brightness
hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd('dms ipc call brightness increment 5 ""'), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd('dms ipc call brightness decrement 5 ""'), { locked = true, repeating = true })

-- keyboard layout
hl.bind(mod .. " + ALT + space", hl.dsp.exec_cmd(
    "hyprctl switchxkblayout $(hyprctl devices -j | jq -r '.keyboards[] | select(.main == true) | .name') next"))

-- -- notifications
-- hl.bind(mod .. " + comma", hl.dsp.exec_cmd("makoctl dismiss"))
-- hl.bind(mod .. " + slash", hl.dsp.exec_cmd("makoctl dismiss -a"))
-- hl.bind(mod .. " + period", hl.dsp.exec_cmd("makoctl invoke || makoctl dismiss"))
-- hl.bind(mod .. " + SHIFT + comma", hl.dsp.exec_cmd("makoctl restore"))
-- hl.bind(mod .. " + SHIFT + slash", hl.dsp.exec_cmd("makoctl restore"))
-- hl.bind(mod .. " + SHIFT + period", hl.dsp.exec_cmd("makoctl restore"))
--
-- -- toggle language and display with swayosd
-- hl.bind(mod .. " + ALT + space", hl.dsp.exec_cmd("hyprctl switchxkblayout $(hyprctl devices -j | jq -r '.keyboards[] | select(.main == true) | .name') next && ~/usr/bin/hyprosd --custom-message=\"⌨  $(hyprctl devices -j | jq -r '.keyboards[] | select(.main == true) | .active_keymap')\""))
--
-- -- Laptop multimedia keys for volume and LCD brightness
-- hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("~/usr/bin/hyprosd --output-volume=raise"), { locked = true, repeating = true })
-- hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("~/usr/bin/hyprosd --output-volume=lower"), { locked = true, repeating = true })
-- hl.bind("XF86AudioMute", hl.dsp.exec_cmd("~/usr/bin/hyprosd --output-volume=mute-toggle"), { locked = true, repeating = true })
-- hl.bind("XF86AudioMicMute", hl.dsp.exec_cmd("~/usr/bin/hyprosd --input-volume=mute-toggle"), { locked = true, repeating = true })
-- hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd("~/usr/bin/hyprosd --brightness=raise"), { locked = true, repeating = true })
-- hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("~/usr/bin/hyprosd --brightness=lower"), { locked = true, repeating = true })
--
-- -- Requires playerctl
-- hl.bind("XF86AudioNext", hl.dsp.exec_cmd("~/usr/bin/hyprosd --playerctl=next"), { locked = true })
-- hl.bind("XF86AudioPause", hl.dsp.exec_cmd("~/usr/bin/hyprosd --playerctl=play-pause"), { locked = true })
-- hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("~/usr/bin/hyprosd --playerctl=play-pause"), { locked = true })
-- hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("~/usr/bin/hyprosd --playerctl=previous"), { locked = true })
--
-- -- Display settings
-- hl.bind("XF86AudioMedia", hl.dsp.exec_cmd("nwg-displays"), { locked = true })
-- hl.bind("XF86Display", hl.dsp.exec_cmd("nwg-displays"), { locked = true })
-- hl.bind("XF86Explorer", hl.dsp.exec_cmd("nwg-displays"), { locked = true })
