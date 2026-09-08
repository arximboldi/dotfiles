hl.config({
    general = {
        layout = "master", -- dwindle
    },

    -- See https://wiki.hypr.land/Configuring/Layouts/Dwindle-layout/ for more
    dwindle = {
        -- pseudotile = true, -- Master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
        preserve_split = true, -- You probably want this
    },

    -- See https://wiki.hypr.land/Configuring/Layouts/Master-layout/ for more
    master = {
        new_status = "inherit",
        mfact = 0.6,
    },
})

-- Ref https://wiki.hypr.land/Configuring/Core/Rules/Workspace-rules/
-- "Smart gaps" / "No gaps when only"
-- uncomment all if you wish to use that.
-- hl.workspace_rule({ workspace = "w[tv1]", gaps_out = 0, gaps_in = 0 })
-- hl.workspace_rule({ workspace = "f[1]",   gaps_out = 0, gaps_in = 0 })
-- hl.window_rule({ match = { float = false, workspace = "w[tv1]" }, border_size = 0, rounding = 0 })
-- hl.window_rule({ match = { float = false, workspace = "f[1]" },   border_size = 0, rounding = 0 })

hl.workspace_rule({ workspace = "1", persistent = true, default_name = "web" })
hl.workspace_rule({ workspace = "2", persistent = true, default_name = "dev" })
hl.workspace_rule({ workspace = "3", persistent = true, default_name = "misc" })
hl.workspace_rule({ workspace = "4", persistent = true, default_name = "chat" })

-- See https://wiki.hypr.land/Configuring/Core/Rules/Window-rules/ for more

-- Example window rule
-- hl.window_rule({ match = { class = "^(kitty)$", title = "^(kitty)$" }, float = true })

hl.window_rule({ match = { class = "^(firefox)$" },              workspace = "1" })
hl.window_rule({ match = { class = "^(zen-beta)$" },             workspace = "1" })
hl.window_rule({ match = { class = "^(emacs)$" },                workspace = "2" })
hl.window_rule({ match = { class = "^(Slack)$" },                workspace = "4" })
hl.window_rule({ match = { class = "^(signal)$" },               workspace = "4" })
hl.window_rule({ match = { class = "^(discord)$" },              workspace = "4" })
hl.window_rule({ match = { class = "^(org.telegram.desktop)$" }, workspace = "4" })
hl.window_rule({ match = { class = "^(com.rtosta.zapzap)$" },    workspace = "4" })

-- Ignore maximize requests from apps. You'll probably like this.
-- hl.window_rule({ match = { class = ".*" }, suppress_event = "maximize" })

-- Fix some dragging issues with XWayland
-- hl.window_rule({
--     match = { class = "^$", title = "^$", xwayland = true, float = true, fullscreen = false, pin = false },
--     no_focus = true,
-- })

-- hl.window_rule({ match = { class = "^(firefox)$" }, min_size = { 1, 1 } })

-- hl.window_rule({ match = { fullscreen = true }, idle_inhibit = "fullscreen" })

hl.layer_rule({
    name = "dms-color-picker",
    match = { namespace = "dms:color-picker" },
    no_anim = true,
})
