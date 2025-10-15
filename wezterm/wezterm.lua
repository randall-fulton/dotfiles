local wezterm = require("wezterm")
local act = wezterm.action

local is_windows = function()
    return wezterm.target_triple:find("windows") ~= nil
end

local is_macos = function()
    return wezterm.target_triple:find("darwin") ~= nil
end

local is_linux = function()
    return wezterm.target_triple:find("linux") ~= nil
end

local config = wezterm.config_builder()
config.color_scheme = "One Light (base16)"
-- config.color_scheme = "Gruber (base16)"
-- config.font = wezterm.font_with_fallback({
--     "FiraCode Nerd Font Mono", -- Windows
--     "Fira Code",               -- macOS
-- })
config.font_size = 14
config.window_decorations = "RESIZE"

config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }
config.keys = {
    {
        key = '|',
        mods = 'LEADER|SHIFT',
        action = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
    },
    {
        key = '-',
        mods = 'LEADER',
        action = act.SplitVertical { domain = 'CurrentPaneDomain' },
    },
    -- Send "CTRL-A" to the terminal when pressing CTRL-A, CTRL-A
    {
        key = 'a',
        mods = 'LEADER|CTRL',
        action = act.SendKey { key = 'a', mods = 'CTRL' },
    },
    {
        key = 'h',
        mods = 'LEADER',
        action = act.ActivatePaneDirection 'Left',
    },
    {
        key = 'l',
        mods = 'LEADER',
        action = act.ActivatePaneDirection 'Right',
    },
    {
        key = 'k',
        mods = 'LEADER',
        action = act.ActivatePaneDirection 'Up',
    },
    {
        key = 'j',
        mods = 'LEADER',
        action = act.ActivatePaneDirection 'Down',
    },
    {
        key = 'z',
        mods = 'LEADER',
        action = act.TogglePaneZoomState,
    },
}

if is_windows() then
    config.wsl_domains = {
        {
            name = "WSL:Ubuntu",
            distribution = "Ubuntu",
        }
    }
    config.default_domain = "WSL:Ubuntu"
end

local bar = wezterm.plugin.require("https://github.com/adriankarlen/bar.wezterm")
bar.apply_to_config(config)

return config
