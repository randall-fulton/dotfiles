local wezterm = require("wezterm")
local act = wezterm.action

-- Creates action handlers for seamless navigation with nvim
require("navigate")

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
-- config.color_scheme = "One Light (base16)"
config.color_scheme = "Gruber (base16)"
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
    { key = 'a', mods = 'LEADER|CTRL', action = act.SendKey { key = 'a', mods = 'CTRL' } },
    { key = 'z', mods = 'LEADER', action = act.TogglePaneZoomState },
    { key = 'h', mods = 'CTRL', action = act.EmitEvent('ActivatePaneDirection-left') },
    { key = 'j', mods = 'CTRL', action = act.EmitEvent('ActivatePaneDirection-down') },
    { key = 'k', mods = 'CTRL', action = act.EmitEvent('ActivatePaneDirection-up') },
    { key = 'l', mods = 'CTRL', action = act.EmitEvent('ActivatePaneDirection-right') },
}

if is_windows() then
    config.launch_menu = {
        { label = "pwsh", args = { "powershell.exe", "-nologo" } },
        {
            label = "Dev Powershell",
            args = {
                "C:\\Windows\\SysWOW64\\WindowsPowerShell\\v1.0\\powershell.exe",
                "-NoLogo",
                "-noe",
                "-c",
                '&{Import-Module "C:/Program Files/Microsoft Visual Studio/2022/Community/Common7/Tools/Microsoft.VisualStudio.DevShell.dll"; Enter-VsDevShell eb0c1a84}'
            },
        },
        {
            label = "bash",
            args = {
                "C:/tools/msys64/msys2_shell.cmd",
                "-defterm",
                "-no-start",
                "-ucrt64",
                "-here",
                "-shell", "bash",
            },
        },
        {
            label = "fish",
            args = {
                "C:/tools/msys64/msys2_shell.cmd",
                "-defterm",
                "-no-start",
                "-ucrt64",
                "-here",
                "-shell", "fish",
            },
        },
    }

    config.default_prog = { "powershell.exe", "-nologo" }
end

config.ssh_domains = {
    {
        name = "x1c",
        remote_address = "192.168.1.89",
        username = "randall",
    }
}

local bar = wezterm.plugin.require("https://github.com/adriankarlen/bar.wezterm")
bar.apply_to_config(config)

return config
