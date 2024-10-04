local wezterm = require("wezterm")

local config = wezterm.config_builder()

local is_windows = function()
    return wezterm.target_triple:find("windows") ~= nil
end

local is_macos = function()
    return wezterm.target_triple:find("darwin") ~= nil
end

local is_linux = function()
    return wezterm.target_triple:find("linux") ~= nil
end

config.color_scheme = "Gruber (base16)"
config.font = wezterm.font_with_fallback({
    "FiraCode Nerd Font Mono", -- Windows
    "Fira Code",               -- macOS
})
config.font_size = 18
config.window_decorations = "RESIZE"

if is_windows() then
    config.default_prog = {
        "C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"
    }
end

return config
