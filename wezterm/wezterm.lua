local wezterm = require("wezterm")

local config = wezterm.config_builder()

config.color_scheme = "Everforest Dark Hard (Gogh)"
config.font = wezterm.font "Fira Code"
config.font_size = 18
config.window_decorations = "RESIZE"

return config
