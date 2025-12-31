local wezterm = require('wezterm')
local act = wezterm.action

local function isViProcess(pane) 
    local name = pane:get_foreground_process_name()
    local title = pane:get_title()
    -- @Hack On windows, nvim launches node, seemingly for TreeSitter...
    return name:find('n?vim') or name:find('node') or title:find('n?vim') or title:find('node')
end

local function conditionalActivatePane(window, pane, pane_direction, vim_direction)
    if isViProcess(pane) then
        print("in vim")
        window:perform_action(
            -- This should match the keybinds you set in Neovim.
            act.SendKey({ key = vim_direction, mods = 'CTRL' }),
            pane
        )
    else
        print("not in vim")
        window:perform_action(act.ActivatePaneDirection(pane_direction), pane)
    end
end

wezterm.on('ActivatePaneDirection-right', function(window, pane)
    conditionalActivatePane(window, pane, 'Right', 'l')
end)
wezterm.on('ActivatePaneDirection-left', function(window, pane)
    conditionalActivatePane(window, pane, 'Left', 'h')
end)
wezterm.on('ActivatePaneDirection-up', function(window, pane)
    conditionalActivatePane(window, pane, 'Up', 'k')
end)
wezterm.on('ActivatePaneDirection-down', function(window, pane)
    conditionalActivatePane(window, pane, 'Down', 'j')
end)
