local theme = require("colors")
local icons = require("icons")

local spaces = {}

function workspace_changed(env)
    local id = env.FOCUSED_WORKSPACE
    if spaces[tonumber(id)].name == env.NAME then
        print("enable drawing for "..env.NAME)
        sbar.set(env.NAME..".group", { background = { drawing = "on" } })
    else
        sbar.set(env.NAME..".group", { background = { drawing = "off" } })
    end
end

function update_windows(env)
    for id, name in ipairs(spaces) do
        sbar.exec(
            "aerospace list-windows --all --format '%{workspace}/%{app-name}'",
            function(output)
                for _, win in ipairs(string.split(output)) do
                    local space, name = string.match(win, "([^/]+)/([^/]+)")
                    if spaces[tonumber(space)].windows[name] == nil then
                        table.insert(spaces[tonumber(space)].windows, name)
                    end
                end
                recreate_spaces()
            end
        )
    end
end

function recreate_spaces()
    for _, s in pairs(spaces) do
        sbar.remove(s.name)
        sbar.remove(s.name..".group")

        for _, win in ipairs(s.windows) do
            sbar.remove(s.name.."."..win)
        end
    end

    for id, s in ipairs(spaces) do
        local space = sbar.add("item", s.name , {
            label = tostring(id),
            click_script = "aerospace workspace "..tostring(id),
        })
        space:subscribe("aerospace_workspace_change", workspace_changed)
        local windows = { s.name }
        for _, win in ipairs(s.windows) do
            sbar.add("item", s.name.."."..win, {
                icon = icons[win] or icons.default,
            })
            table.insert(windows, s.name.."."..win)
        end
        sbar.add("bracket", s.name..".group", windows, {
            background = {
                drawing = "off",
                color = theme.bar.accent,
                corner_radius = 0,
                height = 8,
                y_offset = 14,
            },
        })
    end
end

sbar.add("event", "aerospace_workspace_change")

local aerospace = sbar.add("item", "aerospace", {})
aerospace:subscribe("space_windows_change", update_windows)

for id = 1, 10, 1 do
    local space = sbar.add("item", "space."..tostring(id), {
        label = tostring(id),
        click_script = "aerospace workspace "..id,
    })
    spaces[id] = { name = space.name, windows = {} }
    sbar.add("bracket", space.name..".group", { space.name }, {
        background = {
            drawing = "off",
            color = theme.bar.accent,
            corner_radius = 0,
            height = 8,
            y_offset = 14,
        },
    })
end

recreate_spaces()
