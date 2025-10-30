local theme = require("theme")

--- @class Window
--- @field id string
--- @field name string

--- @class Space
--- @field id number
--- @field name string
--- @field windows Window[]

local spaces = {}

function workspace_changed(env)
    local id = env.FOCUSED_WORKSPACE
    if spaces[tonumber(id)].name == env.NAME then
        sbar.set(env.NAME..".group", { background = { drawing = "on" } })
        sbar.set(env.NAME, { label = { color = theme.colors.accent } })
        for _, win in ipairs(spaces[tonumber(id)].windows) do
            sbar.set(env.NAME.."."..win.id, { icon = { color = theme.colors.accent } })
        end
    else
        sbar.set(env.NAME..".group", { background = { drawing = "off" } })
        sbar.set(env.NAME, { label = { color = theme.colors.fg } })
        for _, space in ipairs(spaces) do
            if space.name == env.NAME then
                for _, win in ipairs(space.windows) do
                    sbar.set(env.NAME.."."..win.id, { icon = { color = theme.colors.fg } })
                end
            end
        end
    end
end

function update_windows(env)
    for id, name in ipairs(spaces) do
        sbar.exec(
            "aerospace list-windows --all --format '%{workspace}/%{window-id}/%{app-name}'",
            function(output)
                for id, space in ipairs(spaces) do
                    for _, win in ipairs(space.windows) do
                        sbar.remove(space.name.."."..win.id)
                    end
                    spaces[id].windows = {}
                end

                local windows = string.split(output)
                for _, win in ipairs(windows) do
                    local space, id, name = string.match(win, "([^/]+)/([^/]+)/([^/]+)")
                    table.insert(spaces[tonumber(space)].windows, {
                        id = id,
                        name = name,
                    })
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
            sbar.remove(s.name.."."..win.id)
        end
    end

    for id, s in ipairs(spaces) do
        render_space(s)
    end

    highlight_active_space()
end

function highlight_active_space()
    sbar.exec("aerospace list-workspaces --focused", function(output)
        sbar.trigger("aerospace_workspace_change", {
            FOCUSED_WORKSPACE = output,
        })
    end)
end

--- @param s Space
function render_space(s)
    local space = sbar.add("item", s.name , {
        label = tostring(s.id),
        click_script = "aerospace workspace "..tostring(s.id),
    })
    space:subscribe("aerospace_workspace_change", workspace_changed)
    local windows = { s.name }
    for _, win in ipairs(s.windows) do
        sbar.add("item", s.name.."."..win.id, {
            icon = theme.icons[win.name] or theme.icons.default,
            click_script = "aerospace focus --window-id "..win.id,
        })
        table.insert(windows, s.name.."."..win.id)
    end
    sbar.add("bracket", s.name..".group", windows, {
        background = {
            drawing = "off",
            color = theme.colors.accent,
            corner_radius = 0,
            height = 8,
            y_offset = 14,
        },
        click_script = "aerospace workspace "..tostring(id),
    })
end

sbar.add("event", "aerospace_workspace_change")

local aerospace = sbar.add("item", "aerospace", {})
aerospace:subscribe("space_windows_change", update_windows)

for id = 1, 10, 1 do
    local space = { id = id, name = "space."..tostring(id), windows = {} }
    table.insert(spaces, space)
    render_space(space)
end

recreate_spaces()
