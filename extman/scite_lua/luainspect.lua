-- This installs LuaInspect in SciTE.

-- If necessary, edit the following path to match your system.
local LUAINSPECT_PATH = props['ext.lua.directory'] .. '/../..'    -- "c:/lua-inspect"
package.path = package.path .. ";" .. LUAINSPECT_PATH .. "/metalualib/?.lua"
package.path = package.path .. ";" .. LUAINSPECT_PATH .. "/luainspectlib/?.lua"
require "luainspect.scite" : install()

