-- This installs LuaInspect in SciTE.

-- Edit the following path to match your system.
local LUAINSPECT_PATH = "z:/work/lua-inspect"
package.path = package.path .. ";" .. LUAINSPECT_PATH .. "/metalualib/?.lua"
package.path = package.path .. ";" .. LUAINSPECT_PATH .. "/luainspectlib/?.lua"
require "luainspect.scite" : install()
require "luainspect.scite2" : preinstall()
