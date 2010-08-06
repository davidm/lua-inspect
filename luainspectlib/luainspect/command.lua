#!/bin/env lua

-- luainspect.command - LuaInspect command-line interface.
-- This file can be invoked from the command line

package.path = package.path .. ';metalualib/?.lua'
package.path = package.path .. ';luainspectlib/?.lua'


local LI = require "luainspect.init"
local LH = require "luainspect.html"

local function loadfile(filename)
  local fh = assert(io.open(filename, 'r'))
  local data = fh:read'*a'
  fh:close()
  return data
end


local path = ...
if not path then
  io.stderr:write("inspect.lua <path.lua>")
  os.exit(1)
end

local src = loadfile(path)
local ast, err, linenum, colnum, linenum2 = LI.ast_from_string(src, path)

--require "metalua.table2"; table.print(ast, 'hash', 50)
if ast then
  local tokenlist = LI.ast_to_tokenlist(ast, src)
  LI.inspect(ast, tokenlist)

  local ast = LH.ast_to_html(ast, src, tokenlist)

  io.stdout:write(ast)
else
  io.stderr:write("syntax error: ", err)
  os.exit(1)
end



