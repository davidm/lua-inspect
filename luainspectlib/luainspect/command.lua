#!/bin/env lua

-- luainspect.command - LuaInspect command-line interface.
-- This file can be invoked from the command line

package.path = package.path .. ';metalualib/?.lua'
package.path = package.path .. ';luainspectlib/?.lua'


local LA = require "luainspect.ast"
local LI = require "luainspect.init"
local LH = require "luainspect.html"

local function loadfile(filename)
  local fh = assert(io.open(filename, 'r'))
  local data = fh:read'*a'
  fh:close()
  return data
end


-- Warning/status reporting function.
-- CATEGORY: reporting + AST
local function report(s) io.stderr:write(s, "\n") end

local path = ...
if not path then
  io.stderr:write("inspect.lua <path.lua>")
  os.exit(1)
end

local src = loadfile(path)
local ast, err, linenum, colnum, linenum2 = LA.ast_from_string(src, path)

--require "metalua.table2"; table.print(ast, 'hash', 50)
if ast then
  local tokenlist = LA.ast_to_tokenlist(ast, src)
  LI.inspect(ast, tokenlist, report)
  LI.mark_related_keywords(ast, tokenlist, src)

  local ast = LH.ast_to_html(ast, src, tokenlist)

  io.stdout:write(ast)
else
  io.stderr:write("syntax error: ", err)
  os.exit(1)
end



