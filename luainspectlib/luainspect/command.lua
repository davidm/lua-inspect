#!/usr/bin/env lua

-- luainspect.command - LuaInspect command-line interface.
-- This file can be invoked from the command line

package.path = package.path .. ';metalualib/?.lua'
package.path = package.path .. ';luainspectlib/?.lua'


local LA = require "luainspect.ast"
local LI = require "luainspect.init"

local function loadfile(filename)
  local fh = assert(io.open(filename, 'r'))
  local data = fh:read'*a'
  fh:close()
  return data
end

local function fail(err)
  io.stderr:write(err, '\n')
  os.exit(1)
end

-- Warning/status reporting function.
-- CATEGORY: reporting + AST
local function report(s) io.stderr:write(s, "\n") end

-- parse flags
local fmt = arg[1] and table.remove(arg, 1):match'^%-f(.*)' or 'delimited'
if fmt == '' and arg[1] then bflag = table.remove(arg, 1) end
local ast_to_text =
  (fmt == 'delimited') and require 'luainspect.delimited'.ast_to_delimited or
  (fmt == 'html') and require 'luainspect.html'.ast_to_html or
  fail('invalid format specified, -f'..fmt)

local path = unpack(arg)
if not path then
  fail("inspect.lua [-f {delimited|html}>] <path.lua>")
end

local src = loadfile(path)
local ast, err, linenum, colnum, linenum2 = LA.ast_from_string(src, path)

--require "metalua.table2"; table.print(ast, 'hash', 50)
if ast then
  local tokenlist = LA.ast_to_tokenlist(ast, src)
  LI.inspect(ast, tokenlist, src, report)
  LI.mark_related_keywords(ast, tokenlist, src)

  local output = ast_to_text(ast, src, tokenlist)

  io.stdout:write(output)
else
  io.stderr:write("syntax error: ", err)
  os.exit(1)
end



