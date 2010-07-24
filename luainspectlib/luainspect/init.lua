-- luainspect.init - core LuaInspect source analysis
--
-- (c) 2010 David Manura, MIT License.

local M = {}

-- boilerplate/utility
-- LUA_PATH="?.lua;/path/to/metalua/src/compiler/?.lua;/path/to/metalua/src/lib/?.lua"
-- import modules -- order is important
require "lexer"
require "gg"
require "mlp_lexer"
require "mlp_misc"
require "mlp_table"
require "mlp_meta"
require "mlp_expr"
require "mlp_stat"
--require "mlp_ext"
_G.mlc = {} -- make gg happy

local inspect_globals = require "luainspect.globals"

local LS = require "luainspect.signatures"

-- Converts Lua source string to Lua AST (via mlp/gg)
function M.ast_from_string(src, filename)
  filename = filename or '(string)'
  local  lx  = mlp.lexer:newstream (src, filename)
  local  ast = mlp.chunk(lx)
  return ast
end

-- Walks AST `ast` in arbitrary order, visiting each node `n`, executing `fdown(n)` (if specified)
-- when doing down and `fup(n)` (if specified) when going if.
function M.walk(ast, fdown, fup)
  assert(type(ast) == 'table')
  if ast.tag and fdown then fdown(ast) end
  for _,bast in ipairs(ast) do
    if type(bast) == 'table' then
      M.walk(bast, fdown, fup)
    end
  end
  if ast.tag and fup then fup(ast) end
end

-- function for t[k]
local function tindex(t, k) return t[k] end

local unescape = {['d'] = '.'}


local ops = {}
ops["add"] = function(a,b) return a+b end
ops["sub"] = function(a,b) return a-b end
ops["mul"] = function(a,b) return a*b end
ops["div"] = function(a,b) return a/b end
ops["mod"] = function(a,b) return a%b end
ops["pow"] = function(a,b) return a^b end
ops["concat"] = function(a,b) return a..b end
ops["eq"] = function(a,b) return a==b end
ops["lt"] = function(a,b) return a<b end
ops["le"] = function(a,b) return a<=b end
ops["and"] = function(a,b) return a and b end
ops["or"] = function(a,b) return a or b end
ops["not"] = function(a) return not a end
ops["len"] = function(a) return #a end
ops["unm"] = function(a) return -a end

function M.inspect(ast)
  local globals = inspect_globals.globals(ast)

  local notes = {}

  -- Label variables with unique identifiers.
  local id = 0
  local seen_globals = {}
  M.walk(ast, function(ast)
    if ast.tag == 'Id' or ast.isfield then
      if ast.localdefinition then
        if ast.isdefinition then
          id = id + 1
          ast.id = id
        else
          ast.id = ast.localdefinition.id
        end
      elseif ast.isfield then
        local previousid = ast.previous.id
        if not previousid then -- note: ("abc"):upper() has no previous ID
          id = id + 1
          previousid = id
        end
        local name = previousid .. '.' .. ast[1]:gsub('%%', '%%'):gsub('%.', '%d')
        if not seen_globals[name] then
          id = id + 1
          seen_globals[name] = id
        end
        ast.id = seen_globals[name]

        -- also resolve name
        local previousresolvedname = ast.previous.resolvedname
        if previousresolvedname then
          ast.resolvedname = previousresolvedname .. '.' .. ast[1]:gsub('%%', '%%'):gsub('%.', '%d')
        end
      else -- global
        local name = ast[1]
        if not seen_globals[name] then
          id = id + 1
          seen_globals[name] = id
        end
        ast.id = seen_globals[name]

        -- also resolve name
        ast.resolvedname = ast[1]
      end
    end
  end)

  -- infer values
  M.walk(ast, nil, function(ast)
    -- process `require` statements.
    if ast.tag == "Local" then
      local vars_ast, values_ast = ast[1], ast[2]
      for i=1,#vars_ast do
        local var_ast, value_ast = vars_ast[i], values_ast[i]
        if value_ast and value_ast.valueknown then
	  var_ast.valueknown = value_ast.valueknown
	  var_ast.value = value_ast.value
	end
      end
    elseif ast.tag == "Id" then
      if ast.isglobal then
        local ok, val = pcall(tindex, _G, ast[1])
	if ok then ast.value = val; ast.valueknown = true else ast.value = nil; ast.valueknown = false end
      else
        local localdefinition = ast.localdefinition
        if localdefinition.valueknown and not localdefinition.isset then -- IMPROVE: support non-const (isset false) too
	  ast.value = localdefinition.value
	  ast.valueknown = localdefinition.valueknown
	end
      end
    elseif ast.tag == "Index" then
      local t_ast, k_ast = ast[1], ast[2]
      if t_ast.valueknown and k_ast.valueknown then
        local ok, val = pcall(tindex, t_ast.value, k_ast.value)
	if ok then
	  ast.value = val
	  ast.valueknown = true
	end
      end
    elseif ast.tag == "Call" then
      local args_known = true
      for i=2,#ast do if ast[i].valueknown ~= true then args_known = false; break end end
      if ast[1].valueknown and args_known then
        local func = ast[1].value
        if LS.safe_function[func] then
          local values = {}; for i=1,#ast-1 do values[i] = ast[i+1].value end
	  local ok, res = pcall(func, unpack(values,1,#ast-1))
	  if ok then ast.value = res; ast.valueknown = true else ast.value = res; ast.valueknown = "error" end
	  --TODO: handle multiple return values
        end
      end
    elseif ast.tag == "Invoke" then
      local t_ast, k_ast = ast[1], ast[2]
      if t_ast.valueknown and k_ast.valueknown then
        local ok, val = pcall(tindex, t_ast.value, k_ast.value)
	if ok then
	  ast.idxvalue = val
	  ast.idxvalueknown = true
	end
      end

      -- note: similar to "Call" code
      local args_known = true
      for i=3,#ast do if ast[i].valueknown ~= true then args_known = false; break end end
      if ast.idxvalueknown and args_known then
        local func = ast.idxvalue
        if LS.safe_function[func] then
          local values = {}; for i=1,#ast-2 do values[i] = ast[i+2].value end
	  local ok, res = pcall(func, t_ast.value, unpack(values,1,#ast-2))
	  if ok then ast.value = res; ast.valueknown = true else ast.value = res; ast.valueknown = "error" end
	  --TODO: handle multiple return values
        end
      end
    elseif ast.tag == "String" or ast.tag == "Number" then
      ast.value = ast[1]
      ast.valueknown = true
    elseif ast.tag == "True" or ast.tag == "False" then
      ast.value = (ast.tag == "True")
      ast.valueknown = true
    elseif ast.tag == "Paren" then
      ast.value = ast[1].value
      ast.valueknown = ast[1].valueknown
    elseif ast.tag == "Op" then
      local opid, aast, bast = ast[1], ast[2], ast[3]
      if aast.valueknown and (not bast or bast.valueknown) then
        local ok, val = pcall(ops[opid], aast.value, bast.value)
        if ok then
          ast.value = val
          ast.valueknown = true
        else
	  ast.value = val
          ast.valueknown = "error"
        end
      end
    end
  end)
  
  local function eval_name_helper(name)
    local var = _G
    for part in (name .. '.'):gmatch("([^.]*)%.") do
      part = part:gsub('%%(.)', unescape)
      if type(var) ~= 'table' and type(var) ~= 'userdata' then return nil end  --TODO:improve?
      var = var[part]
      if var == nil then return nil end
    end
    return var
  end
  local function eval_name(name)
    local ok, o = pcall(eval_name_helper, name)
    if ok then return o else return nil end
  end

  -- Make some nodes as having values related to its parent.
  -- This allows clicking on `bar` in `foo.bar` to display
  -- the value of `foo.bar` rather than just "bar".
  M.walk(ast, function(ast)
    if ast.tag == "Index" then
      ast[2].seevalue = ast
    elseif ast.tag == "Invoke" then
      ast[2].seevalue = {value=ast.idxvalue, valueknown=ast.idxvalueknown}
    end
  end)

  -- Create notes.
  local seen_comment = {}
  M.walk(ast, function(ast)
    if (ast.tag == 'Id' or ast.isfield) and ast.lineinfo then -- note: e.g. `Id "self" may have no lineinfo
      local vname = ast[1]
      local fchar = ast.lineinfo.first[3]
      local lchar = ast.lineinfo.last[3]
      local atype = ast.localdefinition and 'local' or ast.isfield and 'field' or 'global'
      --TODO: rename definedglobal to definedfield for clarity
      local definedglobal = ast.resolvedname and eval_name(ast.resolvedname) ~= nil or
                 atype == 'global' and (globals[vname] and globals[vname].set) or nil
      -- FIX: _G includes modules imported by inspect.lua, which is not desired
      local isparam = ast.localdefinition and ast.localdefinition.isparam or nil
      table.insert(notes, {fchar, lchar, ast=ast, type=atype, definedglobal=definedglobal, isparam=isparam})
    elseif ast.tag == 'String' then
      local fchar = ast.lineinfo.first[3]
      local lchar = ast.lineinfo.last[3]
      table.insert(notes, {fchar, lchar, ast=ast, type='string'})
    end

    -- comments
    if ast.lineinfo then
      local commentss = {}
      if ast.lineinfo.first.comments then table.insert(commentss, ast.lineinfo.first.comments) end
      if ast.lineinfo.last.comments  then table.insert(commentss, ast.lineinfo.last.comments) end
      for _,comments in ipairs(commentss) do
        for i,comment in ipairs(comments) do
          local text,fchar,lchar = unpack(comment)
          if not seen_comment[fchar] then
            seen_comment[fchar] = true
            table.insert(notes, {fchar, lchar, ast=comment, type='comment'})
          end
        end
      end
    end
  end)


  table.sort(notes, function(a,b) return a[1] < b[1] end)

  return notes
end

return M




