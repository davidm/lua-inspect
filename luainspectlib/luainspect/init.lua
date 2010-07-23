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

-- Converts Lua source string to Lua AST (via mlp/gg)
function M.ast_from_string(src, filename)
  filename = filename or '(string)'
  local  lx  = mlp.lexer:newstream (src, filename)
  local  ast = mlp.chunk(lx)
  return ast
end

-- Walks AST `ast` in arbitrary order, visiting each node `n`, executing `f(n)`.
function M.walk(ast, f)
  assert(type(ast) == 'table')
  if ast.tag then
    f(ast)
  end
  for i,bast in ipairs(ast) do
    if type(bast) == 'table' then
      M.walk(bast, f)
    end
  end
end

local unescape = {['d'] = '.'}

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
        local parentid = ast.parent.id
        local name = parentid .. '.' .. ast[1]:gsub('%%', '%%'):gsub('%.', '%d')
        if not seen_globals[name] then
          id = id + 1
          seen_globals[name] = id
        end
        ast.id = seen_globals[name]

        -- also resolve name
        local parentresolvedname = ast.parent.resolvedname
        if parentresolvedname then
          ast.resolvedname = parentresolvedname .. '.' .. ast[1]:gsub('%%', '%%'):gsub('%.', '%d')
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

  -- Create notes.
  local seen_comment = {}
  M.walk(ast, function(ast)
    --print(ast.tag)
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




