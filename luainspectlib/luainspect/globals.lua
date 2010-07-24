-- LuaInspect.globals - identifier scope analysis
-- Locates locals, globals, and their definitions.
--
-- (c) D.Manura, 2008-2010, MIT license.

-- based on http://lua-users.org/wiki/DetectingUndefinedVariables

local M = {}

-- Helper function: Parse current node in AST recursively.
local function traverse(ast, scope, globals, level, functionlevel)
  scope = scope or {}

  local blockrecurse

  -- operations on walking down the AST
  if ast.tag == "Local" or ast.tag == "Localrec" then
    local vnames, vvalues = ast[1], ast[2]
    for i,v in ipairs(vnames) do
      assert(v.tag == "Id")
      local vname = v[1]
      local parentscope = getmetatable(scope).__index
      parentscope[vname] = v

      v.localdefinition = v
      v.isdefinition = true
      v.functionlevel = functionlevel
    end
    blockrecurse = 1
  elseif ast.tag == "Id" then
    local vname = ast[1]
    if scope[vname] then
      ast.localdefinition = scope[vname]
      scope[vname].isused = true
      ast.functionlevel = functionlevel
    else
      ast.isglobal = true
    end
    --if not scope[vname] then
    --  print(string.format("undefined %s at line %d", vname, ast.lineinfo.first[1]))
    --end
  elseif ast.tag == "Function" then
    local params = ast[1]
    local body = ast[2]
    functionlevel = functionlevel + 1
    for i,v in ipairs(params) do
      local vname = v[1]
      assert(v.tag == "Id" or v.tag == "Dots")
      if v.tag == "Id" then
        scope[vname] = v
        v.localdefinition = v
        v.isdefinition = true
        v.isparam = true
        v.functionlevel = functionlevel
      end
    end
    blockrecurse = 1
  elseif ast.tag == "Set" then
    local vrefs, vvalues = ast[1], ast[2]
    for i,v in ipairs(vrefs) do
      if v.tag == 'Id' then
        local vname = v[1]
        if scope[vname] then
          scope[vname].isset = true
        else
          if not globals[vname] then
            globals[vname] = {set=v}
          end
          ast.isglobal = true
        end
      end
    end
  elseif ast.tag == "Fornum" then
    local v = ast[1]
    local vname = v[1]
    scope[vname] = v
    v.localdefinition = v
    v.isdefinition = true
    v.functionlevel = functionlevel
    blockrecurse = 1
  elseif ast.tag == "Forin" then
    local vnames = ast[1]
    for i,v in ipairs(vnames) do
      local vname = v[1]
      scope[vname] = v
      v.localdefinition = v
      v.isdefinition = true
      v.functionlevel = functionlevel
    end
    blockrecurse = 1
  end

  -- recurse (depth-first search down the AST)
  if ast.tag == "Repeat" then
    local scope = scope
    for i,v in ipairs(ast[1]) do
      scope = setmetatable({}, {__index = scope})
      traverse(v, scope, globals, level+1, functionlevel)
    end
    scope = setmetatable({}, {__index = scope})
    traverse(ast[2], scope, globals, level+1, functionlevel)
  else
    for i,v in ipairs(ast) do
      if i ~= blockrecurse and type(v) == "table" then
        local scope = setmetatable({}, {__index = scope})
        traverse(v, scope, globals, level+1, functionlevel)
      end
    end
  end

  -- operations on walking up the AST
  if ast.tag == "Index" then
    local previous = ast[1].tag == 'Id' and ast[1] or ast[1].containid
    if previous and ast[2].tag == "String" then
      ast[2].isfield = true
      ast[2].previous = previous
      ast.containid = ast[2]
    end
  elseif ast.tag == "Invoke" then
    local previous = ast[1].tag == 'Id' and ast[1] or ast[1].containid
    if previous then
      ast[2].isfield = true
      ast[2].previous = previous
      ast.containid = ast[2]
    end
  end
end

function M.globals(ast)
  -- Default list of defined variables.
  local scope = setmetatable({}, {})
  local globals = {}
  traverse(ast, scope, globals, 1, 1) -- Start check.

  return globals
end



return M
