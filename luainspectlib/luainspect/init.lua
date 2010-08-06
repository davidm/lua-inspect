-- luainspect.init - core LuaInspect source analysis.
--
-- This module is a bit more high level than luainspect.ast.  It deals more with
-- interpretation/inference of semantics of an AST.  It also uses luainspect.globals,
-- which does the basic semantic interpretation of globals/locals.
--
-- (c) 2010 David Manura, MIT License.

local M = {}

-- This is the API version.  It is an ISO8601 date expressed as a fraction.
M.APIVERSION = 0.20100805

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
-- Metalua:IMPROVE: make above imports simpler

local LA = require "luainspect.ast"
local LG = require "luainspect.globals"
local LS = require "luainspect.signatures"

--! require 'luainspect.typecheck' (context)


-- Stringifies interpreted value for debugging.
-- CATEGORY: debug
local function debugvalue(ast)
  local s
  if ast then
    s = ast.valueknown and 'known:' .. tostring(ast.value) or 'unknown'
  else
    s = '?'
  end
  return s
end


-- Gets all keywords related to AST `ast`, where `top_ast` is the root of `ast`
-- and `src` is source code of `top_ast`
-- Related keywords are defined as all keywords directly associated with block containing node
-- `ast`.  Furthermore, break statements are related to containing loop statements,
-- and return statements are related to containing function statement (if any).
-- function declaration syntactic sugar is handled specially too to ensure the 'function' keyword
-- is highlighted even though it may be outside of the `Function AST.
--
-- Returns token list or nil if not applicable.  Returned `ast` is AST containing related keywords.
-- CATEGORY: keyword comprehension
local iskeystat = {Do=true, While=true, Repeat=true, If=true, Fornum=true, Forin=true,
    Local=true, Localrec=true, Return=true, Break=true, Function=true,
    Set=true -- note: Set for `function name`
}
local isloop = {While=true, Repeat=true, Fornum=true, Forin=true}
local isblock = {Do=true, While=true, Repeat=true, If=true, Fornum=true, Forin=true, Function=true}
function M.related_keywords(ast, top_ast, tokenlist, src)
  -- Expand or contract AST for certain contained statements.
  local more
  if ast.tag == 'Return' then
    -- if `return` selected, that consider containing function selected (if any)
    if not ast.parent then LA.mark_parents(top_ast) end
    local ancestor_ast = ast.parent
    while ancestor_ast ~= nil and ancestor_ast.tag ~= 'Function' do
      ancestor_ast = ancestor_ast.parent
    end
    if ancestor_ast then ast = ancestor_ast end -- but only change if exists
  elseif ast.tag == 'Break' then
    -- if `break` selected, that consider containing loop selected
    if not ast.parent then LA.mark_parents(top_ast) end
    local ancestor_ast = ast.parent
    while ancestor_ast ~= nil and not isloop[ancestor_ast.tag] do
      ancestor_ast = ancestor_ast.parent
    end
    ast = ancestor_ast
  elseif ast.tag == 'Set' then
    local val1_ast = ast[2][1]
    if val1_ast.tag == 'Function' then
      local token = tokenlist[LA.ast_idx_range_in_tokenlist(tokenlist, ast)]
      if token.tag == 'Keyword' and token[1] == 'function' then -- function with syntactic sugar `function f`
        ast = ast[2][1] -- select `Function node
      else
        more = true
      end
    else
      more = true
    end
  elseif ast.tag == 'Localrec' and ast[2][1].tag == 'Function' then
    -- if `local function f` selected, which becomes a `Localrec, consider `Function node.
    ast = ast[2][1]
    --IMPROVE: only contract ast if `function` part of `local function` is selected.
  else
    more = true
  end
  if more then -- not yet handled
    -- Consider containing block.
    if not ast.parent then LA.mark_parents(top_ast) end
    local ancestor_ast = ast
    while ancestor_ast ~= top_ast and not isblock[ancestor_ast.tag] do
      ancestor_ast = ancestor_ast.parent
    end
    ast = ancestor_ast
  end

  --  keywords in statement/block.    
  if iskeystat[ast.tag] then
    local keywords = {}
    for i=1,#tokenlist do
     local token = tokenlist[i]
     if token.ast == ast and token.tag == 'Keyword' then
       keywords[#keywords+1] = token
     end
    end

    -- Expand keywords for certaining statements.
    if ast.tag == 'Function' then
      -- if `Function, also select 'function' and 'return' keywords
      local function f(ast)
        for _,cast in ipairs(ast) do
          if type(cast) == 'table' then
            if cast.tag == 'Return' then
              local token = tokenlist[LA.ast_idx_range_in_tokenlist(tokenlist, cast)]
              keywords[#keywords+1] = token
            elseif cast.tag ~= 'Function' then f(cast) end
          end
        end
      end
      f(ast)
      if not ast.parent then M.mark_parents(top_ast) end
      local grand_ast = ast.parent.parent
      if grand_ast.tag == 'Set' then
        local token = tokenlist[LA.ast_idx_range_in_tokenlist(tokenlist, grand_ast)]
        if token.tag == 'Keyword' and token[1] == 'function' then
          keywords[#keywords+1] = token
        end
      elseif grand_ast.tag == 'Localrec' then
        local tidx = LA.ast_idx_range_in_tokenlist(tokenlist, grand_ast)
        repeat tidx = tidx + 1 until tokenlist[tidx].tag == 'Keyword' and tokenlist[tidx][1] == 'function'
        local token = tokenlist[tidx]
        keywords[#keywords+1] = token
      end
    elseif isloop[ast.tag] then
      -- if loop, also select 'break' keywords
      local function f(ast)
        for _,cast in ipairs(ast) do
          if type(cast) == 'table' then
            if cast.tag == 'Break' then
              local tidx = LA.ast_idx_range_in_tokenlist(tokenlist, cast)
              keywords[#keywords+1] = tokenlist[tidx]
            elseif not isloop[cast.tag]  then f(cast) end
          end
        end
      end
      f(ast)        
    end
    
    return keywords, ast
  end
  return nil, ast
end


-- Mark tokenlist (top_ast/tokenlist/src) with keywordid AST attributes.
-- All keywords related to each other have the same keyword ID integer.
-- NOTE: This is not done/undone by inspect/uninspect.
-- CATEGORY: keyword comprehension
function M.mark_related_keywords(top_ast, tokenlist, src)
  local id = 0
  local idof = {}
  for _, token in ipairs(tokenlist) do
    if token.tag == 'Keyword' and not idof[token] then
      id = id + 1
      local match_ast =
        LA.smallest_ast_in_range(top_ast, tokenlist, src, token.fpos, token.lpos)
      local ktokenlist = M.related_keywords(match_ast, top_ast, tokenlist, src)
      if ktokenlist then
         for _, ktoken in ipairs(ktokenlist) do
          ktoken.keywordid = id
          idof[ktoken] = true
        end
      end
      -- note: related_keywords may return a keyword set not containing given keyword.
    end
  end
end


-- function for t[k]
local function tindex(t, k) return t[k] end

local unescape = {['d'] = '.'}


-- Functional forms of Lua operators.
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



-- Set known value on ast to that on src_ast.
-- CATEGORY: utility function for infer_values.
local function set_value(ast, src_ast)
  if not src_ast.valueknown or ast.valueknown and ast.value ~= src_ast.value then -- unknown if multiple values
    ast.valueknown = 'multiple'
  else
    ast.valueknown = src_ast.valueknown
    ast.value = src_ast.value
  end
end


-- CATEGORY: utility function for infer_values.
local function tastnewindex(t_ast, k_ast, v_ast)
  if t_ast.valueknown and k_ast.valueknown and v_ast.valueknown then
    local t, k, v = t_ast.value, k_ast.value, v_ast.value
    if t[k] ~= nil and v ~= t[k] then -- multiple values
      return v, 'multiple'
    else
      t[k] = v
      return v, nil
    end
  end
end


-- Infer values of variables.
--FIX/WARNING - this probably needs more work
-- Sets top_ast.valueglobals, ast.value, ast.valueknown, ast.idxvalue, ast.idxvalueknown
-- CATEGORY: code interpretation
local nil_value_ast = {}
function M.infer_values(top_ast)
  if not top_ast.valueglobals then top_ast.valueglobals = {} end

  -- infer values
  LA.walk(top_ast, nil, function(ast)
    -- process `require` statements.
    if ast.tag == 'Local' or ast.tag == 'Localrec' then
      local vars_ast, values_ast = ast[1], ast[2]
      for i=1,#vars_ast do
        local var_ast, value_ast = vars_ast[i], values_ast[i]
        value_ast = value_ast or nil_value_ast
        set_value(var_ast, value_ast)
        --FIX: handle functions with multiple returns
      end
    elseif ast.tag == 'Set' then
      local vars_ast, values_ast = ast[1], ast[2]
      for i=1,#vars_ast do
        local var_ast, value_ast = vars_ast[i], values_ast[i]
        value_ast = value_ast or nil_value_ast
        if var_ast.tag == 'Index' then
          if var_ast[1].valueknown and var_ast[2].valueknown and value_ast.valueknown then
            var_ast.valueknown, var_ast.value, multiple = pcall(tastnewindex, var_ast[1], var_ast[2], value_ast)
            if multiple then var_ast.valueknown = 'multiple' end
            --FIX: propagate to localdefinition?
          end
        else
          assert(var_ast.tag == 'Id', var_ast.tag)
          if var_ast.localdefinition then
            set_value(var_ast, value_ast)
          else -- global
            if value_ast.valueknown then
              local name, val = var_ast[1], value_ast.value
              top_ast.valueglobals[name] = val
            end
          end
        end --FIX: handle functions with multiple returns
        --FIX: propagate to definition or localdefinition?
      end
    elseif ast.tag == 'Id' then
      if ast.localdefinition then
        local localdefinition = ast.localdefinition
        if localdefinition.valueknown and not localdefinition.isset then -- IMPROVE: support non-const (isset false) too
          set_value(ast, localdefinition)
        end
      else -- global
        local name = ast[1]
        local v = top_ast.valueglobals[name]
        if v ~= nil then
          ast.value = v
          ast.valueknown = true
        else
          ast.valueknown, ast.value = pcall(tindex, _G, ast[1])
        end
      end
    elseif ast.tag == 'Index' then
      local t_ast, k_ast = ast[1], ast[2]
      if t_ast.valueknown and k_ast.valueknown then
        ast.valueknown, ast.value = pcall(tindex, t_ast.value, k_ast.value)
      end
    elseif ast.tag == 'Call' then
      local args_known = true
      for i=2,#ast do if ast[i].valueknown ~= true then args_known = false; break end end
      if ast[1].valueknown and args_known then
        local func = ast[1].value
        if LS.safe_function[func] then
          local values = {}; for i=1,#ast-1 do values[i] = ast[i+1].value end
          ast.valueknown, ast.value = pcall(func, unpack(values,1,#ast-1))
          --TODO: handle multiple return values
        end
      end
    elseif ast.tag == 'Invoke' then
      local t_ast, k_ast = ast[1], ast[2]
      if t_ast.valueknown and k_ast.valueknown then
        ast.idxvalueknown, ast.idxvalue = pcall(tindex, t_ast.value, k_ast.value)
      end

      -- note: similar to 'Call' code
      local args_known = true
      for i=3,#ast do if ast[i].valueknown ~= true then args_known = false; break end end
      if ast.idxvalueknown and args_known then
        local func = ast.idxvalue
        if LS.safe_function[func] then
          local values = {}; for i=1,#ast-2 do values[i] = ast[i+2].value end
          ast.valueknown, ast.value = pcall(func, t_ast.value, unpack(values,1,#ast-2))
          --TODO: handle multiple return values
        end
      end
    elseif ast.tag == 'String' or ast.tag == 'Number' then
      ast.value = ast[1]; ast.valueknown = true
    elseif ast.tag == 'True' or ast.tag == 'False' then
      ast.value = (ast.tag == 'True'); ast.valueknown = true
    elseif ast.tag == 'Function' then
      if not ast.valueknown then -- avoid redefinition
        ast.value = function() end -- IMPROVE?
      end
      ast.valueknown = true
    elseif ast.tag == 'Table' then
      if not ast.valueknown then -- avoid redefinition
        local value = {}
        local n = 1
        for _,east in ipairs(ast) do
          if east.tag == 'Pair' then
            local kast, vast = east[1], east[2]
            if kast.valueknown and vast.valueknown then
              value[kast.value] = vast.value
            end
          else
            if east.valueknown then
              value[n] = east.value
            end
            n = n + 1
          end
        end
        --table.foreach(value, print)
        ast.value = value; ast.valueknown = true
      end
    elseif ast.tag == 'Paren' then
      ast.value = ast[1].value; ast.valueknown = ast[1].valueknown
    elseif ast.tag == 'Op' then
      local opid, aast, bast = ast[1], ast[2], ast[3]
      if aast.valueknown and (not bast or bast.valueknown) then
        local ok, val = pcall(ops[opid], aast.value, bast and bast.value)
        if ok then
          ast.value = val; ast.valueknown = true
        else
          ast.value = val; ast.valueknown = false
        end
      end
    end
  end)
end


-- Label variables with unique identifiers.
-- Sets ast.id, ast.resolvedname
-- CATEGORY: code interpretation
function M.mark_identifiers(ast)
  local id = 0
  local seen_globals = {}
  LA.walk(ast, function(ast)
    if ast.tag == 'Id' or ast.isfield then
      if ast.localdefinition then
        if ast.localdefinition == ast then -- lexical definition
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
end


-- Environment in which to execute special comments (see below).
local env = setmetatable({}, {__index=_G})
env.context = env


-- Apply value to all identifiers with name matching pattern.
-- This command is callable inside special comments.
-- CATEGORY: code interpretation / special comment command
function env.apply_value(pattern, val)
  local function f(ast)
    if ast.tag == 'Id' and ast[1]:match(pattern) then
      ast.valueknown = true
      ast.value = val
    end
    for _,bast in ipairs(ast) do
      if type(bast) == 'table' then
        f(bast)
      end
    end
  end
  f(ast) -- ast from environment
  --UNUSED:
  -- for i=env.asti, #env.ast do
  --  local bast = env.ast[i]
  --  if type(bast) == 'table' then f(bast) end
  --end
end
setfenv(env.apply_value, env)


-- Evaluate all special comments (i.e. comments prefixed by '!') in code.
-- This is similar to luaanalyze.
-- CATEGORY: code interpretation / special comments
function M.eval_comments(ast, tokenlist)
  local function eval(command, ast)
    --DEBUG('!', command:gsub('%s+$', ''), ast.tag)
    local f, err = loadstring(command)
    if f then
      setfenv(f, env); env.ast = ast
      local ok, err = pcall(f, ast)
      if not ok then io.stderr:write(err, ': ', command) end
      env.ast = nil
   else
     local message = err, ': ', command
     if _G.scite and _G.scite.SendEditor then -- operating inside SciTE
       print(message) -- IMPROVE? eliminate editor-specific code
     else
       io.stderr:write(message, "\n")
     end
    end
  end

  for idx=1,#tokenlist do
    local token = tokenlist[idx]
    if token.tag == 'Comment' then
      local command = token[1]:match'^!(.*)'
      if command then
        local mast = LA.smallest_ast_in_range(ast, tokenlist, nil, token.fpos, token.lpos)
        eval(command, mast)
      end
    end
  end
end
--IMPROVE: in `do f() --[[!g()]] h()` only apply g to h.




-- Partially undoes effects of inspect().
-- Note: does not undo mark_tag2 and mark_parents (see replace_statements).
-- CATEGORY: code interpretation
function M.uninspect(top_ast)
  LA.walk(top_ast, function(ast)
    -- undo inspect_globals.globals
    ast.localdefinition = nil
    ast.functionlevel = nil
    ast.isparam = nil
    ast.isset = nil
    ast.isused = nil
    ast.isfield = nil
    ast.previous = nil
  
    -- undo mark_identifiers
    ast.id = nil
    ast.resolvedname = nil
    
    -- undo infer_values
    ast.value = nil
    ast.valueknown = nil
    ast.idxvalue = nil
    ast.idxvalueknown = nil
    
    -- undo walk setting ast.seevalue
    ast.seevalue = nil
    
    -- undo walk setting ast.definedglobal
    ast.definedglobal = nil
  end)
  
  -- undo infer_values
  top_ast.valueglobals = nil
end


-- Main inspection routine.
-- CATEGORY: code interpretation
function M.inspect(top_ast, tokenlist)
  --DEBUG: local t0 = os.clock()

  local globals = LG.globals(top_ast)
  
  M.mark_identifiers(top_ast)

  M.eval_comments(top_ast, tokenlist)
  
  M.infer_values(top_ast)
  M.infer_values(top_ast) -- two passes to handle forward declarations of globals (IMPROVE: more passes?)
  
  -- Make some nodes as having values related to its parent.
  -- This allows clicking on `bar` in `foo.bar` to display
  -- the value of `foo.bar` rather than just "bar".
  LA.walk(top_ast, function(ast)
    if ast.tag == "Index" then
      ast[2].seevalue = ast
    elseif ast.tag == "Invoke" then
      ast[2].seevalue = {value=ast.idxvalue, valueknown=ast.idxvalueknown}
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
  
  LA.walk(top_ast, function(ast)
    if ast.tag == 'Id' or ast.isfield then
      local vname = ast[1]
      --TODO: rename definedglobal to definedfield for clarity
      local atype = ast.localdefinition and 'local' or ast.isfield and 'field' or 'global'
      local definedglobal = ast.resolvedname and eval_name(ast.resolvedname) ~= nil or
                 atype == 'global' and (globals[vname] and globals[vname].set) or nil
      ast.definedglobal = definedglobal
      -- FIX: _G includes modules imported by inspect.lua, which is not desired
    end
  end)
end


return M
