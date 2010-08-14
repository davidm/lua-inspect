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

local LA = require "luainspect.ast"
local LG = require "luainspect.globals"
local LS = require "luainspect.signatures"

--! require 'luainspect.typecheck' (context)

-- Like info in debug.getinfo but inferred by static analysis.
-- object -> {fpos=fpos, source="@" .. source, fast=ast, tokenlist=tokenlist}
-- Careful: value may reference key (affects pre-5.2 which lacks emphemerons).
--   See also ast.nocollect.
M.debuginfo = setmetatable({}, {__mode='v'})

-- Modules loaded via require_inspect.
-- module name string -> AST node
-- note: no weak refs
M.package_loaded = {}

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


-- Read contents of text file in path, in binary mode.
-- On error, returns nil and error message.
local function readfile(path)
  local fh, err = io.open(path, 'rb')
  if fh then
    local data; data, err = fh:read'*a'
    if data then return data end
  end
  return nil, err
end

-- Loads source code of given module name.
-- Returns code followed by path.
-- CATEGORY: utility/package
local function load_module_source(name)
  for spec in package.path:gmatch'[^;]+' do
    local testpath = spec:gsub('%?', name:gsub('%.', '/'))
    local src, err_ = readfile(testpath)
    if src then return src, testpath end
  end
  return nil
end


-- Clears global state.
-- This includes cached inspected modules.
function M.clear_cache()
  for k,v in pairs(M.package_loaded) do
    M.package_loaded[k] = nil
  end
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
      if not ast.parent then LA.mark_parents(top_ast) end
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


-- Get expected number of parameters for function (min, max) values.
-- In case of vararg, max is unknown and set to nil.
local function function_param_range(ast)
  local names_ast = ast[1]
  if #names_ast >= 1 and names_ast[#names_ast].tag == 'Dots' then
    return #names_ast-1, nil
  else
    return #names_ast, #names_ast
  end
end

-- Gets number of arguments to function call: (min, max) range.
-- In case of trailing vararg or function call, max is unknown and set to nil.
local function call_arg_range(ast)
  if ast.tag == 'Invoke' then
    if #ast >= 3 and
      (ast[#ast].tag == 'Dots' or ast[#ast].tag == 'Call' or ast[#ast].tag == 'Invoke')
    then
      return #ast-2, nil
    else
      return #ast-1, #ast-1
    end    
  else
    if #ast >= 2 and
      (ast[#ast].tag == 'Dots' or ast[#ast].tag == 'Call' or ast[#ast].tag == 'Invoke')
    then
      return #ast-2, nil
    else
      return #ast-1, #ast-1
    end
  end
end


-- Reports warning. List of strings.
local function warn(...)
   if _G.scite and _G.scite.SendEditor then -- operating inside SciTE
     print('warning:', ...) -- IMPROVE? eliminate editor-specific code
   else
     io.stderr:write('warning:', ...); io.stderr:write'\n'
   end
end

-- Reports status messages. List of strings.
local function status(...)
   if _G.scite and _G.scite.SendEditor then -- operating inside SciTE
     print('status:', ...) -- IMPROVE? eliminate editor-specific code
   else
     io.stderr:write('status:', ...); io.stderr:write'\n'
   end
end



-- Version of require that does source analysis (inspect) on module.
function M.require_inspect(name)
  local ast = M.package_loaded[name]
  if ast then return ast end
  status('loading:' .. name)
  local msrc, mpath = load_module_source(name)
  local vinfo
  if msrc then
    local mast, err = LA.ast_from_string(msrc, mpath)
    if mast then
      local mtokenlist = LA.ast_to_tokenlist(mast, msrc)
      M.inspect(mast, mtokenlist)
      vinfo = mast[#mast] and mast[#mast].tag == 'Return' and mast[#mast][1]
        or {valueknown=true, value=nil}
      -- IMPROVE: return might not be last statement.
    else
      vinfo = {valueknown='error', value=err}
      warn(err, " ", mpath) --Q:error printing good?
    end
  else
    warn('module not found: ' .. name)
    vinfo = {valueknown='error', value='module not found'} --IMPROVE: include search paths?
  end
  M.package_loaded[name] = vinfo
  return vinfo
end

-- Infer values of variables.
--FIX/WARNING - this probably needs more work
-- Sets top_ast.valueglobals, ast.value, ast.valueknown, ast.idxvalue, ast.idxvalueknown
-- CATEGORY: code interpretation
local nil_value_ast = {}
function M.infer_values(top_ast, tokenlist)
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
        local found
        if func == require and ast[2].valueknown then
          local rast = M.require_inspect(ast[2].value)
          if rast and rast.valueknown then
            ast.valueknown, ast.value = rast.valueknown, rast.value
            found = true
          end
        end
        if not found and LS.safe_function[func] then
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
        local x
        local val = function() x=nil end
        local fpos = LA.ast_pos_range(ast, tokenlist)
        local source = ast.lineinfo.first[4] -- a HACK? relies on AST lineinfo
        local info = {fpos=fpos, source="@" .. source, fast=ast, tokenlist=tokenlist}
        M.debuginfo[val] = info
        ast.value = val
        ast.nocollect = info -- prevents garbage collection while ast exists
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
      if not ok then warn(err, ': ', command) end
      env.ast = nil
   else
     warn(err, ': ', command)
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
    ast.localmasked = nil
    ast.localmasking = nil
  
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

    -- undo notes
    ast.note = nil
    
    ast.nocollect = nil
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
  
  M.infer_values(top_ast, tokenlist)
  M.infer_values(top_ast, tokenlist) -- two passes to handle forward declarations of globals (IMPROVE: more passes?)
  
  -- Make some nodes as having values related to its parent.
  -- This allows clicking on `bar` in `foo.bar` to display
  -- the value of `foo.bar` rather than just "bar".
  LA.walk(top_ast, function(ast)
    if ast.tag == "Index" then
      ast[2].seevalue = ast
    elseif ast.tag == "Invoke" then
      ast[2].seevalue = {value=ast.idxvalue, valueknown=ast.idxvalueknown, parent=ast}
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
    elseif ast.tag == 'Call' or ast.tag == 'Invoke' then
      -- Argument count check.
      local value = ast.idxvalue or ast[1].value
      local info = M.debuginfo[value]
      local fast = info and info.fast
      if fast or LS.argument_counts[value] then
        local nparammin, nparammax
        if fast then
          nparammin, nparammax = function_param_range(info.fast)
        else
          nparammin, nparammax = unpack(LS.argument_counts[value])
        end
        local nargmin, nargmax = call_arg_range(ast)
        --print('DEBUG:', nparammin, nparammax, nargmin, nargmax)
        local iswarn
        local target_ast = ast.tag == 'Call' and ast[1] or ast[2]
        if (nargmax or math.huge) < nparammin then
          ast.note = "Too few arguments.  "
          iswarn = true
        elseif nargmin > (nparammax or math.huge) then
          ast.note = "Too many arguments.  "
          iswarn = true
        end
        if iswarn then
          ast.note = ast.note ..  "Expected "
            .. nparammin .. (nparammax == nparammin and "" or " to " .. (nparammax or "infinity"))
            .. " but got "
            .. nargmin .. (nargmax == nargmin and "" or " to " .. (nargmax or "infinity")) .. "."
        end
      end
    end
  end)
end


-- Resolve identifier to value [*]
function M.resolve_id(id, scope, valueglobals, _G)
  local val
  if scope[id] then
    val = scope[id].value
  elseif valueglobals[id] ~= nil then
    val = valueglobals[id]
  else
    val = _G[id] -- assumes not raise
  end
  return val
end

-- Resolve prefix chain expression to value. [*]
-- On error returns nil and error object
function M.resolve_prefixexp(ids, scope, valueglobals, _G)
  local val = M.resolve_id(ids[1], scope, valueglobals, _G)
  local ok, err = pcall(function()
    for i=2,#ids do
      val = val[ids[i]]
    end
  end)
  if err then return nil, err or '?' end
  return val
end

-- Get local scope at given 1-indexed char position
function M.get_scope(pos1, ast, tokenlist)
  local mast, isafter = LA.current_statementblock(ast, tokenlist, pos1)
  local scope = LG.variables_in_scope(mast, isafter)
  return scope
end

-- Gets names in prefix expression ids (as returned by resolve_prefixexp). [*]
function M.names_in_prefixexp(ids, pos, ast, tokenlist)
  local scope = M.get_scope(pos, ast, tokenlist)
  --FIX: above does not handle `for x=1,2 do| print(x) end` where '|' is cursor position.
  local names = {}
  if #ids == 0 then -- global
    for name in pairs(scope) do names[#names+1] = name end
    for name in pairs(ast.valueglobals) do names[#names+1] = name end
    for name in pairs(_G) do names[#names+1] = name end
  else  -- field
    local t, err_ = M.resolve_prefixexp(ids, scope, ast.valueglobals, _G)
    if type(t) == 'table' then  -- note: err_ implies false here
      for name in pairs(t) do names[#names+1] = name end
    end
  end
  return names
end

-- Gets signature (function argument string or helpinfo string) on value.
-- Returns nil on not found.
function M.get_signature_of_value(value)
  local info = M.debuginfo[value] -- first try this
  if info and info.fast then
    local fidx, lidx = LA.ast_idx_range_in_tokenlist(info.tokenlist, info.fast[1])
    local ts = {}
    if fidx then
      for i=fidx,lidx do
        local token = info.tokenlist[i]
        ts[#ts+1] = token.tag == 'Dots' and '...' or token[1]
      end
    end
    local sig = 'function(' .. table.concat(ts, ' ') .. ')'
    return sig
  end
  local sig = LS.value_signatures[value] -- else try this
  return sig
end


-- Gets signature (function argument string or helpinfo string) on variable ast.
-- Returns nil on not found.
function M.get_signature(ast)
  if ast.valueknown then
    return M.get_signature_of_value(ast.value)
  end
end


-- Gets 1-indexed character (or line) position and filename of
-- definition associated with AST node (if any).
function M.ast_to_definition_position(ast, tokenlist)
  local local_ast = ast.localdefinition
  local fpos, fline, path
  if local_ast then
    local tidx = LA.ast_idx_range_in_tokenlist(tokenlist, local_ast)
    if tidx then
      local spath = ast.lineinfo.first[4] -- a HACK? using lineinfo
      fpos = tokenlist[tidx].fpos; path = spath
    end
  end
  if not fpos then
    local valueast = ast.seevalue or ast
    local val = valueast and valueast.value
    local info = M.debuginfo[val] or type(val) == 'function' and debug.getinfo(val)
    if info then
      if info.source:match'^@' then
        path = info.source:match'@(.*)'
        if info.linedefined then
          fline = info.linedefined
        else
          fpos = info.fpos
        end
      end
    end
  end
  return fpos, fline, path
end


-- Returns true iff value in ast node is known in some way.
function M.is_known_value(ast)
  local vast = ast.seevalue or ast
  return vast.definedglobal or vast.valueknown and vast.value ~= nil
end


-- Get details information about value in AST node, as string.
function M.get_value_details(ast, tokenlist, src)
  local info = ""

  if not ast then return '?' end

  local vast = ast.seevalue or ast
  
  if ast.localdefinition then
    if not ast.localdefinition.isused then info = info .. "unused " end
    if ast.localdefinition.isset then info = info .. "mutable " end
    if ast.localdefinition.functionlevel < ast.functionlevel then
      info = info .. "upvalue "
    elseif ast.localdefinition.isparam then
      info = info .. "function parameter "
    else
      info = info .. "local "
    end

    if ast.localmasking then
      info = info .. "masking "
      local fpos = LA.ast_pos_range(ast.localmasking, tokenlist)
      if fpos then
        local linenum = LA.pos_to_linecol(fpos, src)
        info = info .. "definition at line " .. linenum .. " "
      end
    end
    if ast.localmasked then
      info = info .. "masked "
    end
  elseif ast.tag == 'Id' then -- global
    info = info .. (M.is_known_value(vast) and "known" or "unknown")
    info = info .. " global "
  elseif ast.isfield then
    info = info .. (M.is_known_value(vast) and "known" or "unknown")
    info = info .. " field "
  else
    info = info .. "? "
  end

  if vast.valueknown == 'multiple' then
    info = info .. "\nmultiple values including: " .. tostring(vast.value) .. " "
  elseif vast.valueknown then
    info = info .. "\nvalue: " .. tostring(vast.value) .. " "
  elseif vast.value then
    info = info .. "\nerror value: " .. tostring(vast.value) .. " "
  end -- else no info

 
  local sig = M.get_signature(vast)
  if sig then
    local kind = sig:find '%w%s*%b()$'  and 'signature' or 'description'
    info = info .. "\n" .. kind .. ": " .. sig .. " "
  end

  local fpos, fline, path = M.ast_to_definition_position(ast, tokenlist)
  if fpos or fline then
    local fcol
    if fpos then
      fline, fcol = LA.pos_to_linecol(fpos, src)
    end
    local location = path .. ":" .. (fline) .. (fcol and ":" .. fcol or "")
    info = info .. "\nlocation defined: " .. location .. " "
  end
  
  -- Render warning notes attached to calls/invokes.
  local note = vast.parent and (vast.parent.tag == 'Call' or vast.parent.tag == 'Invoke')
                    and vast.parent.note
  if note then
    info = info .. "\nWARNING: " .. note .. " "
  end
  
  return info
end


-- Gets list of all warnings, as strings.
-- In HTML Tidy format (which supports column numbers in SciTE, although is
-- slightly verbose and lacks filename).
function M.list_warnings(tokenlist, src)
  local warnings = {}
  local ttoken
  local function warn(msg)
    local linenum, colnum = LA.pos_to_linecol(ttoken.fpos, src)
    warnings[#warnings+1] = "line " .. linenum .. " column " .. colnum .. " - " .. msg
  end
  local isseen = {}
  for i,token in ipairs(tokenlist) do ttoken = token
    if token.ast then
      local ast = token.ast
      if ast.localmasking then
        local pos = LA.ast_pos_range(ast.localmasking, tokenlist)
        local linenum = pos and LA.pos_to_linecol(pos, src)
        warn("local " .. ast[1] .. " masks another local" .. (pos and " on line " .. linenum or ""))
      end
      if ast.localdefinition == ast and not ast.isused then
        warn("unused local " .. ast[1])
      end
      if ast.isfield and not(ast.seevalue.valueknown and ast.seevalue.value ~= nil) then
        warn("unknown field " .. ast[1])
      elseif ast.tag == 'Id' and not ast.localdefinition and not ast.definedglobal then
        warn("unknown global " .. ast[1])
      end
      local vast = ast.seevalue or ast
      local note = vast.parent and (vast.parent.tag == 'Call' or vast.parent.tag == 'Invoke')
                    and vast.parent.note
      if note and not isseen[vast.parent] then
        isseen[vast.parent] = true
        local etext = LA.ast_to_text(vast.parent, tokenlist, src)
           -- IMPROVE: large items like `f(function() ... end)` should be shortened.
        warn(note .. (etext and "for " .. etext or ""))
      end
    end
  end
  return warnings
end


return M
