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

-- Variable naming conventions:
--  *ast - AST node

-- Gets length of longest prefix string in both provided strings.
-- Returns max n such that text1:sub(1,n) == text2:sub(1,n) and n <= max(#text1,#text2)
local function longest_prefix(text1, text2)
  local nmin = 0
  local nmax = math.min(#text1, #text2)
  while nmax > nmin do
    local nmid = math.ceil((nmin+nmax)/2)
    if text1:sub(1,nmid) == text2:sub(1,nmid) then
      nmin = nmid
    else
      nmax = nmid-1
    end
  end
  return nmin
end


-- Gets length of longest postfix string in both provided strings.
-- Returns max n such that text1:sub(-n) == text2:sub(-n) and n <= max(#text1,#text2)
local function longest_postfix(text1, text2)
  local nmin = 0
  local nmax = math.min(#text1, #text2)
  while nmax > nmin do
    local nmid = math.ceil((nmin+nmax)/2)
    if text1:sub(-nmid) == text2:sub(-nmid) then --[*]
      nmin = nmid
    else
      nmax = nmid-1
    end
  end
  return nmin
end  -- differs from longest_prefix only on line [*]


-- Gets smallest AST node inside AST `ast` completely containing position range [pos1, pos2].
-- careful: "function" is not part of the `Function node.
local locs = {'first', 'last'}
function M.smallest_ast_in_range(ast, pos1, pos2)
  for i,ast2 in ipairs(ast) do
    if type(ast2) == 'table' then
      local apos1 = ast2.lineinfo.first[3]
      local apos2 = ast2.lineinfo.last[3]
      if pos1 >= apos1 and pos2 <= apos2 then
        return M.smallest_ast_in_range(ast2, pos1, pos2, ast)
      end
      -- CAUTION:Metalua: For a block like `Do, it would be sufficient to examine the first
      --   comments from each member and the last comments from the last member.
      --   However, for `If, we need to examine the first and last comments from
      --   each member.  For simplicity both are always examined.
      for j=1,2 do
        local comments = ast2.lineinfo[locs[j]].comments
        if comments then
          for _,comment in ipairs(comments) do
	    local apos1, apos2 = comment[2], comment[3]
	    if pos1 >= apos1 and pos2 <= apos2 then
              return ast, comment
            end
	  end
	end
      end
    end
  end
  return ast, nil
end


-- Gets smallest statement containing or being `ast`.
-- The AST root node `top_ast` must also be provided.
-- Note: may decorate AST as side-effect.
local isstatement = {Do=true, Set=true, While=true, Repeat=true, If=true,
  Fornum=true, Forin=true, Local=true, Localrec=true, Return=true, Break=true}
function M.get_containing_statement(ast, top_ast)
  if not ast.tag or isstatement[ast.tag]  -- block or stat
  then
    return ast
  else
    if not ast.parent then
      M.mark_parents(top_ast)
    end
    if (ast.tag == 'Call' or ast.tag == 'Invoke') and
      (not ast.parent.tag or ast.parent.tag == 'Do')
    then
      -- is apply statement (i.e. call/invoke inside block or stat)
      return ast
    else
      return M.get_containing_statement(ast.parent)
    end
  end
end


-- Determines AST node that must be re-evaluated upon changing code string from
-- `src1` to `src2`, given previous AST `ast1` corresponding to `src1`.
-- Returns both position range [src2_pos1,src2_pos2] in `src2` and matching
-- AST node `match1_ast` in src1.
-- note: decorates ast1 as side-effect
function M.invalidated_code(ast1, src1, src2)
  if src1 == src2 then return end
  local npre = longest_prefix(src1, src2)
  local npost = math.min(#src1-npre, longest_postfix(src1, src2))
    -- note: min to avoid overlap ambiguity
    
  -- Find range of positions in src1 that differences correspond to.
  -- note: for zero byte range, src1_pos2 = src1_pos1 - 1.
  local src1_pos1 = 1 + npre
  local src1_pos2 = #src1 - npost
  
  -- Find smallest AST node in ast1 containing src1 range above.
  local match1_ast, match1_comment = M.smallest_ast_in_range(ast1, src1_pos1, src1_pos2, nil)

  if not match1_comment then
    match1_ast = M.get_containing_statement(match1_ast, ast1)
  end
  local src1_pos1m = match1_comment and match1_comment[2] or match1_ast.lineinfo.first[3]
  local src1_pos2m = match1_comment and match1_comment[3] or match1_ast.lineinfo.last[3]
  local src1_npost = #src1 - src1_pos2m
  
  -- Find range of positions in src2 that match_ast corresponds to.
  local src2_pos1 = src1_pos1m
  local src2_pos2 = #src2 - src1_npost
  
  return src2_pos1, src2_pos2, match1_ast
end


-- Finds smallest statement or comment AST containing position range
-- [fpos, lpos].  If allowexpand is true (default nil) and located AST
-- coincides with position range, then next containing statement is used
-- instead (this allows multiple calls to further expand the statement selection).
function M.select_statement(ast, fpos, lpos, allowexpand)
  local match_ast, comment_ast = M.smallest_ast_in_range(ast, fpos, lpos)
  local select_ast = comment_ast or M.get_containing_statement(match_ast, ast)
  local nfpos = comment_ast and select_ast[2] or select_ast.lineinfo.first[3]
  local nlpos = comment_ast and select_ast[3] or select_ast.lineinfo.last[3]
    --STYLE:Metalua: The lineinfo on Metalua comments is inconsistent with other nodes
  if allowexpand and fpos == nfpos and lpos == nlpos then
    if comment_ast then
      -- Select enclosing statement.
      select_ast = match_ast
      nfpos, nlpos = select_ast.lineinfo.first[3], select_ast.lineinfo.last[3]
    else
      -- note: multiple times may be needed to expand selection.  For example, in
      --   `for x=1,2 do f() end` both the statement `f()` and block `f()` have
      --   the same position range.
      while select_ast ~= ast and fpos == nfpos and lpos == nlpos do
        if not select_ast.parent then M.mark_parents(ast) end -- ensure ast.parent
	select_ast = M.get_containing_statement(select_ast.parent, ast)
	nfpos, nlpos = select_ast.lineinfo.first[3], select_ast.lineinfo.last[3]
      end
    end
  end
  return nfpos, nlpos
end


-- Remove any sheband ("#!") line from Lua source string.
function M.remove_shebang(src)
  local shebang = src:match("^#![^\r\n]*")
  return shebang and (" "):rep(#shebang) .. src:sub(#shebang+1) or src
end


-- Custom version of loadstring that parses out line number info
function M.loadstring(src)
  local f, err = loadstring(src, "")
  if f then
    return f
  else
    err = err:gsub('^%[string ""%]:', "")
    local linenum = assert(err:match("(%d+):"))
    local colnum = 0
    local linenum2 = err:match("^%d+: '[^']+' expected %(to close '[^']+' at line (%d+)")
    return nil, err, linenum, colnum, linenum2
  end
end


-- helper for ast_from_string.  Raises on error.
-- FIX? filename currently ignored in Metalua
local function ast_from_string_helper(src, filename)
  filename = filename or '(string)'
  local  lx  = mlp.lexer:newstream (src, filename)
  local  ast = mlp.chunk(lx)
  return ast
end


-- Converts Lua source string to Lua AST (via mlp/gg).
function M.ast_from_string(src, filename)
  local ok, ast = pcall(ast_from_string_helper, src, filename)
  if not ok then
    local err = ast
    err = err:match('[^\n]*')
    err = err:gsub("^.-:%s*line", "line")
        -- mlp.chunk prepending this is undesirable.   error(msg,0) would be better in gg.lua. Reported.
	-- TODO-Metalua: remove when fixed in Metalua.
    local linenum, colnum = err:match("line (%d+), char (%d+)")
    if not linenum then
      -- Metalua libraries may return "...gg.lua:56: .../mlp_misc.lua:179: End-of-file expected"
      -- without the normal line/char numbers given things like "if x then end end".  Should be
      -- fixed probably with gg.parse_error in _chunk in mlp_misc.lua.
      -- TODO-Metalua: remove when fixed in Metalua.
      linenum = editor.LineCount - 1
      colnum = 0
    end
    local linenum2 = nil
    return nil, err, linenum, colnum, linenum2
  else
    return ast
  end
end
	

-- Walks AST `ast` in arbitrary order, visiting each node `n`, executing `fdown(n)` (if specified)
-- when doing down and `fup(n)` (if specified) when going if.
function M.walk(ast, fdown, fup)
  assert(type(ast) == 'table')
  if fdown then fdown(ast) end
  for _,bast in ipairs(ast) do
    if type(bast) == 'table' then
      M.walk(bast, fdown, fup)
    end
  end
  if fup then fup(ast) end
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


-- For each node n in ast, set n.parent to parent node of n.
-- Assumes ast.parent will be parent_ast (may be nil)
local special = {Forin=3, Function=2}
function M.mark_parents(ast, parent_ast)
  ast.parent = parent_ast
  if special[ast.tag] then
    local ishallow = special[ast.tag]
    for i, t in ipairs(ast) do
      if i == ishallow then
        local ast2 = ast[i]
        M.mark_parents(ast2, ast)
      else
        for _, ast2 in ipairs(t) do M.mark_parents(ast2, ast) end
      end
    end
  elseif ast.tag == 'Set' or ast.tag == 'Local' or ast.tag == 'Localrec' then
    for _, t in ipairs(ast) do
    for _, ast2 in ipairs(t) do
      M.mark_parents(ast2, ast)
    end end
  else
    for _,t_or_ast2 in ipairs(ast) do
      if type(t_or_ast2) == 'table' then
        local ast2 = t_or_ast2
        M.mark_parents(ast2, ast)
      end
    end
  end
  --STYLE:Metalua: the above intentionally avoids treating expression lists as
  --  parent nodes.  QUESTION: on second thought, does this code really want/need this?
  --  This is currently done to allow easy detection of whether a `Call or `Invoke is
  --  a statement or expression (if statement then ast.parent.tag is nil or 'Do'
  --  (see get_containing_statement).
end

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
        local ok, val = pcall(ops[opid], aast.value, bast and bast.value)
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


--[=[TESTSUITE
-- utilities
local ops = {}
ops['=='] = function(a,b) return a == b end
local function check(opname, a, b)
  local op = assert(ops[opname])
  if not op(a,b) then
    error("fail == " .. tostring(a) .. " " .. tostring(b))
  end
end

-- test longtest_prefix/longest_postfix
local function pr(text1, text2)
  local lastv
  local function same(v)
    assert(not lastv or v == lastv); lastv = v; return v
  end
  local function test1(text1, text2) -- test prefix/postfix
    same(longest_prefix(text1, text2))
    same(longest_postfix(text1:reverse(), text2:reverse()))
  end
  local function test2(text1, text2) -- test swap
    test1(text1, text2)
    test1(text2, text1)
  end
  for _,extra in ipairs{"", "x", "xy", "xyz"} do -- test extra chars
    test2(text1, text2..extra)
    test2(text2, text1..extra)
  end
  return lastv
end
check('==', pr("",""), 0)
check('==', pr("a",""), 0)
check('==', pr("a","a"), 1)
check('==', pr("ab",""), 0)
check('==', pr("ab","a"), 1)
check('==', pr("ab","ab"), 2)
check('==', pr("abcdefg","abcdefgh"), 7)

print 'DONE'
--]=]


return M




