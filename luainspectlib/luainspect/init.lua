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

--! require 'luainspect.typecheck' (context)

-- Variable naming conventions:
--  *ast - AST node

local function debug(...)
  print('DEBUG:', ...)
end


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


-- Gets smallest AST node inside AST `ast`
-- completely containing position range [pos1, pos2].
-- careful: "function" is not part of the `Function node.
-- If range is inside comment, returns comment also.
-- If corresponding source `src` is specified and range is inside whitespace, then
--    returns true in third return value.
local locs = {'first', 'last'}
function M.smallest_ast_in_range(ast, src, pos1, pos2)
  local partial
  for i,bast in ipairs(ast) do
    if type(bast) == 'table' and bast.lineinfo then
        -- CAUTION:Metalua: "local x" is generating `Local{{`Id{x}}, {}}`, which
        -- has no lineinfo on {}.  This is contrary to the Metalua
        -- spec: `Local{ {ident+} {expr+}? }.
        -- Other things like `self` also generate no lineinfo.
        -- The ast2.lineinfo test above avoids this.
      local apos1 = bast.lineinfo.first[3]
      local apos2 = bast.lineinfo.last[3]
      if pos1 >= apos1 and pos2 <= apos2 then
        return M.smallest_ast_in_range(bast, src, pos1, pos2)
      end
      if pos1 >= apos1 and pos1 <= apos2 or pos2 >= apos1 and pos2 <= apos2 then -- partial overlap
        partial = true
      end
      -- CAUTION:Metalua: For a block like `Do, it would be sufficient to examine the first
      --   comments from each member and the last comments from the last member.
      --   However, for `If, we need to examine the first and last comments from
      --   each member.  For simplicity both are always examined.
      for j=1,2 do
        local comments = bast.lineinfo[locs[j]].comments
        if comments then
          for _,comment in ipairs(comments) do
            local apos1, apos2 = comment[2], comment[3]
            if pos1 >= apos1 and pos2 <= apos2 then
              return ast, comment, nil
            end
            if pos1 >= apos1 and pos1 <= apos2 or pos2 >= apos1 and pos2 <= apos2 then -- partial overlap
              partial = true
            end
          end
        end
      end
    end
  end

  local iswhitespace
  if src and not partial and ast.tag ~= 'String' then
    if pos2 == pos1 - 1  then -- zero length
      if src:sub(pos2, pos1):match'%s' then iswhitespace = true end -- either right or left %s
    elseif src:sub(pos1,pos2):match'^%s+$' then
      iswhitespace = true
    end
    --FIX:QUESTION: relies on undefined string.sub behavior?
  end
  if iswhitespace then
    return ast, nil, true
  end
  return ast, nil, nil
end
--IMPROVE: handle string edits and maybe others


-- Gets smallest statement or block containing or being `ast`.
-- The AST root node `top_ast` must also be provided.
-- Note: may decorate AST as side-effect (mark_tag2/mark_parents).
-- top_ast is assumed a block, so this is always successful.
function M.get_containing_statementblock(ast, top_ast)
  if not top_ast.tag2 then M.mark_tag2(top_ast) end
  if ast.tag2 == 'Stat' or ast.tag2 == 'StatBlock' or ast.tag2 == 'Block' then
    return ast
  else
    if not ast.parent then M.mark_parents(top_ast) end
    return M.get_containing_statementblock(ast.parent, top_ast)
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
-- kposlist is list of begin/end positions of keywords.  Returned `ast` is AST containing related
-- keywords.
local iskeystat = {Do=true, While=true, Repeat=true, If=true, Fornum=true, Forin=true,
    Local=true, Localrec=true, Return=true, Break=true, Function=true,
    Set=true -- note: Set for `function name`
}
local isloop = {While=true, Repeat=true, Fornum=true, Forin=true}
local isblock = {Do=true, While=true, Repeat=true, If=true, Fornum=true, Forin=true, Function=true}
function M.related_keywords(ast, top_ast, src)
  -- Expand or contract AST for certain contained statements.
  if ast.tag == 'Return' then
    -- if `return` selected, that consider containing function selected (if any)
    if not ast.parent then M.mark_parents(top_ast) end
    local ancestor_ast = ast.parent
    while ancestor_ast ~= nil and ancestor_ast.tag ~= 'Function' do
      ancestor_ast = ancestor_ast.parent
    end
    if ancestor_ast then ast = ancestor_ast end -- but only change if exists
  elseif ast.tag == 'Break' then
    -- if `break` selected, that consider containing loop selected
    if not ast.parent then M.mark_parents(top_ast) end
    local ancestor_ast = ast.parent
    while ancestor_ast ~= nil and not isloop[ancestor_ast.tag] do
      ancestor_ast = ancestor_ast.parent
    end
    ast = ancestor_ast
  elseif ast.tag == 'Set' then
    local val1_ast = ast[2][1]
    if val1_ast.tag == 'Function' and src:sub(val1_ast.lineinfo.first[3], val1_ast.lineinfo.first[3]) ~= 'f' then
      -- if `function f` selected, which becomes `f = function .....` (i.e. a `Set), consider `Function node.
      -- NOTE:Metalua: only the `function()` form of `Function includes `function` in lineinfo.
      ast = ast[2][1]
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
    if not ast.parent then M.mark_parents(top_ast) end
    local ancestor_ast = ast
    while ancestor_ast ~= top_ast and not isblock[ancestor_ast.tag] do
      ancestor_ast = ancestor_ast.parent
    end
    ast = ancestor_ast    
  end

  --  keywords in statement/block.    
  if iskeystat[ast.tag] then
    local kposlist = M.get_keywords(ast, src)

    -- Expand keywords for certaining statements.
    if ast.tag == 'Function' then
      -- if `Function, also select 'function' and 'return' keywords
      local function f(ast)
        for _,cast in ipairs(ast) do
          if type(cast) == 'table' then
            if cast.tag == 'Return' then
              kposlist[#kposlist+1] = cast.lineinfo.first[3]
              kposlist[#kposlist+1] = cast.lineinfo.first[3] + #'return' - 1
            elseif cast.tag ~= 'Function'  then f(cast) end
          end
        end
      end
      if not ast.parent then M.mark_parents(top_ast) end
      local grand_ast = ast.parent.parent
      if grand_ast.tag == 'Set' and src:sub(ast.lineinfo.first[3], ast.lineinfo.first[3]) ~= 'f' then
        kposlist[#kposlist+1] = grand_ast.lineinfo.first[3]
        kposlist[#kposlist+1] = grand_ast.lineinfo.first[3] + #'function' - 1
      elseif grand_ast.tag == 'Localrec' and src:sub(ast.lineinfo.first[3], ast.lineinfo.first[3]) ~= 'f' then
        --FIX:Metalua: Metalua bug here: In `local --[[x]] function --[[y]] f() end`, 'x' comment omitted from AST.
        --FIX:HACK for now.
        local pos = src:match("()function", grand_ast.lineinfo.first[3]); assert(pos)
        kposlist[#kposlist+1] = pos
        kposlist[#kposlist+1] = pos + #'function' - 1
      end
      f(ast)
    elseif isloop[ast.tag] then
      -- if loop, also select 'break' keywords
      local function f(ast)
        for _,cast in ipairs(ast) do
          if type(cast) == 'table' then
            if cast.tag == 'Break' then
              kposlist[#kposlist+1] = cast.lineinfo.first[3]
              kposlist[#kposlist+1] = cast.lineinfo.first[3] + #'break' - 1
            elseif not isloop[cast.tag]  then f(cast) end
          end
        end
      end
      f(ast)        
    end
    
    return kposlist, ast
  end
  return nil, ast
end


-- Simple comment parser.  Returns Metalua-style comment.
local function quick_parse_comment(src)
  local s = src:match"^%-%-([^\n]*)()\n$"
  if s then return {s, 1, #src, 'short'} end
  local _, s = src:match(lexer.lexer.patterns.long_comment .. '\r?\n?$')
  if s then return {s, 1, #src, 'long'} end
  return nil
end
--FIX:check new-line correctness
--note: currently requiring \n at end of single line comment to avoid
-- incremental compilation with `--x\nf()` and removing \n from still
-- recognizing as comment `--x`.
-- currently allowing \r\n at end of long comment since Metalua includes
-- it in lineinfo of long comment (FIX:Metalua?)


-- Determines AST node that must be re-evaluated upon changing code string from
-- `src1` to `src2`, given previous AST `ast1` corresponding to `src1`.
-- note: decorates ast1 as side-effect
function M.invalidated_code(ast1, src1, src2)
  -- Converts posiiton range in src1 to position range in src2.
  local function range_transform(src1_fpos, src1_lpos)
    local src1_nlpos = #src1 - src1_lpos
    local src2_fpos = src1_fpos
    local src2_lpos = #src2 - src1_nlpos
    return src2_fpos, src2_lpos
  end

  if src1 == src2 then return end -- up-to-date
  
  local npre = longest_prefix(src1, src2)
  local npost = math.min(#src1-npre, longest_postfix(src1, src2))
    -- note: min to avoid overlap ambiguity
    
  -- Find range of positions in src1 that differences correspond to.
  -- note: for zero byte range, src1_pos2 = src1_pos1 - 1.
  local src1_fpos, src1_lpos = 1 + npre, #src1 - npost
  
  -- Find smallest AST node in ast1 containing src1 range above,
  -- optionally contained comment or whitespace
  local match1_ast, match1_comment, iswhitespace =
      M.smallest_ast_in_range(ast1, src1, src1_fpos, src1_lpos)
      
  print('DEBUG:invalidate-smallest:', match1_ast and (match1_ast.tag or 'notag'), match1_comment, iswhitespace)

  if iswhitespace then
    local src2_fpos, src2_lpos = range_transform(src1_fpos, src1_lpos)
    if src2:sub(src2_fpos, src2_lpos):match'^%s*$' then -- whitespace replaced with whitespace
      if not src2:sub(src2_fpos-1, src2_lpos+1):match'%s' then
        debug('edit:white-space-eliminated')
        -- whitespace eliminated, continue
      else
        return src1_fpos, src1_lpos, src2_fpos, src2_lpos, nil, 'whitespace'
      end
    end -- else continue
  elseif match1_comment then
    local src1m_fpos, src1m_lpos = match1_comment[2], match1_comment[3]
    local src2m_fpos, src2m_lpos = range_transform(src1m_fpos, src1m_lpos)
    -- If new text is not a single comment, then invalidate containing statementblock instead.
    local m2text = src2:sub(src2m_fpos, src2m_lpos)
    print('DEBUG:inc-compile[' .. m2text .. ']')
    if quick_parse_comment(m2text) then  -- comment replaced with comment
      return src1m_fpos, src1m_lpos, src2m_fpos, src2m_lpos, match1_comment, 'comment'
    end -- else continue
  else -- statementblock modified
    match1_ast = M.get_containing_statementblock(match1_ast, ast1)
    local src1m_fpos, src1m_lpos = match1_ast.lineinfo.first[3], match1_ast.lineinfo.last[3]  
    local src2m_fpos, src2m_lpos = range_transform(src1m_fpos, src1m_lpos)
    local m2text = src2:sub(src2m_fpos, src2m_lpos)
    if loadstring(m2text) then -- statementblock replaced with statementblock 
      return src1m_fpos, src1m_lpos, src2m_fpos, src2m_lpos, match1_ast, 'statblock'
    end -- else continue
  end

  -- otherwise invalidate entire AST.
  -- IMPROVE:performance: we don't always need to invalidate the entire AST here.
  return nil, nil, nil, nil, ast1, 'full'
end


local locs = {'first', 'last'}
function M.mark_alllineinfo(ast)
  if ast.alllineinfo then return end -- already done
  local alllineinfo = {}
  M.walk(ast, function(ast)
    if ast.lineinfo then
      for i=1,2 do
        local lineinfo = ast.lineinfo[locs[i]]
        alllineinfo[lineinfo] = true -- DEBUG:locs[i]..tostring(ast.tag)
        if lineinfo.comments then
          for _, comment in ipairs(lineinfo.comments) do
            alllineinfo[comment] = true
            comment.tag = 'Comment' -- HACK:Metalua to handle comment lineinfo inconsistency
          end
        end
      end      
    end
  end)
  ast.alllineinfo = alllineinfo
end

-- Finds smallest statement, block, or comment AST containing position range
-- [fpos, lpos].  If allowexpand is true (default nil) and located AST
-- coincides with position range, then next containing statement is used
-- instead (this allows multiple calls to further expand the statement selection).
function M.select_statementblockcomment(ast, fpos, lpos, allowexpand)
  local match_ast, comment_ast = M.smallest_ast_in_range(ast, nil, fpos, lpos)
  local select_ast = comment_ast or M.get_containing_statementblock(match_ast, ast)
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
        select_ast = M.get_containing_statementblock(select_ast.parent, ast)
        nfpos, nlpos = select_ast.lineinfo.first[3], select_ast.lineinfo.last[3]
      end
    end
  end
  return nfpos, nlpos
end


-- Gets list of keyword positions related to node ast in source src
-- note: ast must be visible, i.e. have lineinfo.
-- FIX/IMPROVE: `function` might be treated as a special case.
function M.get_keywords(ast, src)
  assert(ast.lineinfo)
  local list = {}
  -- examine space between each pair of children i and j.
  -- special cases: 0 is before first child and #ast+1 is after last child
  local i = 0
  while i <= #ast do
    -- j is node following i that has lineinfo
    local j = i+1; while j < #ast+1 and not ast[j].lineinfo do j=j+1 end

    -- Get position range [fpos,lpos] between subsequent children.
    local fpos
    if i == 0 then  -- before first child
      fpos = ast.lineinfo.first[3]
    else
      local last = ast[i].lineinfo.last; local c = last.comments
      fpos = (c and #c > 0 and c[#c][3] or last[3]) + 1
    end
    local lpos
    if j == #ast+1 then  -- after last child
      lpos = ast.lineinfo.last[3]
    else
      local first = ast[j].lineinfo.first; local c = first.comments
      lpos = (c and #c > 0 and c[1][2] or first[3]) - 1
    end
    
    -- Find keyword in range.
    local mfpos, debug, mlppos = src:match("()(%a+)()", fpos)
    if mfpos and mlppos-1 <= lpos then
      list[#list+1] = mfpos
      list[#list+1] = mlppos-1
    end
    -- note: finds single keyword.  in `local function` returns only `local`
    --print(i,j ,'test[' .. src:sub(fpos, lpos) .. ']')
    
    i = j  -- next
   
    --DESIGN:Lua: comment: string.match accepts a start position but not a stop position
  end
  return list
end
-- Q:Metalua: does ast.lineinfo[loc].comments imply #ast.lineinfo[loc].comments > 0 ?

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
function M.mark_parents(ast, parent_ast)
  ast.parent = parent_ast
  for _,ast2 in ipairs(ast) do
    if type(ast2) == 'table' then
      M.mark_parents(ast2, ast)
    end
  end
end


-- For each node n in ast, set n.tag2 to context string:
-- 'Block' - node is block
-- 'Stat' - node is statement
-- 'StatBlock' - node is statement and block (i.e. `Do)
-- 'Exp' - node is expression
-- 'Explist' - node is expression list (or identifier list)
-- 'Pair' - node is key-value pair in table constructor
-- note: ast.tag2 will be set to context.
local iscertainstat = {Do=true, Set=true, While=true, Repeat=true, If=true,
  Fornum=true, Forin=true, Local=true, Localrec=true, Return=true, Break=true}
function M.mark_tag2(ast, context)
  context = context or 'Block'
  ast.tag2 = context
  for i,ast2 in ipairs(ast) do
    if type(ast2) == 'table' then
      local nextcontext
      if ast2.tag == 'Do' then
        nextcontext = 'StatBlock'
      elseif iscertainstat[ast2.tag] then
        nextcontext = 'Stat'
      elseif ast2.tag == 'Call' or ast2.tag == 'Invoke' then
        nextcontext = context == 'Block' and 'Stat' or 'Exp'
        --DESIGN:Metalua: these calls actually contain expression lists,
        --  but the expression list is not represented as a complete node
        --  by Metalua (as blocks are in `Do statements)
      elseif ast2.tag == 'Pair' then
        nextcontext = 'Pair'
      elseif not ast2.tag then
        if ast.tag == 'Set' or ast.tag == 'Local' or ast.tag == 'Localrec'
          or ast.tag == 'Forin' and i <= 2
          or ast.tag == 'Function'  and i == 1
        then
          nextcontext = 'Explist'
        else 
          nextcontext = 'Block'
        end
      else
        nextcontext = 'Exp'
      end
      M.mark_tag2(ast2, nextcontext)
    end
  end
end


-- Create notes on AST.
-- Notes are sorted by character position and contain semantic information on tokens.
function M.create_notes(ast)
  local notes = {}

  local seen_comment = {}
  M.walk(ast, function(ast)
    if (ast.tag == 'Id' or ast.isfield) and ast.lineinfo then -- note: e.g. `Id "self" may have no lineinfo
      local vname = ast[1]
      local fchar = ast.lineinfo.first[3]
      local lchar = ast.lineinfo.last[3]
      local atype = ast.localdefinition and 'local' or ast.isfield and 'field' or 'global'
      
      local isparam = ast.localdefinition and ast.localdefinition.isparam or nil
      table.insert(notes, {fchar, lchar, ast=ast, type=atype, definedglobal=ast.definedglobal, isparam=isparam})
      --IMPROVE: definedglobal and other fields are redundant
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


local function set_value(ast, src_ast)
  if not src_ast.valueknown or ast.valueknown and ast.value ~= src_ast.value then -- unknown if multiple values
    ast.valueknown = 'multiple'
  else
    ast.valueknown = src_ast.valueknown
    ast.value = src_ast.value
  end
end


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


-- for debugging
local function debugvalue(ast)
  local s
  if ast then
    s = ast.valueknown and 'known:' .. tostring(ast.value) or 'unknown'
  else
    s = '?'
  end
  return s
end


--FIX/WARNING - this probably needs more work
-- Sets top_ast.valueglobals, ast.value, ast.valueknown, ast.idxvalue, ast.idxvalueknown
local nil_value_ast = {}
function M.infer_values(top_ast)
  if not top_ast.valueglobals then top_ast.valueglobals = {} end

  -- infer values
  M.walk(top_ast, nil, function(ast)
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
    elseif ast.tag == "Id" then
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
    elseif ast.tag == "Index" then
      local t_ast, k_ast = ast[1], ast[2]
      if t_ast.valueknown and k_ast.valueknown then
        ast.valueknown, ast.value = pcall(tindex, t_ast.value, k_ast.value)
      end
    elseif ast.tag == "Call" then
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
    elseif ast.tag == "Invoke" then
      local t_ast, k_ast = ast[1], ast[2]
      if t_ast.valueknown and k_ast.valueknown then
        ast.idxvalueknown, ast.idxvalue = pcall(tindex, t_ast.value, k_ast.value)
      end

      -- note: similar to "Call" code
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
    elseif ast.tag == "String" or ast.tag == "Number" then
      ast.value = ast[1]; ast.valueknown = true
    elseif ast.tag == "True" or ast.tag == "False" then
      ast.value = (ast.tag == "True"); ast.valueknown = true
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
    elseif ast.tag == "Paren" then
      ast.value = ast[1].value; ast.valueknown = ast[1].valueknown
    elseif ast.tag == "Op" then
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
function M.mark_identifiers(ast)
  local id = 0
  local seen_globals = {}
  M.walk(ast, function(ast)
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
  for i=env.asti, #env.ast do
    local bast = env.ast[i]
    if type(bast) == 'table' then f(bast) end
  end
end
setfenv(env.apply_value, env)


-- Evaluate all special comments (i.e. comments prefixed by '!') in code.
-- This is similar to luaanalyze.
function M.eval_comments(ast)
  for i, bast in ipairs(ast) do
    if bast.lineinfo and bast.lineinfo.first.comments then
      for i, comment in ipairs(bast.lineinfo.first.comments) do
        local text = comment[1]
        local command = text:match'^!(.*)'
        if command then
          local f, err = loadstring(command)
          if f then
            setfenv(f, env); env.ast = ast; env.asti = i
            local ok, err = pcall(f, ast)
            if not ok then io.stderr:write(err, ': ', text) end
            env.ast = nil; env.asti = nil
          else
            io.stderr:write(err, ': ', text)
          end
        end
      end
    end
  end
end


-- Partially undoes effects of inspect().
-- Note: does not undo mark_tag2 and mark_parents (see replace_ast).
function M.uninspect(top_ast)
  M.walk(top_ast, function(ast)
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
function M.inspect(top_ast)
  --DEBUG: local t0 = os.clock()

  local globals = inspect_globals.globals(top_ast)
  
  M.mark_identifiers(top_ast)

  M.eval_comments(top_ast)
  
  M.infer_values(top_ast)
  M.infer_values(top_ast) -- two passes to handle forward declarations of globals (IMPROVE: more passes?)
  
  -- Make some nodes as having values related to its parent.
  -- This allows clicking on `bar` in `foo.bar` to display
  -- the value of `foo.bar` rather than just "bar".
  M.walk(top_ast, function(ast)
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
  
  local valueglobals = top_ast.valueglobals
  
  M.walk(top_ast, function(ast)
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
  

  local notes = M.create_notes(top_ast)

  --print('DEBUG: t+', os.clock() - t0); t0 = os.clock()

  return notes
end


-- Replaces contents of table t1 with contents of table t2.
-- Does not change metatable (if any).
-- This function is useful for swapping one AST node with another
-- while preserving any references to the node.
function M.switchtable(t1, t2)
  for k in pairs(t1) do t1[k] = nil end
  for k in pairs(t2) do t1[k] = t2[k] end
end

local ispropagatecommentsfirst = {
  Set=true, Op=true, Call=true, Invoke=true, Index=true }
local ispropagatecommentslast = {
  Set=true, Repeat=true, Local=true, Op=true
}
  
local function make_comments(ast, dir, comments)
  if not ast.lineinfo then ast.lineinfo = {first={}, last={}} end
  if not ast.lineinfo[dir].comments then
    comments = comments or {}
    ast.lineinfo[dir].comments = comments
    local ispropagatecomments = dir=='first' and ispropagatecommentsfirst or ispropagatecommentslast
    if ast.tag == nil or ispropagatecomments[ast.tag] then
      if type(ast[1]) == 'table' then make_comments(ast[1], dir, comments) end
    end
  end
end


-- Set comments to dir ('first' or 'last') side of ast.  Propagates to children as needed as well.
local function set_comments(ast, dir, comments)
  if comments then
    if not ast.lineinfo then ast.lineinfo = {first={}, last={}} end
  end
  if ast.lineinfo then
    ast.lineinfo[dir].comments = comments
  end
  local ispropagatecomments = dir=='first' and ispropagatecommentsfirst or ispropagatecommentslast
  if ast.tag == nil or ispropagatecomments[ast.tag] then
    if type(ast[1]) == 'table' then set_comments(ast[dir=='first' and 1 or #ast], dir, comments) end
  end
end

local function get_comments(ast, dir)
  return ast.lineinfo and ast.lineinfo[dir].comments
end

local function link_comments(ast, dir, bast, bdir)
  local comments = get_comments(bast, bdir)
  set_comments(ast, dir, comments)
end


local function compare_comments_(a, b) return a[3] < b[3] end
local function add_comments(ast, dir, bast, bdir, cdir)
  local bcomments = get_comments(bast, bdir)
  if bcomments then
    make_comments(ast, dir)
    local comments = ast.lineinfo[dir].comments
    for i,comment in ipairs(bcomments) do
      if cdir == 'first' then
        table.insert(comments, i, comment)
      else -- 'last' or 'sort'
        table.insert(comments, comment)
      end
    end
    if cdir == 'sort' then table.sort(comments, compare_comments_) end
  end
end


local function break_comments(ast, dir, bast, bdir, pos)
  local comments = ast.lineinfo and ast.lineinfo[dir].comments
  if comments then
    local lastcomments = {}
    for i = #comments,1,-1 do
      if comments[i][3] >= pos then
        table.insert(lastcomments, table.remove(comments, i))
      end
    end
    set_comments(ast, dir, comments)
    set_comments(bast, bdir, lastcomments)
  end
  
end

-- Remove statement at position idx in block ast.
local function remove_statement_in_block(ast, idx)
  if ast[idx-1] and ast[idx+1] then -- between statements
    add_comments(ast[idx-1], 'last', ast[idx+1], 'first', 'last')
    link_comments(ast[idx+1], 'first', ast[idx-1], 'last')
  elseif ast[idx-1] then -- last statement
    add_comments(ast, 'last', ast[idx-1], 'last', 'first')
    link_comments(ast[idx-1], 'last', ast, 'last')
  elseif ast[idx+1] then -- first statement
  
    add_comments(ast, 'first', ast[idx+1], 'first', 'last')
    link_comments(ast[idx+1], 'first', ast, 'first')
  else -- only statement
    add_comments(ast, 'first', ast, 'last', 'last')
    link_comments(ast, 'last', ast, 'first')
  end
  table.remove(ast, idx)
       
end


-- Add statements in block bast before position idx in block ast.
local function insert_statements_in_block(ast, bast, idx)
  -- Get two nodes to insert between
  local first_ast, first_dir, last_ast, last_dir
  if #ast == 0 then
    first_ast, first_dir, last_ast, last_dir = ast, 'first', ast, 'last'
  elseif idx == 1 then
    first_ast, first_dir, last_ast, last_dir = ast, 'first', ast[1], 'first'
  elseif idx == #ast+1 then
    first_ast, first_dir, last_ast, last_dir = ast[#ast], 'last', ast, 'last'
  else
    first_ast, first_dir, last_ast, last_dir = ast[idx-1], 'last', ast[idx], 'first'
  end
  assert(first_ast.lineinfo[first_dir].comments == last_ast.lineinfo[last_dir].comments)
  
  -- Add comments on ends of bast to comment list between above two nodes.
  add_comments(first_ast, first_dir, bast, 'first', 'sort')
  if #bast > 0 then add_comments(first_ast, first_dir, bast, 'last', 'sort') end

  -- Insert bast nodes into ast
  if #bast > 0 then
    break_comments(first_ast, first_dir, last_ast, last_dir, bast.lineinfo.first[3])
    link_comments(bast[1], 'first', first_ast, first_dir)
    link_comments(bast[#bast], 'last', last_ast, last_dir)
    for _, bbast in ipairs(bast) do
      table.insert(ast, idx, bbast)
    end
  end
end

-- Replaces old_ast with new_ast in top_ast.
-- Note: assumes new_ast is a block.  assumes old_ast is a statement or block.
function M.replace_ast(top_ast, old_ast, new_ast)
  if not top_ast.tag2 then M.mark_tag2(top_ast) end; assert(old_ast.tag2)
  if old_ast.tag2 == 'Block' then
    if #old_ast == 0 then
      --Q:OK?
    else
      add_comments(new_ast, 'first', old_ast, 'first', 'first')
      add_comments(new_ast, 'last', old_ast, 'last', 'last')
    end

    local old_parent = old_ast.parent
    M.switchtable(old_ast, new_ast)

    -- fixup annotations
    M.mark_tag2(old_ast, 'Block')
    if old_ast.parent then M.mark_parents(old_ast, old_parent) end
  elseif old_ast.tag2 == 'Stat' or old_ast.tag2 == 'StatBlock' then
    if not old_ast.parent then M.mark_parents(top_ast) end; assert(old_ast.parent)    
    assert(old_ast.parent.tag2 == 'Block' or old_ast.parent.tag2 == 'StatBlock')
    -- find index in parent
    local ii
    for i=1, #old_ast.parent do
      if old_ast.parent[i] == old_ast then ii = i; break end
    end
    assert(ii)
    
    
    local block_ast = old_ast.parent
    assert(#block_ast > 0, 'NOT IMPL') -- ever happen?
    
    remove_statement_in_block(block_ast, ii)
    insert_statements_in_block(block_ast, new_ast, ii)
    -- fixup annotations
    for i=1,#new_ast do
      M.mark_tag2(new_ast[i], new_ast[i].tag == 'Do' and 'StatBlock' or 'Stat')
      M.mark_parents(new_ast[i], block_ast)
    end

  else
    error 'not implemented'
  end
end


--FIX: adjust rows/cols too?
function M.adjust_lineinfo(ast, pos1, delta)
  M.mark_alllineinfo(ast)
  for lineinfo in pairs(ast.alllineinfo) do
    if lineinfo.tag == 'Comment' then
      if lineinfo[2] >= pos1 then
         lineinfo[2] = lineinfo[2] + delta
         lineinfo[3] = lineinfo[3] + delta
         --NOTE: linenum will also need to be adjusted here if Metalua adds linenum to comment node.
      end
    else  -- first or last of normal tag
      if lineinfo[3] >= pos1 then
        lineinfo[3] = lineinfo[3] + delta
      end
    end
  end
end

function M.adjust_notes(notes, pos1, delta)
  for i=1,#notes do local note = notes[i]
    if note[1] >= pos1 then
      note[1] = note[1] + delta
      note[2] = note[2] + delta
    end
  end
end


--Metalua:FIX: `do --[[x]] end` doesn't generate comments in AST.
--  `if x then --[[x]] end` and `while 1 do --[[x]] end` generates comments in first/last of block

--Metalua:FIX: `--[[x]] f() --[[y]]` returns lineinfo around `f()`.  `--[[x]] --[[y]]` returns lineinfo around everything.

--Metalua:FIX: `while 1 do --[[x]] --[[y]] end` returns first > last lineinfo for contained block

--Metalua:NOTE: `do  f()   end` returns lineinfo around `do  f()   end`, while
--  `while 1 do  f()  end` returns lineinfo around `f()` for inner block.

--Metalua:Q: Why does `return --[[y]]  z  --[[x]]` have lineinfo.first.comments, lineinfo.last.comments,
-- plus lineinfo.comments (which is the same as lineinfo.first.comments) ?

--Metalua:Q: loadstring parses "--x" but metalua omits the comment in the AST

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

-- test longest_prefix/longest_postfix
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




