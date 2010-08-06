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


local function DEBUG(...)
  if LUAINSPECT_DEBUG then
    print('DEBUG:', ...)
  end
end


-- Converts tokenlist to string representation for debugging.
function M.dump_tokenlist(tokenlist)
  local ts = {}
  for i,token in ipairs(tokenlist) do
    ts[#ts+1] = 'tok.' .. i .. ': [' .. token.fpos .. ',' .. token.lpos .. '] '
       .. tostring(token[1]) .. tostring(token.ast.tag)
  end
  return table.concat(ts, '\n')
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


-- My own object dumper.
-- Intended for debugging, not serialization, with compact formatting.
-- Robust against recursion.
-- Renders Metalua table tag fields specially {tag=X, ...} --> "`X{...}".
-- On first call, only pass parameter o.
local ignore_keys_ = {lineinfo=true, tag=true}
local norecurse_keys_ = {parent=true, ast=true}
local function dumpstring_key_(k, isseen, newindent)
  local ks = type(k) == 'string' and k:match'^[%a_][%w_]*$' and k or
             '[' .. M.dumpstring(k, isseen, newindent) .. ']'
  return ks
end
local function sort_keys_(a, b)
  if type(a) == 'number' and type(b) == 'number' then
    return a < b
  elseif type(a) == 'number' then
    return false
  elseif type(b) == 'number' then
    return true
  elseif type(a) == 'string' and type(b) == 'string' then
    return a < b
  else
    return tostring(a) < tostring(b) -- arbitrary
  end
end
function M.dumpstring(o, isseen, indent, key)
  isseen = isseen or {}
  indent = indent or ''

  if type(o) == 'table' then
    if isseen[o] or norecurse_keys_[key] then
      return (type(o.tag) == 'string' and '`' .. o.tag .. ':' or '') .. tostring(o)
    else isseen[o] = true end -- avoid recursion

    local tag = o.tag
    local s = (tag and '`' .. tag or '') .. '{'
    local newindent = indent .. '  '

    local ks = {}; for k in pairs(o) do ks[#ks+1] = k end
    table.sort(ks, sort_keys_)
    --for i,k in ipairs(ks) do print ('keys', k) end

    local forcenummultiline
    for k in pairs(o) do
       if type(k) == 'number' and type(o[k]) == 'table' then forcenummultiline = true end
    end

    -- inline elements
    local used = {}
    for _,k in ipairs(ks) do
      if ignore_keys_[k] then used[k] = true
      elseif (type(k) ~= 'number' or not forcenummultiline) and
              type(k) ~= 'table' and (type(o[k]) ~= 'table' or norecurse_keys_[k])
      then
        s = s .. dumpstring_key_(k, isseen, newindent) .. '=' .. M.dumpstring(o[k], isseen, newindent, k) .. ', '
        used[k] = true
      end
    end

    -- elements on separate lines
    local done
    for _,k in ipairs(ks) do
      if not used[k] then
        if not done then s = s .. '\n'; done = true end
        s = s .. newindent .. dumpstring_key_(k) .. '=' .. M.dumpstring(o[k], isseen, newindent, k) .. ',\n'
      end
    end
    s = s:gsub(',(%s*)$', '%1')
    s = s .. (done and indent or '') .. '}'
    return s
  elseif type(o) == 'string' then
    return string.format('%q', o)
  else
    return tostring(o)
  end
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


-- Gets common parent of aast and bast.  Always returns value.
-- Must provide root top_ast too.
function M.common_ast_parent(aast, bast, top_ast)
  if top_ast[1] and not top_ast[1].parent then M.mark_parents(top_ast) end
  local isparent = {}
  local tast = bast; repeat isparent[tast] = true; tast = tast.parent until not tast
  local uast = aast; repeat if isparent[uast] then return uast end; uast = uast.parent until not uast
  assert(false)
end

-- Gets smallest AST node inside top_ast/tokenlist/src
-- completely containing position range [pos1, pos2].
-- careful: "function" is not part of the `Function node.
-- If range is inside comment, returns comment also.
-- If corresponding source `src` is specified (may be nil)
-- and range is inside whitespace, then returns true in third return value.
--FIX: maybe src no longer needs to be passed
function M.smallest_ast_in_range(top_ast, tokenlist, src, pos1, pos2)
  local f0idx, l0idx = M.tokenlist_idx_range_over_pos_range(tokenlist, pos1, pos2)
  
  -- Find enclosing AST.
  if top_ast[1] and not top_ast[1].parent then M.mark_parents(top_ast) end
  local fidx, lidx = f0idx, l0idx
  while tokenlist[fidx] and not tokenlist[fidx].ast.parent do fidx = fidx - 1 end
  while tokenlist[lidx] and not tokenlist[lidx].ast.parent do lidx = lidx + 1 end
  -- DEBUG(fidx, lidx, f0idx, l0idx, #tokenlist, pos1, pos2, tokenlist[fidx], tokenlist[lidx])
  local ast = not (tokenlist[fidx] and tokenlist[lidx]) and top_ast or
      M.common_ast_parent(tokenlist[fidx].ast, tokenlist[lidx].ast, top_ast)
  -- DEBUG('m2', tokenlist[fidx], tokenlist[lidx], top_ast, ast, ast and ast.tag)
  if src and l0idx == f0idx - 1 then -- e.g.whitespace (FIX-currently includes non-whitespace too)
    local iswhitespace
    if pos2 == pos1 - 1  then -- zero length
      if src:sub(pos2, pos1):match'%s' then iswhitespace = true end -- either right or left %s
    elseif src:sub(pos1,pos2):match'^%s+$' then
      iswhitespace = true
    end
    if iswhitespace then
      return ast, nil, true
    else
      return ast, nil, nil
    end
  elseif l0idx == f0idx and tokenlist[l0idx].tag == 'Comment' then
    return ast, tokenlist[l0idx], nil
  else
    return ast, nil, nil
  end
end
--IMPROVE: handle string edits and maybe others


-- Calls mark_parents(ast) if ast not marked.
local function ensure_parents_marked(ast)
  if ast[1] and not ast[1].parent then M.mark_parents(ast) end
end


-- Gets smallest statement or block containing or being `ast`.
-- The AST root node `top_ast` must also be provided.
-- Note: may decorate AST as side-effect (mark_tag2/mark_parents).
-- top_ast is assumed a block, so this is always successful.
function M.get_containing_statementblock(ast, top_ast)
  if not top_ast.tag2 then M.mark_tag2(top_ast) end
  if ast.tag2 == 'Stat' or ast.tag2 == 'StatBlock' or ast.tag2 == 'Block' then
    return ast
  else
    ensure_parents_marked(top_ast)
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
-- Returns token list or nil if not applicable.  Returned `ast` is AST containing related keywords.
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
    if val1_ast.tag == 'Function' then
      local token = tokenlist[M.ast_idx_range_in_tokenlist(tokenlist, ast)]
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
    if not ast.parent then M.mark_parents(top_ast) end
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
              local token = tokenlist[M.ast_idx_range_in_tokenlist(tokenlist, cast)]
              keywords[#keywords+1] = token
            elseif cast.tag ~= 'Function' then f(cast) end
          end
        end
      end
      f(ast)
      if not ast.parent then M.mark_parents(top_ast) end
      local grand_ast = ast.parent.parent
      if grand_ast.tag == 'Set' then
        local token = tokenlist[M.ast_idx_range_in_tokenlist(tokenlist, grand_ast)]
        if token.tag == 'Keyword' and token[1] == 'function' then
          keywords[#keywords+1] = token
        end
      elseif grand_ast.tag == 'Localrec' then
        local tidx = M.ast_idx_range_in_tokenlist(tokenlist, grand_ast)
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
              local tidx = M.ast_idx_range_in_tokenlist(tokenlist, cast)
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
-- `src` to `bsrc`, given previous AST `top_ast` and tokenlist `tokenlist` corresponding to `src`.
-- note: decorates ast1 as side-effect
function M.invalidated_code(top_ast, tokenlist, src, bsrc)
  -- Converts posiiton range in src1 to position range in src2.
  local function range_transform(src_fpos, src_lpos)
    local src_nlpos = #src - src_lpos
    local bsrc_fpos = src_fpos
    local bsrc_lpos = #bsrc - src_nlpos
    return bsrc_fpos, bsrc_lpos
  end

  if src == bsrc then return end -- up-to-date
  
  local npre = longest_prefix(src, bsrc)
  local npost = math.min(#src-npre, longest_postfix(src, bsrc))
    -- note: min to avoid overlap ambiguity
    
  -- Find range of positions in src1 that differences correspond to.
  -- note: for zero byte range, src1_pos2 = src1_pos1 - 1.
  local src1_fpos, src1_lpos = 1 + npre, #src - npost
  
  -- Find smallest AST node in ast1 containing src1 range above,
  -- optionally contained comment or whitespace
  local match1_ast, match1_comment, iswhitespace =
      M.smallest_ast_in_range(top_ast, tokenlist, src, src1_fpos, src1_lpos)

  DEBUG('invalidate-smallest:', match1_ast and (match1_ast.tag or 'notag'), match1_comment, iswhitespace)

  if iswhitespace then
    local src2_fpos, src2_lpos = range_transform(src1_fpos, src1_lpos)
    if bsrc:sub(src2_fpos, src2_lpos):match'^%s*$' then -- whitespace replaced with whitespace
      if not bsrc:sub(src2_fpos-1, src2_lpos+1):match'%s' then
        DEBUG('edit:white-space-eliminated')
        -- whitespace eliminated, continue
      else
        return src1_fpos, src1_lpos, src2_fpos, src2_lpos, nil, 'whitespace'
      end
    end -- else continue
  elseif match1_comment then
    local src1m_fpos, src1m_lpos = match1_comment.fpos, match1_comment.lpos
    local src2m_fpos, src2m_lpos = range_transform(src1m_fpos, src1m_lpos)
    -- If new text is not a single comment, then invalidate containing statementblock instead.
    local m2text = bsrc:sub(src2m_fpos, src2m_lpos)
    DEBUG('inc-compile-comment[' .. m2text .. ']')
    if quick_parse_comment(m2text) then  -- comment replaced with comment
      return src1m_fpos, src1m_lpos, src2m_fpos, src2m_lpos, match1_comment, 'comment'
    end -- else continue
  else -- statementblock modified
    match1_ast = M.get_containing_statementblock(match1_ast, top_ast)
    local src1m_fpos, src1m_lpos = M.ast_pos_range(match1_ast, tokenlist)
    local src2m_fpos, src2m_lpos = range_transform(src1m_fpos, src1m_lpos)
    local m2text = bsrc:sub(src2m_fpos, src2m_lpos)
    DEBUG('inc-compile-statblock:', match1_ast and match1_ast.tag, '[' .. m2text .. ']')
    if loadstring(m2text) then -- statementblock replaced with statementblock 
      return src1m_fpos, src1m_lpos, src2m_fpos, src2m_lpos, match1_ast, 'statblock'
    end -- else continue
  end

  -- otherwise invalidate entire AST.
  -- IMPROVE:performance: we don't always need to invalidate the entire AST here.
  return nil, nil, nil, nil, top_ast, 'full'
end


-- Finds smallest statement, block, or comment AST  in ast/tokenlist containing position
-- range [fpos, lpos].  If allowexpand is true (default nil) and located AST
-- coincides with position range, then next containing statement is used
-- instead (this allows multiple calls to further expand the statement selection).
function M.select_statementblockcomment(ast, tokenlist, fpos, lpos, allowexpand)
--IMPROVE: rename ast to top_ast
  local match_ast, comment_ast = M.smallest_ast_in_range(ast, tokenlist, nil, fpos, lpos)
  local select_ast = comment_ast or M.get_containing_statementblock(match_ast, ast)
  local nfpos, nlpos = M.ast_pos_range(select_ast, tokenlist)
  --DEBUG('s', nfpos, nlpos, fpos, lpos, match_ast.tag, select_ast.tag)
  if allowexpand and fpos == nfpos and lpos == nlpos then
    if comment_ast then
      -- Select enclosing statement.
      select_ast = match_ast
      nfpos, nlpos = M.ast_pos_range(select_ast, tokenlist)
    else
      -- note: multiple times may be needed to expand selection.  For example, in
      --   `for x=1,2 do f() end` both the statement `f()` and block `f()` have
      --   the same position range.
      ensure_parents_marked(ast)
      while select_ast.parent and fpos == nfpos and lpos == nlpos do
        select_ast = M.get_containing_statementblock(select_ast.parent, ast)
        nfpos, nlpos = M.ast_pos_range(select_ast, tokenlist)
      end
    end
  end
  return nfpos, nlpos
end


-- Gets list of keyword positions related to node ast in source src
-- note: ast must be visible, i.e. have lineinfo (e.g. unlike `Id "self" definition).
-- Note: includes operators.
-- Note: Assumes ast Metalua-style lineinfo is valid.
function M.get_keywords(ast, src)
  local list = {}
  if not ast.lineinfo then return list end
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
      --DEBUG('first', ast.tag, first[3], src:sub(first[3], first[3]+3))
      lpos = (c and #c > 0 and c[1][2] or first[3]) - 1
    end
    
    -- Find keyword in range.
    local spos = fpos
    repeat
      local mfpos, tok, mlppos = src:match("^%s*()(%a+)()", spos)
      if not mfpos then
        mfpos, tok, mlppos = src:match("^%s*()(%p+)()", spos)
      end
      --DEBUG('look', ast.tag, #ast,i,j,'*', mfpos, tok, mlppos, fpos, lpos, src:sub(fpos, fpos+5))
      if mfpos and mlppos-1 <= lpos then
        list[#list+1] = mfpos
        list[#list+1] = mlppos-1
      end
      spos = mlppos
    until not spos or spos > lpos
    -- note: finds single keyword.  in `local function` returns only `local`
    --DEBUG(i,j ,'test[' .. src:sub(fpos, lpos) .. ']')
    
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


-- Set known value on ast to that on src_ast.
-- Utility function used by infer_values.
local function set_value(ast, src_ast)
  if not src_ast.valueknown or ast.valueknown and ast.value ~= src_ast.value then -- unknown if multiple values
    ast.valueknown = 'multiple'
  else
    ast.valueknown = src_ast.valueknown
    ast.value = src_ast.value
  end
end


-- Utility function used by infer_values.
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
        local mast = M.smallest_ast_in_range(ast, tokenlist, nil, token.fpos, token.lpos)
        eval(command, mast)
      end
    end
  end
end
--IMPROVE: in `do f() --[[!g()]] h()` only apply g to h.


-- Mark tokenlist (top_ast/tokenlist/src) with keywordid AST attributes.
-- All keywords related to each other have the same keyword ID integer.
-- NOTE: This is not done/undone by inspect/uninspect.
function M.mark_related_keywords(top_ast, tokenlist, src)
  local id = 0
  local idof = {}
  for _, token in ipairs(tokenlist) do
    if token.tag == 'Keyword' and not idof[token] then
      id = id + 1
      local match_ast =
        M.smallest_ast_in_range(top_ast, tokenlist, src, token.fpos, token.lpos)
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


-- Partially undoes effects of inspect().
-- Note: does not undo mark_tag2 and mark_parents (see replace_statements).
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
function M.inspect(top_ast, tokenlist)
  --DEBUG: local t0 = os.clock()

  local globals = inspect_globals.globals(top_ast)
  
  M.mark_identifiers(top_ast)

  M.eval_comments(top_ast, tokenlist)
  
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
end


-- Replaces contents of table t1 with contents of table t2.
-- Does not change metatable (if any).
-- This function is useful for swapping one AST node with another
-- while preserving any references to the node.
function M.switchtable(t1, t2)
  for k in pairs(t1) do t1[k] = nil end
  for k in pairs(t2) do t1[k] = t2[k] end
end


-- Generates ordered list of tokens in top_ast/src.
-- Note: currently ignores operators and parens.
-- Note: Modifies ast.
-- Note: Assumes ast Metalua-style lineinfo is valid.
local isterminal = {Nil=true, Dots=true, True=true, False=true, Number=true, String=true,
  Dots=true, Id=true}
local function compare_tokens_(atoken, btoken) return atoken.fpos < btoken.fpos end
function M.ast_to_tokenlist(top_ast, src)
  local tokens = {}
  local isseen = {}
  M.walk(top_ast, function(ast)
    if isterminal[ast.tag] then -- Extract terminal
      local token = ast
      token.fpos, token.lpos, token.ast = ast.lineinfo.first[3], ast.lineinfo.last[3], ast
      table.insert(tokens, token)
    else -- Extract non-terminal
      local keywordposlist = M.get_keywords(ast, src)
      for i=1,#keywordposlist,2 do
        local fpos, lpos = keywordposlist[i], keywordposlist[i+1]
        local toktext = src:sub(fpos, lpos)
        local token = {tag='Keyword', fpos=fpos, lpos=lpos, ast=ast, toktext}
        table.insert(tokens, token)
      end
    end
    -- Extract comments
    for i=1,2 do
      local comments = ast.lineinfo and ast.lineinfo[i==1 and 'first' or 'last'].comments
      if comments then for _, comment in ipairs(comments) do
        if not isseen[comment] then
          comment.tag = 'Comment'
          local token = comment
          token.fpos, token.lpos, token.ast = comment[2], comment[3], comment
          table.insert(tokens, token)
          isseen[comment] = true
        end
      end end
    end
    
  end)
  table.sort(tokens, compare_tokens_)
  return tokens
end


-- Gets tokenlist range [fidx,lidx] covered by ast/tokenlist.  Returns nil,nil if not found.
function M.ast_idx_range_in_tokenlist(tokenlist, ast)
  -- Get list of primary nodes under ast.
  local isold = {}; M.walk(ast, function(ast) isold[ast] = true end)
  -- Get range.
  local fidx, lidx
  for idx=1,#tokenlist do
    local token = tokenlist[idx]
    if isold[token.ast] then
      lidx = idx
      if not fidx then fidx = idx end
    end
  end
  return fidx, lidx
end


-- Get index range in tokenlist overlapped by character position range [fpos, lpos].
-- Partially overlapped tokens are included.  If position range between tokens, then fidx is last token and lidx is first token
-- (which implies lidx = fidx - 1)
function M.tokenlist_idx_range_over_pos_range(tokenlist, fpos, lpos)
  -- note: careful with zero-width range (lpos == fpos - 1)
  local fidx, lidx
  for idx=1,#tokenlist do
    local token = tokenlist[idx]
    --if (token.fpos >= fpos and token.fpos <= lpos) or (token.lpos >= fpos and token.lpos <= lpos) then -- token overlaps range
    if fpos <= token.lpos and lpos >= token.fpos then -- range overlaps token
      if not fidx then fidx = idx end
      lidx = idx
    end
  end
  if not fidx then -- on fail, check between tokens
    for idx=1,#tokenlist+1 do
      local tokfpos, toklpos = tokenlist[idx-1] and tokenlist[idx-1].lpos, tokenlist[idx] and tokenlist[idx].fpos
      if (not tokfpos or fpos > tokfpos) and (not toklpos or lpos < toklpos) then -- range between tokens
        return idx, idx-1
      end
    end
  end
  assert(fidx and lidx)
  return fidx, lidx
end

-- Remove tokens in tokenlist covered by ast. 
local function remove_ast_in_tokenlist(tokenlist, ast)
  local fidx, lidx  = M.ast_idx_range_in_tokenlist(tokenlist, ast)
  if fidx then  -- note: fidx implies lidx
    for idx=lidx,fidx,-1 do table.remove(tokenlist, idx) end
  end
end


-- Inserts all elements in list bt at index i in list t.
-- Utility function.
local function tinsertlist(t, i, bt)
  for bi=#bt,1,-1 do
    table.insert(t, i, bt[bi])
  end
end


-- Insert tokens from btokenlist into tokenlist.  Preserves sort.
local function insert_tokenlist(tokenlist, btokenlist)
  local ftoken = btokenlist[1]
  if ftoken then
    -- Get index in tokenlist in which to insert tokens in btokenlist.
    local fidx
    for idx=1,#tokenlist do
      if tokenlist[idx].fpos > ftoken.fpos then fidx = idx; break end
    end
    fidx = fidx or #tokenlist + 1  -- else append

    -- Insert tokens.
    tinsertlist(tokenlist, fidx, btokenlist)
  end
end


-- Get character position range covered by ast in tokenlist.  Returns nil,nil if not found
function M.ast_pos_range(ast, tokenlist)
  local fidx, lidx  = M.ast_idx_range_in_tokenlist(tokenlist, ast)
  if fidx then
    return tokenlist[fidx].fpos, tokenlist[lidx].lpos
    else
    return nil, nil
  end
end


-- Gets index of bast in ast (nil if not found).
function M.ast_idx(ast, bast)
  for idx=1,#ast do
    if ast[idx] == bast then return idx end
  end
  return nil
end


-- Gets parent of ast and index of ast in parent.
-- Root node top_ast must also be provided.  Returns nil, nil if ast is root.
-- Note: may call mark_parents.
function M.ast_parent_idx(top_ast, ast)
  if ast == top_ast then return nil, nil end
  if not ast.parent then M.mark_parents(top_ast) end; assert(ast.parent)
  local idx = M.ast_idx(ast.parent, ast)
  return ast.parent, idx
end


-- Replaces old_ast with new_ast/new_tokenlist in top_ast/tokenlist.
-- Note: assumes new_ast is a block.  assumes old_ast is a statement or block.
function M.replace_statements(top_ast, tokenlist, old_ast, new_ast, new_tokenlist)
  remove_ast_in_tokenlist(tokenlist, old_ast)
  insert_tokenlist(tokenlist, new_tokenlist)
  if old_ast == top_ast then -- special case: no parent
    M.switchtable(old_ast, new_ast) -- note: safe since block is not in tokenlist.
  else
    local parent_ast, idx = M.ast_parent_idx(top_ast, old_ast)
    table.remove(parent_ast, idx)
    tinsertlist(parent_ast, idx, new_ast)
  end

  -- fixup annotations
  for _,bast in ipairs(new_ast) do
    if top_ast.tag2 then M.mark_tag2(bast, bast.tag == 'Do' and 'StatBlock' or 'Block') end
    if old_ast.parent then M.mark_parents(bast, old_ast.parent) end
  end
end


-- Adjust lineinfo in tokenlist.
-- All char positions starting at pos1 are shifted by delta number of chars.
function M.adjust_lineinfo(tokenlist, pos1, delta)
  for _,token in ipairs(tokenlist) do
    if token.fpos >= pos1 then
       token.fpos = token.fpos + delta
    end
    if token.lpos >= pos1 then
      token.lpos = token.lpos + delta
    end
  end
end


--FIX:Q: does this handle Unicode ok?

--FIX:Metalua: Metalua bug here: In `local --[[x]] function --[[y]] f() end`, 'x' comment omitted from AST.

--Metalua:FIX: `do --[[x]] end` doesn't generate comments in AST.
--  `if x then --[[x]] end` and `while 1 do --[[x]] end` generates comments in first/last of block

--Metalua:FIX: `--[[x]] f() --[[y]]` returns lineinfo around `f()`.  `--[[x]] --[[y]]` returns lineinfo around everything.

--Metalua:FIX: `while 1 do --[[x]] --[[y]] end` returns first > last lineinfo for contained block

--Metalua:NOTE: `do  f()   end` returns lineinfo around `do  f()   end`, while
--  `while 1 do  f()  end` returns lineinfo around `f()` for inner block.

--Metalua:Q: Why does `return --[[y]]  z  --[[x]]` have lineinfo.first.comments, lineinfo.last.comments,
-- plus lineinfo.comments (which is the same as lineinfo.first.comments) ?

--Metalua:Q: loadstring parses "--x" but metalua omits the comment in the AST

-- CAUTION:Metalua: "local x" is generating `Local{{`Id{x}}, {}}`, which
-- has no lineinfo on {}.  This is contrary to the Metalua
-- spec: `Local{ {ident+} {expr+}? }.
-- Other things like `self` also generate no lineinfo.
-- The ast2.lineinfo test above avoids this.

--STYLE:Metalua: The lineinfo on Metalua comments is inconsistent with other nodes
        
--Metalua:FIX: Metalua shouldn't overwrite ipairs/pairs.  Note: Metalua version doesn't set
-- errorlevel correctly.

--Metalua:Q: lineinfo of table in `f{}` is [3,2], of `f{ x,y }` it's [4,6].
-- This is inconsistent with `x={}` which is [3,4] and `f""` which is [1,2] for the string.

-- NOTE:Metalua: only the `function()` form of `Function includes `function` in lineinfo.
-- 'function' is part of `Localrec and `Set in syntactic sugar form.

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




