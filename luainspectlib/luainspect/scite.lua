-- luainspect.scite - SciTE text editor plugin
--
-- (c) 2010 David Manura, MIT License.

-- Whether to update the AST on every edit (true) or only when the selection
-- is moved to a different line (false).  false can be more efficient for large files.
local UPDATE_ALWAYS = scite_GetProp('luainspect.update.always', '1') == '1'

-- Experimental feature: display types/values of all known locals as annotations.
-- Allows Lua to be used like a Mathcad worksheet.
local ANNOTATE_ALL_LOCALS = scite_GetProp('luainspect.annotate.all.locals', '0') == '1'

-- When user edits code, recompile only the portion of code that is edited.
-- This can improve performance and normally should be true unless you find problems.
local INCREMENTAL_COMPILATION = scite_GetProp('luainspect.incremental.compilation', '1') == '1'

-- Whether to run timing tests (for internal development purposes).
local PERFORMANCE_TESTS = scite_GetProp('luainspect.performance.tests', '0') == '1'

local LI = require "luainspect.init"
local LS = require "luainspect.signatures"

local M = {}

--! require 'luainspect.typecheck' (context)

-- variables stored in `buffer`:
-- ast -- last successfully compiled AST
-- text  -- text corresponding to `ast`
-- lasttext  -- last attempted `text` (might not be successfully compiled)
-- tokenlist  -- tokenlist corresponding to `ast`
-- lastline - number of last line in scite_OnUpdateUI (only if not UPDATE_ALWAYS)


-- Performance test utilities.  Enabled only for PERFORMANCE_TESTS.
local perf_names = {}
local perf_times = {os.clock()}
local nilfunc = function() end
local clock = PERFORMANCE_TESTS and function(name)
  perf_times[#perf_times+1] = os.clock()
  perf_names[#perf_names+1] = name
end or nilfunc
local clockbegin = PERFORMANCE_TESTS and function(name)
  perf_names = {}
  perf_times = {}
  clock(name)
end or nilfunc
local clockend = PERFORMANCE_TESTS and function(name)
  clock(name)
  for i=1,#perf_times do
    print('DEBUG:clock:', perf_names[i], perf_times[i] - perf_times[1])
  end
end or nilfunc


-- Debug utility function:
-- Shorten string by replacing any long middle section with "..."
local _pat
local function debug_shorten(s)
  local keep_pat = ("."):rep(100)
  _pat = _pat or "^(" .. keep_pat .. ").*(" .. keep_pat .. ")$"
  return s:gsub(_pat, "%1\n<...>\n%2")
end

local function DEBUG(...)
  if LUAINSPECT_DEBUG then
    print('DEBUG:', ...)
  end
end


-- Style IDs - correspond to style properties
local S_DEFAULT = 0
local S_LOCAL = 1
local S_LOCAL_MUTATE = 6
local S_LOCAL_UNUSED = 7
local S_LOCAL_PARAM = 8
local S_UPVALUE = 10
local S_UPVALUE_MUTATE = 15
local S_GLOBAL_RECOGNIZED = 2
local S_GLOBAL_UNRECOGNIZED = 3
local S_FIELD = 11
local S_FIELD_RECOGNIZED = 12
local S_COMMENT = 4
local S_STRING = 5
local S_TAB = 13
local S_KEYWORD = 14
local S_COMPILER_ERROR = 9
local STYLES = {}
STYLES.default = S_DEFAULT
STYLES['local'] = S_LOCAL
STYLES.local_mutate = S_LOCAL_MUTATE
STYLES.local_unused = S_LOCAL_UNUSED
STYLES.local_param = S_LOCAL_PARAM
STYLES.upvalue = S_UPVALUE
STYLES.upvalue_mutate = S_UPVALUE_MUTATE
STYLES.global_recognized = S_GLOBAL_RECOGNIZED
STYLES.global_unrecognized = S_GLOBAL_UNRECOGNIZED
STYLES.field = S_FIELD
STYLES.field_recognized = S_FIELD_RECOGNIZED
STYLES.comment = S_COMMENT
STYLES.string = S_STRING
STYLES.tab = S_TAB
STYLES.keyword = S_KEYWORD
STYLES.compiler_error = S_COMPILER_ERROR
STYLES.indic_fore = 'indic_fore'
STYLES.indic_style = 'indic_style'

local function formatvariabledetails(token)
  local info = ""
  local ast = token.ast

  if not ast then return '?' end
  
  if ast.tag == 'Id' and not ast.localdefinition then -- global
    info = info .. (ast.definedglobal and "recognized" or "unrecognized") .. " global "
  elseif ast.localdefinition then
    if not ast.localdefinition.isused then
      info = info .. "unused "
    end
    if ast.localdefinition.isset then
      info = info .. "mutable "
    end
    if ast.localdefinition.functionlevel < ast.functionlevel then
      info = info .. "upvalue "
    elseif ast.localdefinition.isparam then
      info = info .. "param "
    end
    info = info .. "local "
    if ast.ismasking then
      info = info .. "masking "
    end
  elseif ast.isfield then
    info = info .. "field "
    if ast.definedglobal then info = info .. "recognized " else info = info .. "unrecognized " end
  else
    info = info .. "? "
  end

  if ast.resolvedname and LS.global_signatures[ast.resolvedname] then
    local name = ast.resolvedname
    info = LS.global_signatures[name] .. "\n" .. info
  end
 
  local vast = ast.seevalue or ast
  if vast.valueknown == 'multiple' then
    info = info .. "\nmultiple values including= " .. tostring(vast.value) .. " "
  elseif vast.valueknown then
    info = info .. "\nvalue= " .. tostring(vast.value) .. " "
  elseif vast.value then
    info = info .. "\nerror value= " .. tostring(vast.value) .. " "
  end -- else no info
  return info
end


-- Used for ANNOTATE_ALL_LOCALS feature.
local function annotate_all_locals()
  -- Build list of annotations.
  local annotations = {}
  for i=1,#buffer.tokenlist do
    local token = buffer.tokenlist[i]
    if token.ast.localdefinition == token.ast then
      local info = formatvariabledetails(token)
      local linenum = editor:LineFromPosition(token.lpos-1)
      annotations[linenum] = (annotations[linenum] or "") .. "detail: " .. info
    end
  end
  -- Apply annotations.
  editor.AnnotationVisible = ANNOTATION_BOXED
  for linenum=0,table.maxn(annotations) do
    if annotations[linenum] then
      editor.AnnotationStyle[linenum] = S_DEFAULT
      editor:AnnotationSetText(linenum, annotations[linenum])
    end
  end
end


-- Attempt to update AST from editor text and apply decorations.
local function update_ast()
  -- Skip update if text unchanged.
  local newtext = editor:GetText()
  if newtext == buffer.lasttext then
    return false
  end
  buffer.lasttext = newtext
  clockbegin 't1'

  local err, linenum, colnum, linenum2
  
  -- Update AST.
  if newtext == buffer.text then -- returned to previous good version
    -- note: nothing to do besides display
  else  
   -- note: loadstring and metalua don't parse shebang
   local newtextm = LI.remove_shebang(newtext)

   -- Quick syntax check.   
   -- loadstring is much faster than Metalua, so try that first.
   -- Furthermore, Metalua accepts a superset of the Lua grammar.
   local f; f, err, linenum, colnum, linenum2 = LI.loadstring(newtextm)

   -- Analyze code using LuaInspect, and apply decorations
   if f then
    -- Select code to compile.
    local isincremental = INCREMENTAL_COMPILATION and buffer.ast
    local pos1f, pos1l, pos2f, pos2l, old_ast, old_type, compiletext
    if isincremental then
      pos1f, pos1l, pos2f, pos2l, old_ast, old_type =
          LI.invalidated_code(buffer.ast, buffer.tokenlist, LI.remove_shebang(buffer.text), newtextm)
      compiletext = old_type == 'full' and newtextm or newtextm:sub(pos2f,pos2l)
      DEBUG('inc', pos1f, pos1l, pos2f, pos2l, old_ast, old_type )
      DEBUG('inc-compile:[' .. debug_shorten(compiletext)  .. ']', old_ast and (old_ast.tag or 'notag'), old_type, pos1f and (pos2l - pos1l), pos1l, pos2f)
    else
      compiletext = newtextm
    end
    clock 't2'

    -- Generate AST.
    local ast
    if old_type ~= 'whitespace' then
      --currently not needed: compiletext = compiletext .. '\n' --FIX:Workaround:Metalua:comments not postfixed by '\n' ignored.
      ast, err, linenum, colnum, linenum2 = LI.ast_from_string(compiletext, "noname.lua")
      --DEBUG(table.tostring(ast, 20))
    end
    clock 't3'

    if err then
      print "warning: metalua failed to compile code that compiles with loadstring.  error in metalua?"
    else
      local tokenlist = ast and LI.ast_to_tokenlist(ast, compiletext)
        -- note: ast nil if whitespace
      --LI.dump_tokenlist(tokenlist)
      
   
      buffer.text = newtext
      if isincremental and old_type ~= 'full' then
        -- Adjust line numbers.
        local delta = pos2l - pos1l
        LI.adjust_lineinfo(buffer.tokenlist, pos1l, delta)
        if ast then
          LI.adjust_lineinfo(tokenlist, 1, pos2f-1)
        end
 
        -- Inject AST
        if old_type == 'whitespace' then
          -- nothing
        elseif old_type == 'comment' then
          assert(#tokenlist == 1 and tokenlist[1].tag == 'Comment') -- replacing with comment
          local newcommenttoken = tokenlist[1]
          local token = old_ast
          token.fpos, token.lpos, token[1], token[4] =
              newcommenttoken.fpos, newcommenttoken.lpos, newcommenttoken[1], newcommenttoken[4]
        else assert(old_type == 'statblock')
          LI.replace_statements(buffer.ast, buffer.tokenlist, old_ast, ast, tokenlist)
        end

        if not(old_type == 'comment' or old_type == 'whitespace') then
          LI.uninspect(buffer.ast)
          LI.inspect(buffer.ast, buffer.tokenlist) --IMPROVE: don't do full inspection
        end
      else --full
            -- old(FIX-REMOVE?): careful: if `buffer.tokenlist` variable exists in `newtext`, then
      --   `LI.inspect` may attach its previous value into the newly created
      --   `buffer.tokenlist`, eventually leading to memory overflow.
      
        buffer.tokenlist = tokenlist
        buffer.ast = ast
        LI.inspect(buffer.ast, buffer.tokenlist)
      end
      if LUAINSPECT_DEBUG then
        DEBUG(LI.dump_tokenlist(buffer.tokenlist))
        DEBUG(LI.dumpstring(buffer.ast))
        --DEBUG(table.tostring(buffer.ast, 20))
      end
    end
   end
  end
  clockend 't4'
  
  -- Apply styling
  if err then
     local pos = linenum and editor:PositionFromLine(linenum-1) + colnum - 1
     --old: editor:CallTipShow(pos, err)
     --old: editor:BraceHighlight(pos,pos) -- highlight position of error (hack: using brace highlight)
     editor.IndicatorCurrent = 0
     editor:IndicatorClearRange(0, editor.Length)
     editor:IndicatorFillRange(pos, 1) --IMPROVE:mark entire token?
     editor:MarkerDefine(0, SC_MARK_CHARACTER+33) -- '!'
     editor:MarkerSetFore(0, 0xffffff)
     editor:MarkerSetBack(0, 0x0000ff)
     editor:MarkerDeleteAll(0)
     editor:MarkerAdd(linenum-1, 0)
     editor:AnnotationClearAll()
     editor.AnnotationVisible = ANNOTATION_BOXED
     editor.AnnotationStyle[linenum-1] = S_COMPILER_ERROR
     editor:AnnotationSetText(linenum-1, "error " .. err)
     if linenum2 then -- display error in two locations
       editor.AnnotationStyle[linenum2-1] = S_COMPILER_ERROR
       editor:AnnotationSetText(linenum2-1, "error " .. err)
     end
     return
  else
    
    --old: editor:CallTipCancel()
    editor.IndicatorCurrent = 0
    editor:IndicatorClearRange(0, editor.Length)
    editor:MarkerDeleteAll(0)
    editor:AnnotationClearAll()

    if ANNOTATE_ALL_LOCALS then annotate_all_locals() end
  end
end


-- Gets note assocated with currently selected variable (if any).
local function getselectedvariable()
  if buffer.text ~= editor:GetText() then return end  -- skip if AST not up-to-date
  local selectedtoken
  local id
  local pos = editor.Anchor+1
  for i,token in ipairs(buffer.tokenlist) do
    if pos >= token.fpos and pos <= token.lpos then
      if token.ast.id then
        selectedtoken = token
        id = token.ast.id
      end
      break
    end
  end
  return selectedtoken, id
end


-- Mark in margin range of 0-indexed lines.
local function scope_lines(firstline0, lastline0)
  if firstline0 ~= lastline0 then
    --TODO: not rendering exactly as desired.  TCORNERCURVE should
    -- preferrably be an upside-down LCORNERCURVE; plus the color on TCORNERCURVE is off.
    editor:MarkerDefine(1, SC_MARK_TCORNERCURVE)
    editor:MarkerDefine(2, SC_MARK_VLINE)
    editor:MarkerDefine(3, SC_MARK_LCORNERCURVE)
    editor:MarkerSetFore(1, 0x0000ff)
    editor:MarkerSetFore(2, 0x0000ff)
    editor:MarkerSetFore(3, 0x0000ff)

    editor:MarkerAdd(firstline0, 1)
    for n=firstline0+1,lastline0-1 do
      editor:MarkerAdd(n, 2)
    end
    editor:MarkerAdd(lastline0, 3)
  else
    editor:MarkerDefine(2, SC_MARK_VLINE)
    editor:MarkerSetFore(2, 0x0000ff)
    editor:MarkerAdd(firstline0, 2)
  end
end


-- Mark in margin range of 0-indexed positions.
local function scope_positions(fpos0, lpos0)
  local firstline0 = editor:LineFromPosition(fpos0-1)
  local lastline0 = editor:LineFromPosition(lpos0-1)
  scope_lines(firstline0, lastline0)
end

local function init_indicator_styles()
  local indic_style = props["style.script_lua.indic_style"]
  editor.IndicStyle[1] = indic_style == '' and INDIC_ROUNDBOX or indic_style
  editor.IndicStyle[2] = INDIC_PLAIN
  local indic_fore = props["style.script_lua.indic_fore"]
  if indic_fore ~= '' then
    local color = tonumber(indic_fore:sub(2), 16)
    editor.IndicFore[1] = color
    editor.IndicFore[2] = color
  end
end


-- Respond to UI updates.  This includes moving the cursor.
scite_OnUpdateUI(function()
  -- FIX: how do we make this event only occur for Lua buffers?
  -- Hack below probably won't work with multiple Lua-based lexers.
  if editor.Lexer ~= 0 then return end

  -- This updates the AST when the selection is moved to a different line.
  if not UPDATE_ALWAYS then
    local currentline = editor:LineFromPosition(editor.Anchor)
    if currentline ~= buffer.lastline then
      update_ast()
      buffer.lastline = currentline
    end
  end

  if buffer.text ~= editor:GetText() then return end -- skip if AST is not up-to-date
  
  -- check if selection if currently on identifier
  local selectednote, id = getselectedvariable()

  --test: adding items to context menu upon variable selection
  --if id then
  --  props['user.context.menu'] = selectednote.ast[1] .. '|1101'
  --  --Q: how to reliably remove this upon a buffer switch?
  --end

  -- Highlight all instances of that identifier.
  editor:MarkerDeleteAll(1)
  editor:MarkerDeleteAll(2)
  editor:MarkerDeleteAll(3)
  editor.IndicatorCurrent = 1
  editor:IndicatorClearRange(0, editor.Length)
  if id then
    init_indicator_styles() --Q: how often need this be called?

    local ftoken, ltoken -- first and last occurances
    for _,token in ipairs(buffer.tokenlist) do
      if token.ast.id == id then
        ltoken = token
        if not ftoken then ftoken = token end
        editor:IndicatorFillRange(token.fpos-1, token.lpos-token.fpos+1)
      end
    end

    scope_positions(ftoken.fpos-1, ltoken.lpos-1)
  end
  
  -- Highlight related keywords.
  do
    editor.IndicatorCurrent = 2
    editor:IndicatorClearRange(0, editor.Length)

    -- Check for selection over statement or expression.
    local fpos, lpos = editor.Anchor, editor.CurrentPos
    if lpos < fpos then fpos, lpos = lpos, fpos end -- swap
    fpos, lpos = fpos + 1, lpos + 1 - 1
    local match1_ast, match1_comment, iswhitespace =
      LI.smallest_ast_in_range(buffer.ast, buffer.tokenlist, buffer.text, fpos, lpos)
    -- DEBUG('m', match1_ast and match1_ast.tag, match1_comment, iswhitespace)

    -- Find and highlight.
    local keywords; keywords, match1_ast = LI.related_keywords(match1_ast, buffer.ast, buffer.tokenlist, buffer.text)
    if keywords then
      for i=1,#keywords do
        local fpos, lpos = keywords[i].fpos, keywords[i].lpos
        editor:IndicatorFillRange(fpos-1, lpos-fpos+1)
      end
    end
    
    -- Mark range of lines covered by item on selection.
    if not id then
      local fpos, lpos = LI.ast_pos_range(match1_ast, buffer.tokenlist)
      if fpos then scope_positions(fpos, lpos) end
    end
  end


  --[[
  -- Display callinfo help on function.
  if selectednote and selectednote.ast.resolvedname and LS.global_signatures[selectednote.ast.resolvedname] then
    local name = selectednote.ast.resolvedname
    editor:CallTipShow(editor.Anchor, LS.global_signatures[name])
  else
    --editor:CallTipCancel()
  end
  ]]
end)


-- Respond to requests for restyling.
-- Note: if StartStyling is not applied over the entire requested range, than this function is quickly recalled
--   (which possibly can be useful for incremental updates)
local n = 0
local isblock = {Function=true}
local function OnStyle(styler)
  if styler.language ~= "script_lua" then return end -- avoid conflict with other stylers

  --if n == 0 then n = 2 else n = n - 1; return end -- this may improves performance on larger files only marginally
  --IMPROVE: could metalua libraries parse text across multiple calls to `OnStyle` to reduce long pauses with big files?

  --DEBUG("style",styler.language, styler.startPos, styler.lengthDoc, styler.initStyle)

  -- update AST if needed
  if UPDATE_ALWAYS then
    update_ast()
  elseif not buffer.lasttext then
    -- this ensures that AST compiling is attempted when file is first loaded since OnUpdateUI
    -- is not called on load.
    update_ast()
  end

  --DEBUG('OnStyle', editor:LineFromPosition(styler.startPos), editor:LineFromPosition(styler.startPos+styler.lengthDoc), styler.initStyle)
  if buffer.text ~= editor:GetText() then return end  -- skip if AST not up-to-date
    -- note: SciTE will repeatedly call OnStyle until StartStyling is performed.
    -- However, StartStyling clears styles in the given range, but we prefer to leave
    -- the styles as is.
 
  -- Apply SciTE styling
  editor.StyleHotSpot[S_LOCAL] = true
  editor.StyleHotSpot[S_LOCAL_MUTATE] = true
  editor.StyleHotSpot[S_LOCAL_UNUSED] = true
  editor.StyleHotSpot[S_LOCAL_PARAM] = true
  editor.StyleHotSpot[S_UPVALUE] = true
  editor.StyleHotSpot[S_UPVALUE_MUTATE] = true
  editor.StyleHotSpot[S_GLOBAL_RECOGNIZED] = true
  editor.StyleHotSpot[S_GLOBAL_UNRECOGNIZED] = true
  editor.StyleHotSpot[S_FIELD] = true
  editor.StyleHotSpot[S_FIELD_RECOGNIZED] = true
  -- note: SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK currently aren't
  -- implemented by SciTE, although it has been proposed.

  local startpos0 = 0
  styler:StartStyling(startpos0, editor.Length, 0)
  -- local startpos0 = styler.startPos
  --styler:StartStyling(styler.startPos, styler.lengthDoc, styler.initStyle)
  --   a partial range like this doesn't work right since variables outside of edited range
  --   may need styling adjusted (e.g. a local variable definition that becomes unused)

  local i=startpos0+1
  local tokenidx = 1
  local token = buffer.tokenlist[tokenidx]
  local function nexttoken() tokenidx = tokenidx+1; token = buffer.tokenlist[tokenidx] end
  while styler:More() do
    while token and i > token.lpos do
      nexttoken()
    end
    
    if token and i >= token.fpos and i <= token.lpos then
      local ast = token.ast
      if token.tag == 'Id' then
        if ast.localdefinition then -- local
          if not ast.localdefinition.isused then
            styler:SetState(S_LOCAL_UNUSED)
          elseif ast.localdefinition.functionlevel  < ast.functionlevel then  -- upvalue
            if ast.localdefinition.isset then
              styler:SetState(S_UPVALUE_MUTATE)
            else
              styler:SetState(S_UPVALUE)
            end
          elseif ast.localdefinition.isset then
            styler:SetState(S_LOCAL_MUTATE)
          elseif ast.localdefinition.isparam then
            styler:SetState(S_LOCAL_PARAM)
          else
            styler:SetState(S_LOCAL)
          end
        else -- global
          if ast.definedglobal then
            styler:SetState(S_GLOBAL_RECOGNIZED)
          else
            styler:SetState(S_GLOBAL_UNRECOGNIZED)
          end
        end
      elseif ast.isfield then -- implies token.tag == 'String'
        if ast.definedglobal or ast.seevalue.valueknown and ast.seevalue.value ~= nil then
          styler:SetState(S_FIELD_RECOGNIZED)
        else
          styler:SetState(S_FIELD)
        end
      elseif token.tag == 'Comment' then
        styler:SetState(S_COMMENT)
      elseif token.tag == 'String' then -- note: excludes ast.isfield
        styler:SetState(S_STRING)
      elseif token.tag == 'Keyword' then
        styler:SetState(S_KEYWORD)
      else
        styler:SetState(S_DEFAULT)
      end
    elseif styler:Current() == '\t' then
      styler:SetState(S_TAB)
    else
      styler:SetState(S_DEFAULT)
    end
    styler:Forward()
    i = i + 1
  end
  styler:EndStyling()  

  -- Mark masked local variables.
  editor.IndicatorCurrent = 3
  editor.IndicStyle[3] = INDIC_STRIKE
  editor.IndicFore[3] = 0x0000ff
  editor:IndicatorClearRange(0, editor.Length)
  local tokenlist = buffer.tokenlist
  for idx=1,#tokenlist do
    local token = tokenlist[idx]
    local ast = token.ast
    if ast and ast.ismasking then
      editor:IndicatorFillRange(token.fpos-1, token.lpos - token.fpos + 1)
    end
  end
  
  -- Apply folding.
  --[[FIX:disabled due to odd problems discussed below
  local linea0 = editor:LineFromPosition(styler.startPos)
  local lineb0 = editor:LineFromPosition(styler.startPos+styler.lengthDoc)
  DEBUG('+', linea0,lineb0) -- test for recursion
  -- IMPROVE: This might be done only over styler.startPos, styler.lengthDoc.
  --   Does that improve performance?
  local level = 0
  local levels = {}; for line1=1,editor.LineCount do levels[line1] = level end
  LI.walk(buffer.ast, function(ast)
    if isblock[ast.tag] then
      local fline1, lline1 =  LI.ast_pos_range(ast, buffer.tokenlist)
      levels[fline1] = level + (lline1>fline1 and SC_FOLDLEVELHEADERFLAG or 0)
      level = level + 1      
      for line1=fline1+1, lline1 do
        levels[line1] = level
      end
    end
  end, function(ast)
    if isblock[ast.tag] then level = level - 1 end
  end)
  for line1=#levels,1,-1 do -- [*1]
    --  if line1-1 >= linea0 and line1-1 <= lineb0 then [*2]
    styler:SetLevelAt(line1-1, levels[line1])
  end
  -- caution: If StartStyling is performed over a range larger than suggested by startPos/lengthDoc,
  --   then we cannot rely on it for folding.
  -- QUESTION: this function is prone to recursion.  Changing a flag on a line more than once
  --   like this causes OnStyle sometimes causing stack overflow from recursion:
  --     styler:SetLevelAt(0,1)
  --     styler:SetLevelAt(0,1 + SC_FOLDLEVELHEADERFLAG)
  --   Setting levels only on lines being styled [*2] improves this to little or no recusion but worsens
  --     styling problems (which exist whenever folding is used here).
  --   Iterating in reverse [*1] reduces recursion to little or none.
  --   Disabling folding completely eliminates recursion.
  print'DEBUG:-'  -- test for recursion
  ]]
end


scite_OnDoubleClick(function()
  if buffer.text ~= editor:GetText() then return end -- skip if AST is not up-to-date
  
  -- check if selection if currently on identifier
  local token = getselectedvariable()
  if token then
    local info  = formatvariabledetails(token)
    editor:CallTipShow(token.fpos-1, info)
  end
end)


-- Command for replacing all occurances of selected variable (if any) with given text `newname`
-- Usage in SciTE properties file:
function M.rename_selected_variable(newname)
  local selectedtoken = getselectedvariable()
  
  if selectedtoken and selectedtoken.ast then
    local id = selectedtoken.ast.id
    editor:BeginUndoAction()
    local lasttoken
    for i=#buffer.tokenlist,1,-1 do
      local token = buffer.tokenlist[i]
      local ast = token.ast
      if ast and ast.id == id then
        editor:SetSel(token.fpos-1, token.lpos)
        editor:ReplaceSel(newname)
        lasttoken = token
      end
    end
    if lasttoken then
      editor:SetSel(lasttoken.fpos-1, lasttoken.fpos + newname:len())
      editor.Anchor = lasttoken.fpos-1
    end
    editor:EndUndoAction()
  end
end
-- IMPROVE: prevent rename to conflicting existing variable.


-- Gets 1-indexed character position of definition associated with AST node (if any).
local function ast_to_definition_position(ast, tokenlist)
  local local_ast = ast.localdefinition
  if local_ast then
    local tidx = LI.ast_idx_range_in_tokenlist(tokenlist, local_ast)
    if tidx then return tokenlist[tidx].fpos end
  end
end


-- Command for going to definition of selected variable.
-- TODO: currently only works for locals in the same file.
function M.goto_definition()
  local selectedtoken = getselectedvariable()
  local pos1 = selectedtoken.ast and ast_to_definition_position(selectedtoken.ast, buffer.tokenlist) --FIX:may be nil
  if pos1 then
    if set_mark then set_mark() end -- if ctagsdx.lua available
    editor:GotoPos(pos1 - 1)
  end  
end


-- Command for inspecting fields of selected table variable.
function M.inspect_variable_contents()
  local token = getselectedvariable()
  if not token or not token.ast then return end
  local ast = token.ast 

  editor.AutoCSeparator = 1
  if type(ast.value) == 'table' then
    local t = ast.value
    local keys = {}; for k,v in pairs(t) do keys[#keys+1] = k end
    table.sort(keys)
    local info = ''
    for _,k in ipairs(keys) do
      local ks = tostring(k);    if ks:len() > 50 then ks = ks:sub(1,50)..'...' end
      local vs = tostring(t[k]); if vs:len() > 50 then vs = vs:sub(1,50)..'...' end
      info = info .. ks .. "=" .. vs .. "\1"
    end
    editor:AutoCShow(0, info)
  elseif type(ast.value) == 'userdata' then
    editor:AutoCShow(0, "userdata not inspectable") -- unfortunately without __pairs.
  else
    editor:AutoCShow(0, tostring(ast.value) .. " not inspectable")
  end
end

-- Command to show all uses of selected variable
function M.show_all_variable_uses()
  local stoken = getselectedvariable()
  if not stoken or not stoken.ast then return end
  
  editor.AutoCSeparator = 1
  local infos = {}
  for _,token in ipairs(buffer.tokenlist) do
    if token.ast and token.ast.id == stoken.ast.id then
      local linenum0 = editor:LineFromPosition(token.fpos-1)
      infos[#infos+1] = (linenum0+1) .. ": " .. editor:GetLine(linenum0):gsub("[\r\n]+$", "")
    end
  end
  --editor:UserListShow(1, table.concat(infos, "\1"))  
  scite_UserListShow(infos, 1, function(text)
    local line1 = tonumber(text:match("^%d+"))
    if set_mark then set_mark() end -- if ctagsdx.lua available
    editor:GotoLine(line1-1)
  end)
end
--IMPROVE: jump to exact position, not just line number.


-- Command to select smallest statement (or comment) containing selection.
-- Executing multiple times selects larger statements containing current statement.
function M.select_statementblockcomment()
  if buffer.text ~= editor:GetText() then return end  -- skip if AST not up-to-date  

  -- Get selected position range.
  -- caution: SciTE appears to have an odd behavior where if SetSel
  --   is performed with CurrentPos at the start of a new line,
  --   then Anchor and CurrentPos get reversed.  Similar behavior is observed
  --   when holding down the shift key and pressing the right arrow key
  --   until the cursor advances to the next line.
  --   In any case, we want to handle reversed ranges.
  local fpos, lpos = editor.Anchor, editor.CurrentPos
  if lpos < fpos then fpos, lpos = lpos, fpos end -- swap
  fpos, lpos = fpos + 1, lpos + 1 - 1
  local fpos, lpos = LI.select_statementblockcomment(buffer.ast, buffer.tokenlist, fpos, lpos, true)
  editor:SetSel(fpos-1, lpos-1 + 1)
end


function M.install()
  scite_Command("Rename all instances of selected variable|*luainspect_rename_selected_variable $(1)|*.lua|Ctrl+Alt+R")
  scite_Command("Go to definition of selected variable|luainspect_goto_definition|*.lua|Ctrl+Alt+D")
  scite_Command("Show all variable uses|luainspect_show_all_variable_uses|*.lua|Ctrl+Alt+U")
  scite_Command("Inspect table contents|luainspect_inspect_variable_contents|*.lua|Ctrl+Alt+I")
  scite_Command("Select current statement, block or comment|luainspect_select_statementblockcomment|*.lua|Ctrl+Alt+S")
  --FIX: user.context.menu=Rename all instances of selected variable|1102 or props['user.contextmenu']
  _G.OnStyle = OnStyle
  _G.luainspect_rename_selected_variable = M.rename_selected_variable
  _G.luainspect_goto_definition = M.goto_definition
  _G.luainspect_inspect_variable_contents = M.inspect_variable_contents
  _G.luainspect_show_all_variable_uses = M.show_all_variable_uses
  _G.luainspect_select_statementblockcomment = M.select_statementblockcomment

  -- apply styles if not overridden in properties file.
  local light_styles = [[
# This can be customized in your properties file.
lexer.*.lua=script_lua
style.script_lua.default=fore:#000000
style.script_lua.local=fore:#000080
style.script_lua.local_mutate=fore:#000080,italics
style.script_lua.local_unused=fore:#ffffff,back:#000080
style.script_lua.local_param=fore:#000040
style.script_lua.upvalue=fore:#0000ff
style.script_lua.upvalue_mutate=fore:#0000ff,italics
style.script_lua.global_recognized=fore:#600000
style.script_lua.global_unrecognized=fore:#ffffff,back:#ff0000,bold
style.script_lua.field_recognized=fore:#600000
style.script_lua.field=fore:#c00000
style.script_lua.comment=fore:#008000
style.script_lua.string=fore:#00c000
style.script_lua.tab=back:#f0f0f0
style.script_lua.keyword=fore:#505050,bold
style.script_lua.compiler_error=fore:#800000,back:#ffffc0

# From SciTE docs:
# As well as the styles generated by the lexer, there are other numbered styles used.
# Style 32 is the default style and its features will be inherited by all other styles unless overridden.
# Style 33 is used to display line numbers in the margin.
# Styles 34 and 35 are used to display matching and non-matching braces respectively.
# Style 36 is used for displaying control characters. This is not a full style as the foreground and background colours for control characters are determined by their lexical state rather than this style.
# Style 37 is used for displaying indentation guides. Only the fore and back are used.
# A * can be used instead of a lexer to indicate a global style setting. 
#style.script_lua.32=back:#000000
#style.script_lua.33=
#style.script_lua.33=
#style.script_lua.34=
#style.script_lua.36=
#style.script_lua.37=

# warning: these changes are global for all file types:
caret.line.back=#ffff00
caret.line.back.alpha=20
]]

  -- or dark background style
  local dark_styles = [[
lexer.*.lua=script_lua
style.script_lua.32=back:#000000
style.script_lua.default=fore:#ffffff
style.script_lua.local=fore:#c0c0ff
style.script_lua.local_mutate=fore:#c0c0ff,italics
style.script_lua.local_unused=fore:#ffffff,back:#000080
style.script_lua.local_param=fore:#8080ff
style.script_lua.upvalue=fore:#e8e8ff
style.script_lua.upvalue_mutate=fore:#e8e8ff,italics
style.script_lua.global_recognized=fore:#ffc080
style.script_lua.global_unrecognized=fore:#ffffff,back:#ff0000,bold
style.script_lua.field_recognized=fore:#ffc080
style.script_lua.field=fore:#ff0000
style.script_lua.comment=fore:#00c000
style.script_lua.string=fore:#00c000
style.script_lua.tab=back:#303030
style.script_lua.keyword=fore:#a0a080,bold
style.script_lua.compiler_error=fore:#800000,back:#ffffc0
style.script_lua.indic_style=6
style.script_lua.indic_fore=#808080
# warning: these changes are global for all file types.  Avoid #ffffff in case those
# are light styles
style.script_lua.caret.fore=#c0c0c0
style.script_lua.caret.line.back=#ffff00
style.script_lua.caret.line.back.alpha=20
style.script_lua.selection.alpha=128
style.script_lua.selection.back=#808080
]]

  local styles = (props['style.script_lua.scheme'] == 'dark') and dark_styles or light_styles

  for style in styles:gmatch("[^\n]+") do
    if not (style:match("^%s*#") or style:match("^%s*$")) then
        local name, value = style:match("^([^=]+)=(.*)"); assert(name, style)
        local realname =string.gsub(name, '^(style%.script_lua%.)(.+)$', function(first, last)
          return STYLES[last] and first .. STYLES[last] or
                    last:match'^%d+$' and name or last
        end) -- convert to real style name
        if props[name] ~= '' then value = props[name] end -- override by user
        --DEBUG(realname .. '=' .. value)
        props[realname] = value
    end
  end
  -- DESIGN:SciTE: The above technique does not work ideally.  A property like 'selection.back'
  -- may be pre-defined by SciTE, and then we'd want this script to override that default, and
  -- finally we'd want to allow the user to override that in property files.  However, this script
  -- is run after property files are applied and doesn't know whether a property
  -- has been re-defined in a property file unless the property was left blank by SciTE and the
  -- user property file changed it to a non-blank value.  This is the reason why the above
  -- dark_styles uses style.script_lua.selection.back (which is undefined by SciTE) rather
  -- than selection.back (which SciTE may predefine to a non-blank value).
  
end

--COMMENT:SciTE: when Lua code fails, why doesn't SciTE display a full stack traceback
-- (debug.traceback) to assist debugging?

return M
