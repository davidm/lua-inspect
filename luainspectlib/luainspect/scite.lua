-- luainspect.scite - SciTE text editor plugin
--
-- (c) 2010 David Manura, MIT License.

-- Whether to update the AST on every edit (true) or only when the selection
-- is moved to a different line (false).  false can be more efficient for large files.
local UPDATE_ALWAYS = scite_GetProp('luainspect.update.always', '1') == '1'

-- Experimental feature: display types/values of all known locals as annotations.
-- Allows Lua to be used like a Mathcad worksheet.
local ANNOTATE_ALL_LOCALS = scite_GetProp('luainspect.annotate.all.locals', '0') == '1'

-- WARNING: Experimental feature--Probably still has bugs.
-- When user edits code, recompile only the portion of code that is edited.
-- This should improve performance.
local INCREMENTAL_COMPILATION = scite_GetProp('luainspect.incremental.compilation', '0') == '1'

-- Whether to run timing tests (for internal development purposes).
local PERFORMANCE_TESTS = scite_GetProp('luainspect.performance.tests', '0') == '1'

local LI = require "luainspect.init"
local LS = require "luainspect.signatures"

local M = {}

-- variables stored in `buffer`:
-- ast -- last successfully compiled AST
-- text  -- text corresponding to `ast`
-- lasttext  -- last attempted `text` (might not be successfully compiled)
-- notes  -- notes corresponding to `ast`
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


-- Style IDs - correspond to style properties
local STYLES = {}
local S_DEFAULT = 0
local S_LOCAL = 1
local S_RECOGNIZED_GLOBAL = 2
local S_UNRECOGNIZED_GLOBAL = 3
local S_COMMENT = 4
local S_STRING = 5
local S_LOCAL_MUTATE = 6
local S_LOCAL_UNUSED = 7
local S_LOCAL_PARAM = 8
local S_COMPILER_ERROR = 9
local S_LOCAL_UPVALUE = 10
local S_TABLE_FIELD = 11
local S_TABLE_FIELD_RECOGNIZED = 12
local S_TAB = 13
local S_KEYWORD = 14
STYLES.default = S_DEFAULT
STYLES['local'] = S_LOCAL
STYLES.recognized_global = S_RECOGNIZED_GLOBAL
STYLES.unrecognized_global = S_UNRECOGNIZED_GLOBAL
STYLES.comment = S_COMMENT
STYLES.string = S_STRING
STYLES.local_mutate = S_LOCAL_MUTATE
STYLES.local_unused = S_LOCAL_UNUSED
STYLES.local_param = S_LOCAL_PARAM
STYLES.compiler_error = S_COMPILER_ERROR
STYLES.local_upvalue = S_LOCAL_UPVALUE
STYLES.table_field = S_TABLE_FIELD
STYLES.table_field_recognized = S_TABLE_FIELD_RECOGNIZED
STYLES.tab = S_TAB
STYLES.keyword = S_KEYWORD

local function formatvariabledetails(note)
  local info = ""
  if note.type == "global" then
    info = info .. (note.definedglobal and "recognized" or "unrecognized") .. " global "
  elseif note.type == "local" then
    if not note.ast.localdefinition.isused then
      info = info .. "unused "
    end
    if note.ast.localdefinition.isset then
      info = info .. "mutable "
    end
    if note.ast.localdefinition.functionlevel < note.ast.functionlevel then
      info = info .. "upvalue "
    elseif note.ast.localdefinition.isparam then
      info = info .. "param "
    end
    info = info .. "local "
  elseif note.type == "field" then
    info = info .. "field "
    if note.definedglobal then info = info .. "recognized " else info = info .. "unrecognized " end
  else
    info = info .. "? "
  end

  if note and note.ast.resolvedname and LS.global_signatures[note.ast.resolvedname] then
    local name = note.ast.resolvedname
    info = LS.global_signatures[name] .. "\n" .. info
  end
 
  local vast = note.ast.seevalue or note.ast
  if vast.valueknown then
    info = info .. "\nvalue= " .. tostring(vast.value) .. " "
  end
  return info
end


-- Used for ANNOTATE_ALL_LOCALS feature.
local function annotate_all_locals()
  -- Build list of annotations.
  local annotations = {}
  for i=1,#buffer.notes do
    local note = buffer.notes[i]
    if note.ast.localdefinition == note.ast then
      local info = formatvariabledetails(note)
      local linenum = editor:LineFromPosition(note[2]-1)
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
          LI.invalidated_code(buffer.ast, LI.remove_shebang(buffer.text), newtextm)
      compiletext = old_type == 'full' and newtextm or newtextm:sub(pos2f,pos2l)
      print('DEBUG:inc-compile:[' .. compiletext .. ']', old_ast and old_ast.tag, old_type, pos1f and (pos2l - pos1l), pos1l, pos2f)
    else
      compiletext = newtextm
    end
    clock 't2'

    -- Generate AST.
    local ast
    if old_type ~= 'whitespace' then
      ast, err, linenum, colnum, linenum2 = LI.ast_from_string(compiletext, "noname.lua")
    end
    clock 't3'

    if err then
      print "warning: metalua failed to compile code that compiles with loadstring.  error in metalua?"
    else
      buffer.text = newtext
      if isincremental and old_type ~= 'full' then
        -- Adjust line numbers.
        local delta = pos2l - pos1l
        LI.adjust_lineinfo(buffer.ast, pos1l, delta)
        if ast then  -- note: nil if whitespace
          LI.adjust_lineinfo(ast, 1, pos2f-1)
        end
 
        -- Inject AST
        if old_type == 'whitespace' then
          -- nothing
        elseif old_type == 'comment' then
        --table.print(ast)
          local new_comment = ast.lineinfo.first.comments[1]
          --table.print(old_ast)
          LI.switchtable(old_ast, new_comment)
        else assert(old_type == 'statblock')
          -- Merge alllineinfo.
          --[[
          assert(ast.alllineinfo)  -- from adjust_lineinfo
          if old_ast ~= buffer.ast then -- not replacing full AST
            for k in pairs(ast.alllineinfo) do buffer.ast.alllineinfo[k] = true end
            ast.alllineinfo = nil
          end
          .....
          old_ast = nil -- remove reference for gc
          
          collectgarbage() -- remove weak refs in alllineinfo
          ]]
          buffer.ast.alllineinfo = nil --IMPROVE

          LI.replace_ast(buffer.ast, old_ast, ast)
          
          --LI.walk(buffer.ast, function(ast) ast.parent = nil end)--FIX:DEBUG!!!!
          --table.print(buffer.ast, 20, 'nohash')
        end

        -- update notes
        if old_type == 'comment' or old_type == 'whitespace' then
          for i,note in ipairs(buffer.notes) do
            if note[1] >= pos1l then note[1] = note[1] + delta end
            if note[2] >= pos1l then note[2] = note[2] + delta end
          end
        else
          buffer.notes = nil; collectgarbage()
          buffer.notes = LI.inspect(buffer.ast) --IMPROVE: don't do full inspection
        --adjust_notes(buffer.notes, pos1l+1, delta)
        --buffer.notes = LI.create_notes(buffer.ast)
        end
      else --full
        buffer.ast = ast
        
        -- careful: if `buffer.notes` variable exists in `newtext`, then
        --   `LI.inspect` may attach its previous value into the newly created
        --   `buffer.notes`, eventually leading to memory overflow.
        buffer.notes = nil; collectgarbage()
        buffer.notes = LI.inspect(buffer.ast)
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
  --unused: editor.IndicStyle[0]=
end


-- Gets note assocated with currently selected variable (if any).
local function getselectedvariable()
  if buffer.text ~= editor:GetText() then return end  -- skip if AST not up-to-date
  local selectednote
  local id
  local pos = editor.Anchor+1
  for i,note in ipairs(buffer.notes) do
    if pos >= note[1] and pos <= note[2] then
      if note.ast.id then
        selectednote = note
        id = note.ast.id
      end
      break
    end
  end
  return selectednote, id
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


-- Respond to UI updates.  This includes moving the cursor.
scite_OnUpdateUI(function()
  -- FIX: how to make the occur only in Lua buffers.
  if editor.Lexer ~= 0 then return end -- FIX: hack: probably won't work with multiple Lua-based lexers

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

  -- highlight all instances of that identifier
  editor:MarkerDeleteAll(1)
  editor:MarkerDeleteAll(2)
  editor:MarkerDeleteAll(3)
  if id then
    editor.IndicStyle[1] = INDIC_ROUNDBOX
    editor.IndicatorCurrent = 1
    editor:IndicatorClearRange(0, editor.Length)
    local first, last -- first and last occurances
    for _,note in ipairs(buffer.notes) do
      if note.ast.id == id then
        last = note
        if not first then first = note end
        editor:IndicatorFillRange(note[1]-1, note[2]-note[1]+1)
      end
    end

    scope_positions(first[1]-1, last[2]-1)

  else
    editor.IndicatorCurrent = 1
    editor:IndicatorClearRange(0, editor.Length)
  end
  
  if not id then
    -- Check for selection over statement or expression.
    local fpos, lpos = editor.Anchor, editor.CurrentPos
    if lpos < fpos then fpos, lpos = lpos, fpos end -- swap
    fpos, lpos = fpos + 1, lpos + 1 - 1
    local match1_ast, match1_comment, iswhitespace =
      LI.smallest_ast_in_range(buffer.ast, buffer.text, fpos, lpos)
    --print('m', match1_ast and match1_ast.tag, match1_comment, iswhitespace)

    -- Highlight any related keywords  
    local kposlist; kposlist, match1_ast = LI.related_keywords(match1_ast, buffer.ast, buffer.text)
    if kposlist then
      editor.IndicStyle[1] = INDIC_ROUNDBOX
      editor.IndicatorCurrent = 1
      editor:IndicatorClearRange(0, editor.Length)
      for i=1,#kposlist,2 do
        local fpos, lpos = kposlist[i], kposlist[i+1]
        --print(fpos,lpos,'m')
        editor:IndicatorFillRange(fpos-1, lpos-fpos+1)
      end
    end
    
    -- Mark range of lines covered by item on selection.
    scope_positions(match1_ast.lineinfo.first[3], match1_ast.lineinfo.last[3])
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

  --print("DEBUG:","style",styler.language, styler.startPos, styler.lengthDoc, styler.initStyle)

  -- update AST if needed
  if UPDATE_ALWAYS then
    update_ast()
  elseif not buffer.lasttext then
    -- this ensures that AST compiling is attempted when file is first loaded since OnUpdateUI
    -- is not called on load.
    update_ast()
  end

  --print('DEBUG:OnStyle', editor:LineFromPosition(styler.startPos), editor:LineFromPosition(styler.startPos+styler.lengthDoc), styler.initStyle)
  if buffer.text ~= editor:GetText() then return end  -- skip if AST not up-to-date
    -- note: SciTE will repeatedly call OnStyle until StartStyling is performed.
    -- However, StartStyling clears styles in the given range, but we prefer to leave
    -- the styles as is.
 
  -- Apply SciTE styling
  editor.StyleHotSpot[S_LOCAL] = true
  editor.StyleHotSpot[S_LOCAL_MUTATE] = true
  editor.StyleHotSpot[S_LOCAL_UNUSED] = true
  editor.StyleHotSpot[S_LOCAL_PARAM] = true
  editor.StyleHotSpot[S_LOCAL_UPVALUE] = true
  editor.StyleHotSpot[S_RECOGNIZED_GLOBAL] = true
  editor.StyleHotSpot[S_UNRECOGNIZED_GLOBAL] = true
  editor.StyleHotSpot[S_TABLE_FIELD] = true
  editor.StyleHotSpot[S_TABLE_FIELD_RECOGNIZED] = true
  -- note: SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK currently aren't
  -- implemented by SciTE, although it has been proposed.

  local startpos0 = 0
  styler:StartStyling(startpos0, editor.Length, 0)
  -- local startpos0 = styler.startPos
  --styler:StartStyling(styler.startPos, styler.lengthDoc, styler.initStyle)
  --   a partial range like this doesn't work right since variables outside of edited range
  --   may need styling adjusted (e.g. a local variable definition that becomes unused)

  local i=startpos0+1
  local inote = 1
  local note = buffer.notes[inote]
  local function nextnote() inote = inote+1; note = buffer.notes[inote] end
  while styler:More() do
    while note and i > note[2] do
      nextnote()
    end
    
    if note and i >= note[1] and i <= note[2] then
      if note.type == 'global' and note.definedglobal then
        styler:SetState(S_RECOGNIZED_GLOBAL)
      elseif note.type == 'global' then
        styler:SetState(S_UNRECOGNIZED_GLOBAL)
      elseif note.type == 'local' then
        if not note.ast.localdefinition.isused then
          styler:SetState(S_LOCAL_UNUSED)
        elseif note.ast.localdefinition.isset then
          styler:SetState(S_LOCAL_MUTATE)
        elseif note.ast.localdefinition.functionlevel  < note.ast.functionlevel then
          styler:SetState(S_LOCAL_UPVALUE)
        elseif note.ast.localdefinition.isparam then
          styler:SetState(S_LOCAL_PARAM)
        else
          styler:SetState(S_LOCAL)
        end
      elseif note.type == 'field' then
        if note.definedglobal or note.ast.seevalue.value ~= nil then
          styler:SetState(S_TABLE_FIELD_RECOGNIZED)
        else
          styler:SetState(S_TABLE_FIELD)
        end
      elseif note.type == 'comment' then
        styler:SetState(S_COMMENT)
      elseif note.type == 'string' then
        styler:SetState(S_STRING)
      -- TODO: how to highlight keywords? The Metalua AST currently doesn't make this easy,
      -- but there are possible plans in Metalua to change that.  Check back with Metalua dev.
      else
        styler:SetState(S_DEFAULT)
      end
    elseif styler:Current() == '\t' then
      styler:SetState(S_TAB)
    elseif styler:Current():match'%a'
    then
      styler:SetState(S_KEYWORD)
    else
      styler:SetState(S_DEFAULT)
    end
    styler:Forward()
    i = i + 1
  end
  styler:EndStyling()  

  -- Apply folding.
  --[[FIX:disabled due to odd problems discussed below
  local linea0 = editor:LineFromPosition(styler.startPos)
  local lineb0 = editor:LineFromPosition(styler.startPos+styler.lengthDoc)
  print('DEBUG:+', linea0,lineb0) -- test for recursion
  -- IMPROVE: This might be done only over styler.startPos, styler.lengthDoc.
  --   Does that improve performance?
  local level = 0
  local levels = {}; for line1=1,editor.LineCount do levels[line1] = level end
  LI.walk(buffer.ast, function(ast)
    if isblock[ast.tag] then
      local fline1, lline1 = ast.lineinfo.first[1], ast.lineinfo.last[1]
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
  local note = getselectedvariable()
  if note then
    local info  = formatvariabledetails(note)
    editor:CallTipShow(note[1]-1, info)
  end
end)


-- Command for replacing all occurances of selected variable (if any) with given text `newname`
-- Usage in SciTE properties file:
function M.rename_selected_variable(newname)
  local selectednote = getselectedvariable()
  if selectednote then
    local id = selectednote.ast.id
    editor:BeginUndoAction()
    local lastnote
    for i=#buffer.notes,1,-1 do
      local note = buffer.notes[i]
      if note.ast.id == id then
        editor:SetSel(note[1]-1, note[2])
        editor:ReplaceSel(newname)
        lastnote = note
      end
    end
    if lastnote then
      editor:SetSel(lastnote[1]-1, lastnote[1] + newname:len())
      editor.Anchor = lastnote[1]-1
    end
    editor:EndUndoAction()
  end
end


-- Gets 1-indexed character position of definition associated with AST node (if any).
local function ast_to_definition_position(ast)
  local local_ast = ast.localdefinition
  if local_ast and local_ast.lineinfo then
    return local_ast.lineinfo.first[3]
  end
end


-- Command for going to definition of selected variable.
-- TODO: currently only works for locals in the same file.
function M.goto_definition()
  local selectednote = getselectedvariable()
  local pos1 = ast_to_definition_position(selectednote.ast) --FIX:may be nil
  if pos1 then
    if set_mark then set_mark() end -- if ctagsdx.lua available
    editor:GotoPos(pos1 - 1)
  end  
end


-- Command for inspecting fields of selected table variable.
function M.inspect_variable_contents()
  local note = getselectedvariable()
  if not note then return end
  local ast = note.ast 

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
  local snote = getselectedvariable()
  if not snote then return end
  
  editor.AutoCSeparator = 1
  local infos = {}
  for _,note in ipairs(buffer.notes) do
    if note.ast.id == snote.ast.id then
      local linenum0 = editor:LineFromPosition(note[1]-1)
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
  local fpos, lpos = LI.select_statementblockcomment(buffer.ast, fpos, lpos, true)
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
  local styles = [[
# This can be customized in your properties file.
lexer.*.lua=script_lua
style.script_lua.default=fore:#000000
style.script_lua.local=fore:#000080
style.script_lua.recognized_global=fore:#600000
style.script_lua.unrecognized_global=fore:#ffffff,back:#ff0000,bold
style.script_lua.comment=fore:#008000
style.script_lua.string=fore:#00c000
style.script_lua.local_mutate=fore:#000080,italics
style.script_lua.local_unused=fore:#ffffff,back:#0000ff
style.script_lua.local_param=fore:#000040
style.script_lua.compiler_error=fore:#800000,back:#ffffc0
style.script_lua.local_upvalue=fore:#0000ff
style.script_lua.table_field=fore:#c00000
style.script_lua.table_field_recognized=fore:#600000
style.script_lua.tab=back:#f0f0f0
style.script_lua.keyword=fore:#505050,bold
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
]]

  -- or dark background style
  --[=[
  local styles = [[
lexer.*.lua=script_lua
style.script_lua.32=back:#000000
style.script_lua.default=fore:#ffffff
style.script_lua.local=fore:#8080ff
style.script_lua.recognized_global=fore:#600000
style.script_lua.unrecognized_global=fore:#ffffff,back:#ff0000,bold
style.script_lua.comment=fore:#008000
style.script_lua.string=fore:#00c000
style.script_lua.local_mutate=fore:#8080ff,italics
style.script_lua.local_unused=fore:#ffffff,back:#0000ff
style.script_lua.local_param=fore:#4040ff
style.script_lua.compiler_error=fore:#800000,back:#ffffc0
style.script_lua.local_upvalue=fore:#c0c0ff
style.script_lua.table_field=fore:#c00000
style.script_lua.table_field_recognized=fore:#600000
style.script_lua.tab=back:#f0f0f0
style.script_lua.keyword=fore:#505050,bold
]]
--]=]

  for style in styles:gmatch("[^\n]+") do
    if not (style:match("^%s*#") or style:match("^%s*$")) then
        local name, value = style:match("^([^=]+)=(.*)"); assert(name, style)
        local realname =string.gsub(name, '^(style%.script_lua%.)([%a_]+)$', function(first, last)
          return first .. assert(STYLES[last], last)
        end) -- convert to style number
        if props[name] ~= '' then print'o' value = props[name] end -- override by user
        props[realname] = value
    end
  end
end

--COMMENT:SciTE: when Lua code fails, why doesn't SciTE display a full stack traceback
-- (debug.traceback) to assist debugging?

return M
