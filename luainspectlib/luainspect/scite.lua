-- luainspect.scite - SciTE text editor plugin
--
-- (c) 2010 David Manura, MIT License.

-- Whether to update the AST on every edit (true) or online when the selection
-- is moved to a different line (false).  false can be more efficient for large files.
local UPDATE_ALWAYS = true

-- Experimental feature: display types/values of all known locals as annotations.
-- Allows Lua to be used like a Mathcad worksheet.
local ANNOTATE_ALL_LOCALS = false

-- WARNING: Experimental feature--in development.
-- When user edits code, recompile only the portion of code that is edited.
-- This should improve performance.
local ALLOW_INCREMENTAL_COMPILATION = false

-- Whether to run timing tests (for internal development purposes).
local PERFORMANCE_TESTS = false

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
    if note.ast.localdefinition.functionlevel  < note.ast.functionlevel then
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
  -- skip if text unchanged
  local newtext = editor:GetText()
  if newtext == buffer.lasttext then return false end
  buffer.lasttext = newtext
  
  clockbegin 't1'

  -- loadstring and metalua don't parse shebang
  local newtextm = LI.remove_shebang(newtext)

  local isincremental = ALLOW_INCREMENTAL_COMPILATION and buffer.ast
  
  -- Analyze code using LuaInspect, and apply decorations
  -- loadstring is much faster than Metalua, so try that first.
  -- Furthermore, Metalua accepts a superset of the Lua grammar.
  local f, err, linenum, colnum, linenum2 = LI.loadstring(newtextm)
  if f then
    local compiletext
    local pos1f, pos1l, pos2f, pos2l, old_ast
    
    if isincremental then
      pos1f, pos1l, pos2f, pos2l, old_ast = LI.invalidated_code(buffer.ast, LI.remove_shebang(buffer.text), newtextm)
      compiletext = newtextm:sub(pos2f,pos2l)
      --print('DEBUG:incremental:[' .. compiletext .. ']')
    else
      compiletext = newtextm
    end

    clock 't2'
    local ast; ast, err, linenum, colnum, linenum2 = LI.ast_from_string(compiletext, "noname.lua")
    clock 't3'

    if not ast then
      print "warning: metalua failed to compile code that compiles with loadstring.  error in metalua?"
    else
      if isincremental then
        -- insert new AST into original AST and adjust line numbers
        local delta = pos2l - pos1l
        LI.adjust_lineinfo(buffer.ast, pos1l+1, delta)
        LI.adjust_lineinfo(ast, 1, pos2f-1)        
        LI.switchtable(old_ast, ast)

        buffer.notes = nil; collectgarbage()
        buffer.notes = LI.inspect(buffer.ast) --IMPROVE: don't do full inspection
        --adjust_notes(buffer.notes, pos1l+1, delta)
        --buffer.notes = LI.create_notes(buffer.ast)
      else
        buffer.ast = ast
        
        -- careful: if `buffer.notes` variable exists in `newtext`, then
        --   `LI.inspect` may attach its previous value into the newly created
        --   `buffer.notes`, eventually leading to memory overflow.
        buffer.notes = nil; collectgarbage()
        buffer.notes = LI.inspect(buffer.ast)
      end
    end
    clockend 't4'
  end
  --unused: editor.IndicStyle[0]=
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
    buffer.text = newtext
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

  -- hightlight all instances of that identifier
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

    -- mark entire scope
    local firstline = editor:LineFromPosition(first[1]-1)
    local lastline = editor:LineFromPosition(last[2]-1)
    if firstline ~= lastline then
      --TODO: not rendering exactly as desired.  TCORNERCURVE should
      -- preferrably be an upside-down LCORNERCURVE; plus the color on TCORNERCURVE is off.
      editor:MarkerDefine(1, SC_MARK_TCORNERCURVE)
      editor:MarkerDefine(2, SC_MARK_VLINE)
      editor:MarkerDefine(3, SC_MARK_LCORNERCURVE)
      editor:MarkerSetFore(1, 0x0000ff)
      editor:MarkerSetFore(2, 0x0000ff)
      editor:MarkerSetFore(3, 0x0000ff)

      editor:MarkerAdd(firstline, 1)
      for n=firstline+1,lastline-1 do
        editor:MarkerAdd(n, 2)
      end
      editor:MarkerAdd(lastline, 3)
    else
      editor:MarkerDefine(2, SC_MARK_VLINE)
      editor:MarkerSetFore(2, 0x0000ff)
      editor:MarkerAdd(firstline, 2)
    end

  else
    editor.IndicatorCurrent = 1
    editor:IndicatorClearRange(0, editor.Length)
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
  local pos1 = ast_to_definition_position(selectednote.ast)
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
function M.select_statement()
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
  fpos = fpos + 1
  lpos = lpos + 1 - 1
  local fpos, lpos = LI.select_statement(buffer.ast, fpos, lpos, true)
  editor:SetSel(fpos-1, lpos-1 + 1)
end


function M.install()
  scite_Command("Rename all instances of selected variable|*luainspect_rename_selected_variable $(1)|*.lua|Ctrl+Alt+R")
  scite_Command("Go to definition of selected variable|luainspect_goto_definition|*.lua|Ctrl+Alt+D")
  scite_Command("Show all variable uses|luainspect_show_all_variable_uses|*.lua|Ctrl+Alt+U")
  scite_Command("Inspect table contents|luainspect_inspect_variable_contents|*.lua|Ctrl+Alt+I")
  scite_Command("Select current statement or comment|luainspect_select_statement|*.lua|Ctrl+Alt+S")
  --FIX: user.context.menu=Rename all instances of selected variable|1102 or props['user.contextmenu']
  _G.OnStyle = OnStyle
  _G.luainspect_rename_selected_variable = M.rename_selected_variable
  _G.luainspect_goto_definition = M.goto_definition
  _G.luainspect_inspect_variable_contents = M.inspect_variable_contents
  _G.luainspect_show_all_variable_uses = M.show_all_variable_uses
  _G.luainspect_select_statement = M.select_statement

  -- apply styles if not overridden in properties file.
  local styles = [[  
lexer.*.lua=script_lua
# default
style.script_lua.0=fore:#000000
# local
style.script_lua.1=fore:#000080
# recognized global
style.script_lua.2=fore:#600000
# unrecognized global
style.script_lua.3=fore:#ffffff,back:#ff0000,bold
# comment
style.script_lua.4=fore:#008000
# string
style.script_lua.5=fore:#00c000
# local mutate
style.script_lua.6=fore:#000080,italics
# local unused
style.script_lua.7=fore:#ffffff,back:#0000ff
# local param
style.script_lua.8=fore:#000040
# compiler error
style.script_lua.9=fore:#800000,back:#ffffc0
# local upvalue
style.script_lua.10=fore:#0000ff
# table field
style.script_lua.11=fore:#c00000
# table field recognized
style.script_lua.12=fore:#600000
# tab character in whitespace
style.script_lua.13=back:#f0f0f0
]]
  if props["lexer.*.lua"] == "" then
    for style in styles:gmatch("[^\n]+") do
      if not (style:match("^%s*#") or style:match("^%s*$")) then
        local name, value = style:match("^([^=]+)=(.*)"); assert(name, style)
        props[name] = value
      end
    end
  end
end


return M
