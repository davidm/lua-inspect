-- luainspect.scite - SciTE text editor plugin
--
-- (c) 2010 David Manura, MIT License.

-- Whether to update the AST on every edit (true) or online when the selection
-- is moved to a different line (false).  false can be more efficient for large files.
local UPDATE_ALWAYS = true

-- Experimental feature: display types/values of all known locals as annotations.
-- Allows Lua to be used like a Mathcad worksheet.
local ANNOTATE_ALL_LOCALS = false

local LI = require "luainspect.init"
local LS = require "luainspect.signatures"

local M = {}

-- variables stored in `buffer`:
-- ast -- last successfully compiled AST
-- text  -- text corresponding to `ast`
-- lasttext  -- last attempted `text` (might not be successfully compiled)
-- notes  -- notes corresponding to `ast`
-- lastline - number of last line in scite_OnUpdateUI (only if not UPDATE_ALWAYS)

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

  -- loadstring and metalua don't parse shebang
  local shebang = newtext:match("^#![^\r\n]*")
  local newtextm = shebang and (" "):rep(#shebang) .. newtext:sub(#shebang+1) or newtext

  -- Analyze code using LuaInspect, and apply decorations
  -- loadstring is much faster than Metalua, so try that first.
  -- Furthermore, Metalua accepts a superset of the Lua grammar.
  local linenum, colnum, err, linenum2
  local ok, err_ = loadstring(newtextm, "")
  if not ok then
    err = err_
    err = err:gsub('^%[string ""%]:', "")
    linenum = assert(err:match("(%d+):"))
    colnum = 0
    linenum2 = err:match(":%d+: '[^']+' expected %(to close '[^']+' at line (%d+)")
  else
    local ok_, ast_ = pcall(LI.ast_from_string, newtextm, "noname.lua"); ok = ok_
    if not ok then
      print "warning: metalua failed to compile code that compiles with loadstring.  error in metalua?"
      err = ast_
      err = err:match('[^\n]*')
      err = err:gsub("^.-:%s*line", "line")
          -- mlp.chunk prepending this is undesirable.   error(msg,0) would be better in gg.lua. Reported.
	  -- TODO-Metalua: remove when fixed in Metalua.
      linenum, colnum = err:match("line (%d+), char (%d+)")
      if not linenum then
        -- Metalua libraries may return "...gg.lua:56: .../mlp_misc.lua:179: End-of-file expected"
        -- without the normal line/char numbers given things like "if x then end end".  Should be
	-- fixed probably with gg.parse_error in _chunk in mlp_misc.lua.
	-- TODO-Metalua: remove when fixed in Metalua.
        linenum = editor.LineCount - 1
        colnum = 0
      end
    else
      buffer.ast = ast_
    end
  end
  --unused: editor.IndicStyle[0]=
  if not ok then
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
    buffer.notes = LI.inspect(buffer.ast)
    buffer.text = newtext
    --old: editor:CallTipCancel()
    editor.IndicatorCurrent = 0
    editor:IndicatorClearRange(0, editor.Length)
    editor:MarkerDeleteAll(0)
    editor:AnnotationClearAll()

    if ANNOTATE_ALL_LOCALS then annotate_all_locals() end
  end
end

-- Helper function used by rename_selected_variable
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

-- Command for going to definition of selected variable.
-- TODO: currently only works for locals in the same file.
function M.goto_definition()
  local selectednote = getselectedvariable()
  if selectednote then
    if selectednote.ast.localdefinition then
      local local_ast = selectednote.ast.localdefinition
      if local_ast.lineinfo then
        if set_mark then set_mark() end -- if ctagsdx.lua available
        editor:GotoPos(local_ast.lineinfo.first[3] - 1)
      end
    end
  end  
end

-- Command for inspecting fields of selected table variable.
function M.inspect_variable_contents()
  local note = getselectedvariable()
  if not note then return end
  local ast = note.ast 

  if type(ast.value) == 'table' then
    local t = ast.value
    local keys = {}; for k,v in pairs(t) do keys[#keys+1] = k end
    table.sort(keys)
    local info = ''
    editor.AutoCSeparator = 1
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

-- Respond to UI updates.  This includes moving the cursor.
scite_OnUpdateUI(function()
  -- 2DO:FIX: how to make the occur only in Lua buffers.
  if editor.Lexer ~= 0 then return end -- 2DO: hack: probably won't work with multiple Lua-based lexers

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
      --2DO: not rendering exactly as desired
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
local function OnStyle(styler)
  if styler.language ~= "script_lua" then return end -- avoid conflict with other stylers

  --if n == 0 then n = 2 else n = n - 1; return end -- this may improves performance on larger files only marginally
  --2DO: could metalua libraries parse text across multiple calls to `OnStyle` to reduce long pauses with big files?

  --print("DEBUG:","style",styler.language, styler.startPos, styler.lengthDoc, styler.initStyle)

  -- update AST if needed
  if UPDATE_ALWAYS then
    update_ast()
  elseif not buffer.lasttext then
    -- this ensures that AST compiling is attempted when file is first loaded since OnUpdateUI
    -- is not called on load.
    update_ast()
  end

  if buffer.text ~= editor:GetText() then return end  -- skip if AST not up-to-date

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
  --2DO: use SCN_HOTSPOTCLICK somehow?
  styler:StartStyling(0, editor.Length, 0)
  local i=1
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
      -- 2DO: how to lightlight keywords? how to obtain this from the AST?
      else
        styler:SetState(S_DEFAULT)
      end
    else
      styler:SetState(S_DEFAULT)
    end
    styler:Forward()
    i = i + 1
  end
  styler:EndStyling()
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

function M.install()
  scite_Command("Rename all instances of selected variable|*luainspect_rename_selected_variable $(1)|*.lua|Ctrl+Alt+R")
  scite_Command("Go to definition of selected variable|luainspect_goto_definition|*.lua|Ctrl+Alt+D")
  scite_Command("Show all variable uses|luainspect_show_all_variable_uses|*.lua|Ctrl+Alt+U")
  scite_Command("Inspect table contents|luainspect_inspect_variable_contents|*.lua|Ctrl+Alt+I")
  --FIX: user.context.menu=Rename all instances of selected variable|1102 or props['user.contextmenu']
  _G.OnStyle = OnStyle
  _G.luainspect_rename_selected_variable = M.rename_selected_variable
  _G.luainspect_goto_definition = M.goto_definition
  _G.luainspect_inspect_variable_contents = M.inspect_variable_contents
  _G.luainspect_show_all_variable_uses = M.show_all_variable_uses

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
]]
  if not props["lexer.*.lua"] then
    for style in styles:gmatch("[^\n]+") do
      if not (style:match("^%s*#") or style:match("^%s*$")) then
        local name, value = style:match("^([^=]+)=(.*)"); assert(name, style)
        print(props[name])
      end
    end
  end
end

return M

