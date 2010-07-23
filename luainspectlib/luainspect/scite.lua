-- luainspect.scite - SciTE text editor plugin
--
-- (c) 2010 David Manura, MIT License.

-- Whether to update the AST on every edit (true) or online when the selection
-- is moved to a different line (false).  false can be more efficient for large files.
local UPDATE_ALWAYS = true

local LI = require "luainspect.init"
local LS = require "luainspect.signatures"

local M = {}

local ast -- last successfully compiled AST
local text  -- text corresponding to `ast`
local lasttext  -- last attempted `text` (might not be successfully compiled)
local notes  -- notes corresponding to `ast`

-- Style IDs
-- 2DO:improve: define default styles here in Lua in case these are not defined in the properties file.
--    See "SciTE properties" in http://lua-users.org/wiki/UsingLuaWithScite .
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


-- Attempt to update AST from editor text and apply decorations.
local function update_ast()
  -- skip if text unchanged
  local newtext = editor:GetText()
  if newtext == lasttext then return false end
  lasttext = newtext

  -- Analyze code using LuaInspect, and apply decorations
  local ok, ast_ = pcall(LI.ast_from_string, newtext, "fake.lua") -- 2DO: filename
  --unused: editor.IndicStyle[0]=
  if not ok then
     local err = ast_
     err = err:match('[^\n]*')
     err = err:gsub("^.-:%s*line", "line") -- 2DO: improve Metalua libraries to avoid LI.ast_from_string prepending this?
     local linenum, colnum = err:match("line (%d+), char (%d+)")
     if not linenum then
       --2DO: improve Metalua libraries since it may return "...gg.lua:56: .../mlp_misc.lua:179: End-of-file expected"
       --without the normal line/char numbers given things like "if x then end end"
       linenum = editor.LineCount - 1
       colnum = 0
     end
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
     return
  else
     ast = ast_
     notes = LI.inspect(ast)
     text = newtext
     --old: editor:CallTipCancel()
     editor.IndicatorCurrent = 0
     editor:IndicatorClearRange(0, editor.Length)
     editor:MarkerDeleteAll(0)
     editor:AnnotationClearAll()
  end
  return ast_ ~= nil
end


-- Respond to UI updates.  This includes moving the cursor.
local lastline
scite_OnUpdateUI(function(x)
  -- 2DO:FIX: how to make the occur only in Lua buffers.
  if editor.Lexer ~= 0 then return end -- 2DO: hack: probably won't work with multiple Lua-based lexers

  -- This updates the AST when the selection is moved to a different line.
  if not UPDATE_ALWAYS then
    local currentline = editor:LineFromPosition(editor.CurrentPos)
    if currentline ~= lastline then
      update_ast()
      lastline = currentline
    end
  end

  if text ~= editor:GetText() then return end -- skip if AST is not up-to-date
  
  -- check if selection if currently on identifier
  local id, selectednote
  if text == lasttext then -- valid compile
    local pos = editor.CurrentPos+1
    for i,note in ipairs(notes) do
      if pos >= note[1] and pos <= note[2] then
        if note.ast.id then
          selectednote = note
  	  id = note.ast.id
        end
        break
      end
    end
  end

  -- hightlight all instances of that identifier
  if id then
    editor.IndicStyle[1] = INDIC_ROUNDBOX
    editor.IndicatorCurrent = 1
    editor:IndicatorClearRange(0, editor.Length)
    for _,note in ipairs(notes) do
      if note.ast.id == id then
        editor:IndicatorFillRange(note[1]-1, note[2]-note[1]+1)
      end
    end
  else
    editor.IndicatorCurrent = 1
    editor:IndicatorClearRange(0, editor.Length)
  end

  -- Display callinfo help on function.
  if selectednote and selectednote.definedglobal and LS.global_signatures[selectednote.ast[1]] then
    local name = selectednote.ast[1]
    editor:CallTipShow(editor.CurrentPos, LS.global_signatures[name])
  else
    editor:CallTipCancel()
  end
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
  elseif not lasttext then
    -- this ensures that AST compiling is attempted when file is first loaded since OnUpdateUI
    -- is not called on load.
    update_ast()
  end

  if text ~= editor:GetText() then return end  -- skip if AST not up-to-date

  -- Apply SciTE styling
  editor.StyleHotSpot[S_LOCAL] = true
  editor.StyleHotSpot[S_LOCAL_MUTATE] = true
  --2DO: use SCN_HOTSPOTCLICK somehow?
  styler:StartStyling(0, editor.Length, 0)
  local i=1
  local inote = 1
  local note = notes[inote]
  local function nextnote() inote = inote+1; note = notes[inote] end
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
        elseif note.ast.localdefinition.isparam then
          styler:SetState(S_LOCAL_PARAM)
        else
          styler:SetState(S_LOCAL)
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

function M.install()
   _G.OnStyle = OnStyle
end

return M

