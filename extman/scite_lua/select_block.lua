-- this experimental script makes it easy to select blocks with a single click.
-- The usual behaviour is to select the whole line, and if that line happens to be a fold line
-- then select the rest of that block.

scite_require 'bit.luax'

function line_selected()
--	if not scite_GetProp('fold') then return end
	local s1 = editor.SelectionStart
	local s2 = editor.SelectionEnd
	if s2 > s1 then -- non-trivial selection
		local line = editor:LineFromPosition(s1)
		if editor:PositionFromLine(line) > s1 then
			return -- because selection didn't start at begining of line
		end 
		if s2 == editor:PositionFromLine(line+1) then -- whole line selected!
			local lev = editor.FoldLevel[line]
			if bit.band(lev,SC_FOLDLEVELHEADERFLAG) then -- a fold line
				local lastl = editor:GetLastChild(line,-1)
				s2 = editor:PositionFromLine(lastl+1)
				-- hack: a fold line beginning with a '{' is not where we want to start...
				if string.find(editor:GetLine(line),'^%s*{') then
					s1 = editor:PositionFromLine(line-1)
				end
				editor.Anchor = s2
				editor.CurrentPos = s1
			end
		end
	end
end

scite_OnUpdateUI(line_selected)

