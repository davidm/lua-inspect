--switch_buffers.lua
--drops down a list of buffers, in recently-used order

scite_Command 'Switch Buffer|do_buffer_list|Alt+F12'
scite_Command 'Last Buffer|last_buffer|Ctrl+F12'

local buffers = {}

scite_OnOpenSwitch(function(f)
--- swop the new current buffer with the last one!
  local idx  
  for i,file in ipairs(buffers) do
     if file == f then  idx = i; break end
  end
  if idx then 
    table.remove(buffers,idx)
    table.insert(buffers,1,f)
  else
    table.insert(buffers,1,f)
  end
end)

function last_buffer()
   if table.getn(buffers) > 1 then
      scite.Open(buffers[2])
   end
end

function do_buffer_list()
     scite_UserListShow(buffers,2,scite.Open)
end
