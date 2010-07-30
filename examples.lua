-- some examples/tests

local mt = require "math"

-- Basic variable scope and usage tests
local unused_local = 1
local used_local = 2; print(used_local)
local reassigned_local = 1; reassigned_local = 2
local upval_local; function f() return upval_local end
local reassigned_upval_local; function f() reassigned_upval_local = 2 end --Q:OK?
function f(param_unused_local, param_used_local, param_reassigned_local, param_upval_local, param_reassigned_upval_local)
  print(param_used_local)
  param_reassigned_local = 2
  return function()
    print(param_upval_local)
    param_reassigned_upval_local = 2
  end
end  -- Q:OK?
print(undefined_global)
print(math) -- predefined global
print(defined_global); defined_global = 2; print(defined_global)

-- Scope tests for specific statements
do local local1; for local1=local1,2 do end end  -- used, unused, used local
do local local1; for local1 in local1 do end end  -- used, unused, used local
do local local1; local local1 = local1 end -- used, unused, used local
do local function local1() local1() end -- used, used local
do local local1; local local1 = function() local1() end end -- used, unused, used local
end
do
  -- test repeat-until
  local local1  -- unused local
  repeat
    local local1 -- unused local
    local local1 -- used local
  until local1 -- used local
end

-- test local var scope stays inside block
do
  repeat local v2 until false;
  while false do local v3 end
  for v4=1,1 do local v5 end
  for v6 in nil do local v6 end
  print(v2, v3, v4, v5, v6) -- undefined globals
end

local t = {}
local t2 = {z=123}

t.abc = 123
t.def = function() end
t.abc = {}
t.abc.def = {x=2}
t.abc.def =  t.abc.def.x  + 1 + t:def() + t2.z

t2.abc = 456
t2.def = 234

t3 = {}
t3.abc = 123
t3.def = 234
t3.abc = {}
t3.abc.def = {x=2}
t3.abc.def =  t3.abc.def.x  + 1
t3[123] = {abc=3}
print(t3[123].abc)

local asdf, asdfg
print(math.sqrt(2), _G.math.sqrt(2).asd, _G.b:f())
print(string.lower("A"), string.lowers("A"), string:lower():upper(), asdf, asd)
print(mt.sqrt(2), mt.sqr(2), math.pi)

local sum = math.pi + 2
local sum = math.pii + 2
local sum = math.sqrt(4) + 1
local sum2 = sum + 1 + math.cos(2*4)^2
local zz = "Z" .. "Z"
local str1 = ("aBc"):upper():gsub("A", zz)


--test recursive v.s. non-recursive local scoping
do
 local x1 = x1
 local x1 = x1
 local function func() local x; func(x) end
 local func2 = function() local x; func2(x) end
end