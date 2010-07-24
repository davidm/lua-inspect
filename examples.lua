-- some examples/tests

local mt = require "math"

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