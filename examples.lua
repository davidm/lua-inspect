-- some examples/tests.   -*- coding: utf-8 -*-

local mt = require "math"

-- unicode test (this should not break highlighting)
do print("Δ™«»∂≈") end -- Δ™«»∂≈

-- Basic variable scope and usage tests
local unused_local = 1
local used_local = 2; print(used_local)
local reassigned_local = 1; reassigned_local = 2
local upval_local; function f() return upval_local end
local reassigned_upval_local; function f() reassigned_upval_local = 2 end
function f(param_unused_local, param_used_local, param_reassigned_local, param_upval_local, param_reassigned_upval_local)
  print(param_used_local)
  param_reassigned_local = 2
  return function()
    print(param_upval_local)
    param_reassigned_upval_local = 2
  end
end
print(undefined_global)
print(math) -- predefined global
print(defined_global); defined_global = 2; print(defined_global)

-- Scope tests for specific statements
do local local1; for local1=local1,2 do end end  -- used, unused+mask, used local
do local local1; for local1 in local1 do end end  -- used, unused+mask, used local
do local local1; local local1 = local1 end -- used, unused+mask, used local
do local function local1() local1() end end -- used, used local
do local local1; local local1 = function() local1() end end -- used, unused+mask, used local
do -- test repeat-until
  local local1  -- unused local
  repeat
    local local1 -- unused local+mask
    local local1 -- used local+mask
  until local1 -- used local
end
do -- test local var scope stays inside block
  repeat local v2 until false
  while false do local v3 end
  for v4=1,1 do local v5 end
  for v6 in nil do local v6 end
  print(v2, v3, v4, v5, v6) -- undefined globals
end
do  -- more masking testss
  local abc,abc -- not mask, mask
  local function bcd(bcd, abc, cde) local bcd end -- not mask, mask, mask, mask, not mask
  for cde, cde in pairs{} do local cde end -- not mask, mask, mask
  for def=1,2 do local def end -- not mask, mask
  function abc:def() local self end  -- not mask, mask
  function abc:def() local self end  -- not mask, mask
  function abc:def(self) end -- not mask, mask
end
for _,x in ipairs{} do local _,x = function(_,x)end end -- ignore unused/masking '_'

-- Field accesses
math.sqrt(math.pi)  -- statically+dynamically defined fields
math["sqrt"](2)  -- statically+dynamically defined field (this works too)
math.undefinedfield(math.pii)
_G.math.sqrt(2) -- dynamically defined (IMPROVE? statically defined too)
_=package.loaded  -- statically+dynamically defined field
_=package.loaded.math  -- dynamically defined field, deeply nested
_=package.loaded.undefinedfield
local root = math.sqrt; root(2) -- IMPROVE: statically define
math:sqrt(2) -- statically+dynamically defined invoke (although non-sensical - IMPROVE?)
math:undefmethod(2)
local t = {x=1, {y={z=2}}, f = function() end}
print(t.forwarddeclared(), t.undef()) -- recognized (forward declared), unrecognized
function t.forwarddeclared() end -- define
t.y = t.x, t[1].y.z, t[1].y.undef + t.undef, t.f().undef   --OK?
;("abc"):upper():lower()  -- dynamically defined (IMPROVE? statically defined too)
local m = math; local mm = {sqrt=math.sqrt}; print(m.sqrt, mm.sqrt, math.sqrt) --OK?

-- infer values
local pi = math.pi -- 3.14...
local a1 = math.fmod(12, 10) == 2 -- true (safe function)

-- more value inferences
local loc1 = 3
loc1=4
print(loc1) -- IMPROVE? infer value even though binding mutable?

-- luaanalyze style comments.
--! context.apply_value('shape$', {color='?', size={x=1,y=1}, f=function() return "?" end})
function f(myshape) print(myshape.color, myshape.size.x, myshape.undef, myshape.f():len()) end
--IMPROVE: `len` above

-- Argument count checks.
function zero() end
function one(a) end
function two(a,b) end
function oneplus(a,...) end
function zeroplus(...) end
zero() zero(1) zero(1,2)
one() one(1) one(1,2)
one(f()) one(1,zero()) one(1,2,zero())
two() two() two(1,2)
oneplus() oneplus(1) oneplus(1,2) oneplus(1,2,3)
zeroplus()
math.sqrt(1) math.sqrt(1,2) _G.math.sqrt(1,2)
local sq = math.sqrt
sq(1,2)
function f(...)
  one(...) one(1, ...) one(1, 2, ...)
end
local tt = {zero=zero,one=one, more={one=one}}  -- test methods
tt:zero() tt:zero(1)
tt:one() tt:one(1)
tt.more:one() tt.more:one(1)

-- return values (instructions: inspect `fa`)
local function fa() end -- no returns
local function fa() return nil end -- returns nil
local function fa() return 2 end -- return 2
local function fa(x,y) return 2,x>y end -- return 2, 'boolean' (FIX:returns 2,'unknown')
local function fa(x) if x then return 1,2,3 else return 1,3,'z',nil end return 'z' end
  -- returns 1, number, unknown, unknown (note deadcode)
local function fa(x) if x then return 2 end end -- returns unknown (due to implicit return)
local function fa(x) do return 2 end return 3 end -- returns 2 (note deadcode)
local function fa(x) return (function() return 2 end)()+1 end -- returns 3
local function fa(x) return x end -- return unknown
local x1 = fa(5) -- unknown
  -- note: "infer 5" is not implemented (i.e. return values specific
  -- to function call arguments)  It could infer, however,
  -- that fa is a "safe function" to execute.
local function fa(...) return ... end --FIX
local function fa(f) return 2,f() end --FIX
  --TODO: multiple returns not inferred
  
-- expression lists from function returns
local a1,a1 = (function() return 1,2 end)() -- 1,2
local function zero() end
local function one() return 'a' end
local function two() return 'a', 'b' end
local a1, a2 = zero() -- nil, nil
local a1, a2 = one() -- 'a', nil
local a1, a2, a3 = two() -- 'a', 'b', nil
local a1, a2, a3 = two(), 'c' -- 'a', 'c', nil
local a1, a2, a3, a4 = 'z', two()  -- 'z', 'a', 'b', nil
ga1, ga2, ga3, ga4 = 'z', two()  -- 'z', 'a', 'b', nil  (global sets)
local tt = {}; tt.ga1, tt.ga2, tt.ga3, tt.ga4 = 'z', two()  -- 'z', 'a', 'b', nil  (index sets)
local a1, a2, a3 = two(), unknownfunc()  -- 'a', unknown, unknown
math.atan2(function() return 2, 3 end) -- FIX: arg count ok
math.atan2(function() return 2, 'x' end) -- FIX: arg type mismatch
math.atan2(unknownfunc()) -- FIX: arg count could be ok
math.atan2(1,2, unknownfunc()) -- FIX: arg count could be ok
  
-- deadcode detection
local deadcode
local function f(x)
  if false then deadcode()
  elseif 0==1 then deadcode() deadcode()
  elseif 1==1 then print 'ok'
    while 0==1 do deadcode() end
    do return end
    deadcode() if x then end while 1 do end
  else
    deadcode()
  end
end
--test: do return end deadcode()

-- error messages
do
  local n
  local z1,z2 = true,false
  local xd1 = z1 + z2  -- error, arithmetic on boolean
  local xd2 = true + 5 -- error, arithmetic on boolean literal
  local xd3 = n^n  -- error, arithmetic on nil
  local xd4 = z1.zz  -- error, index bool
  local xd4b = z1:zz() -- error, index bool in meth call
  local xd5 = #z1 -- error, len of bool
  local xd6 = g11 + g22  -- error, arithmetic on global nil
end

-- type inferences
do
  local n1, n2 --! context.apply_value('^n.*', number)
  local s1, s2 --! context.apply_value('^s.*', string)
  local b1, b2 --! context.apply_value('^b.*', boolean)
  local x1,y1 = n1+n2, n1+2 -- number
  local x2,y2 = n1 or n2, n1 or 2  -- number
  local x3,y3 = n1 > n2, n1 > 2 -- boolean
  local x4,y4 = -n1, -2 -- number, -2
  local x5,y5 = not n1, not 2 -- boolean, false
  local xb1,yb1 = s1+s2, s1+"z" -- number
  local xb2,yb2 = s1 or s2, s1 or "z" --  string
  local xb3,yb3 = s1 .. s2, s1 .. "z"  -- string
  local xb4,yb4 = s1 > s2, s1 > "z"  -- boolean
  local xc1,yc1 = b1 and b2, b1 and true  -- boolean
  local e1,ey1 = #n1, #2  -- error
  local e2,ey2 = -b1, -true   -- error
  local e3,ey3 = #b1, #true  -- error
  local xd1 = n1+n2^2 * n2 or 4 -- number
  local xe1 = math.sqrt(n1) -- number
  local xe2 = math:sqrt() -- number (although nonsensical)
  for ii=1,10 do print(ii) end -- number
  for a1,a2,a3 in ipairs(t) do print(a1,a2,a3) end -- number, unknown, nil
  for a1,a2,a3 in pairs(t) do print(a1,a2,a3) end -- unknown, unknown, nil
  for a1,a2,a3 in it(t) do print(a1,a2,a3) end -- unknown, unknown, unknown
end
