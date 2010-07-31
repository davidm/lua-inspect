-- some examples/tests

local mt = require "math"

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
do local local1; for local1=local1,2 do end end  -- used, unused, used local
do local local1; for local1 in local1 do end end  -- used, unused, used local
do local local1; local local1 = local1 end -- used, unused, used local
do local function local1() local1() end -- used, used local
do local local1; local local1 = function() local1() end end end -- used, unused, used local
do -- test repeat-until
  local local1  -- unused local
  repeat
    local local1 -- unused local
    local local1 -- used local
  until local1 -- used local
end
do -- test local var scope stays inside block
  repeat local v2 until false
  while false do local v3 end
  for v4=1,1 do local v5 end
  for v6 in nil do local v6 end
  print(v2, v3, v4, v5, v6) -- undefined globals
end

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
math:undefinedmethod(2)
local t = {{x=1}, x=1, f = function() end, a = {b=2}}
t.y = t.x + t.z + t.y + t.a.b + t.a.c, t[1].x   --IMPROVE?
;("abc"):upper():lower()  -- dynamically defined (IMPROVE? statically defined too)

