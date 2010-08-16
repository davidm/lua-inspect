local T = {} -- types

-- istype[o] iff o represents a type (i.e. set of values)
T.istype = {}

-- Number type
T.number = {}
setmetatable(T.number, T.number)
function T.number.__tostring(self)
  return 'number'
end
T.istype[T.number] = true

-- String type
T.string = {}
setmetatable(T.string, T.string)
function T.string.__tostring(self)
  return 'string'
end
T.istype[T.string] = true

-- Boolean type
T.boolean = {}
setmetatable(T.boolean, T.boolean)
function T.boolean.__tostring(self)
  return 'boolean'
end
T.istype[T.boolean] = true

-- Error type
local CError = {}; CError.__index = CError
function CError.__tostring(self) return "error:" .. tostring(self.value) end
function T.error(val)
  return setmetatable({value=val}, CError)
end
T.istype[T.error] = true

return T
