-- luainspect.csv - Convert AST to CSV using LuaInspect info embedded.
--

--! require 'luainspect.typecheck' (context)

local M = {}

local LS = require"luainspect.signatures"
local T = require"luainspect.types"

function M.ast_to_csv(ast, src, tokenlist)
  local start = 1
  local fmt_tokens = {}
  for _, token in ipairs(tokenlist) do
    local fchar, lchar = token.fpos, token.lpos
    local desc = describe(token)
    if desc then
      fmt_tokens[#fmt_tokens + 1] = ("%d,%d,%s\n"):format(fchar, lchar, desc)
    end
  end
  return table.concat(fmt_tokens)
end


function describe(token)
  if token then
    local ast = token.ast
    if token.tag == 'Id' or ast.isfield then
      local class = 'id'

      if ast.localdefinition then
        class = class .. ',local'
        if ast.functionlevel > ast.localdefinition.functionlevel then
          class = class .. ',upvalue'
        end
        if not ast.localdefinition.isused then
          class = class .. ',unused'
        end
        if ast.localdefinition.isset then
          class = class .. ',mutatebind'
        else
          class = class .. ',constbind'
        end
        if ast.isparam then
          class = class .. ',param'
        end
        if ast.localmasking then
          class = class .. ',masking'
        end
        if ast.localmasked then
          class = class .. ',masked'
        end
      elseif ast.isfield then
        class = class .. ',field'
        local val = ast.seevalue.value
        if ast.definedglobal or val ~= T.universal and not T.iserror[val] and val ~= nil then
          class = class .. ',recognized'
        else
          class = class .. ',unrecognized'
        end
      else -- global
        class = class .. ',global'
        if ast.definedglobal then
          class = class .. ',recognized'
        else
          class = class .. ',unrecognized'
        end
      end

      if ast.id then
        class = class .. ",id" .. ast.id
      elseif ast.id then
        class = class .. ",id" .. ast.localdefinition.id
      end

      if ast.resolvedname and LS.global_signatures[ast.resolvedname] then
        local name = ast.resolvedname
        class = class .. "," .. LS.global_signatures[name]
      end
      return class
    end
  end
end


return M
