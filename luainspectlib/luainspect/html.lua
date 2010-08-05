-- luainspect.html - Convert AST to HTML using LuaInspect info embedded.
--
-- (c) 2010 David Manura, MIT License.

--! require 'luainspect.typecheck' (context)

local M = {}

local LS = require "luainspect.signatures"

-- FIX!!! improve: should be registered utility function
local function escape_html(s)
  return s:gsub('&', '&amp;'):gsub('<', '&lt;'):gsub('>', '&gt;'):gsub('"', '&quot;')
end

local function annotate_source(src, ast, tokenlist, emit)
  local start = 1
  local fmt_srcs = {}
  for _,token in ipairs(tokenlist) do
    local fchar, lchar = token[1], token[2]
    if fchar > start then
      table.insert(fmt_srcs, emit(src:sub(start, fchar-1)))
    end
    table.insert(fmt_srcs, emit(src:sub(fchar, lchar), token))
    start = lchar + 1
  end
  if start < #src then
    table.insert(fmt_srcs, emit(src:sub(start)))
  end
  return table.concat(fmt_srcs)
end

function M.ast_to_html(ast, src, tokenlist)
 local src_html = annotate_source(src, ast, tokenlist, function(snip_src, token)
  local snip_html = escape_html(snip_src)
  if token then
    if token.type == 'comment' then
      return "<span class='comment'>" .. snip_html .. "</span>"
    elseif token.type == 'string' then
      return "<span class='string'>" .. snip_html .. "</span>"
    elseif token.type == 'global' or token.type == 'local' or token.type == 'field' then -- Id
      local class = token.type
      local desc_html = escape_html(class)

      if token.type == 'global'  then
        if token.definedglobal then
          class = class .. ' recognized'
          desc_html = desc_html .. ' recognized'
        else
          class = class .. ' unrecognized'
          desc_html = desc_html .. ' unrecognized'
        end
      elseif token.type == 'local' then
        if token.ast.functionlevel > token.ast.localdefinition.functionlevel then
          class = class .. ' upvalue'
          desc_html = desc_html .. ' upvalue'
        end
        if not token.ast.localdefinition.isused then
          class = class .. ' unused'
        end
        if token.ast.localdefinition.isset then
          class = class .. ' mutatebind'
          desc_html = desc_html .. ' mutate-bind'
        else
          class = class .. ' constbind'
        end
        if token.isparam then
          class = class .. ' param'
          desc_html = desc_html .. ' param'
        end
        if token.ast.localdefinition.lineinfo then
          local linenum = token.ast.localdefinition.lineinfo.first[1]
          desc_html = desc_html .. ' defined-line:' .. linenum
        end
      elseif token.type == 'field' then
        if token.definedglobal or token.ast.seevalue.value ~= nil then
          class = class .. ' field recognized'
          desc_html = desc_html .. ' field recognized'
        else
          class = class .. ' field unrecognized'
          desc_html = desc_html .. ' field unrecognized'
        end
      end
      
      local id_html = ''
      if token.ast.id then
        id_html = " id='id" .. token.ast.id .. "'"
        class = class .. " id" .. token.ast.id
      elseif token.ast.id then
        class = class .. " id" .. token.ast.localdefinition.id
      end

      if token.ast.resolvedname and LS.global_signatures[token.ast.resolvedname] then
        local name = token.ast.resolvedname
        desc_html = desc_html .. "<br>" .. escape_html(LS.global_signatures[name])
      end
      return "<span class='id " .. class .. "'" .. id_html .. ">" .. snip_html .. "</span><span class='info'>" .. desc_html .. "</span>"
    end
  end
  return snip_html
 end)

 local function add_linenums(src_html)
  local out_htmls = {}
  local linenum = 1
  for line in src_html:gmatch(".-\n") do
    table.insert(out_htmls, string.format("%5d: ", linenum) .. line)
    linenum = linenum + 1
  end
  return table.concat(out_htmls)
 end

 src_html = add_linenums(src_html)

 src_html = [[
<html>
<head>
  <title></title>
  <script src="jquery-1.4.2.min.js" type="text/javascript"></script>
  <script src="luainspect.js" type="text/javascript"></script>
  <link rel="stylesheet" type="text/css" href="luainspect.css">
</head>
<body><pre>
]] .. src_html .. "</pre></body></html>"

 return src_html
end

return M
