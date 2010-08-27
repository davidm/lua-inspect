-- luainspect.html - Convert AST to HTML using LuaInspect info embedded.
--
-- (c) 2010 David Manura, MIT License.

--! require 'luainspect.typecheck' (context)

local M = {}

local LS = require "luainspect.signatures"
local T = require "luainspect.types"

-- FIX!!! improve: should be registered utility function
local function escape_html(s)
  return s:gsub('&', '&amp;'):gsub('<', '&lt;'):gsub('>', '&gt;'):gsub('"', '&quot;')
end

local function annotate_source(src, ast, tokenlist, emit)
  local start = 1
  local fmt_srcs = {}
  for _,token in ipairs(tokenlist) do
    local fchar, lchar = token.fpos, token.lpos
    if fchar > start then
      table.insert(fmt_srcs, emit(src:sub(start, fchar-1)))
    end
    table.insert(fmt_srcs, emit(src:sub(fchar, lchar), token))
    start = lchar + 1
  end
  if start <= #src then
    table.insert(fmt_srcs, emit(src:sub(start)))
  end
  return table.concat(fmt_srcs)
end

function M.ast_to_html(ast, src, tokenlist)
  local src_html = annotate_source(src, ast, tokenlist, function(snip_src, token)
  local snip_html = escape_html(snip_src)
  if token then
    local ast = token.ast
    if token.tag == 'Id' or ast.isfield then
      local class = 'id'
      local desc_html = escape_html(class)

      if ast.localdefinition then
        class = class .. ' local'
        desc_html = desc_html .. ' local'
        if ast.functionlevel > ast.localdefinition.functionlevel then
          class = class .. ' upvalue'
          desc_html = desc_html .. ' upvalue'
        end
        if not ast.localdefinition.isused then
          class = class .. ' unused'
        end
        if ast.localdefinition.isset then
          class = class .. ' mutatebind'
          desc_html = desc_html .. ' mutate-bind'
        else
          class = class .. ' constbind'
        end
        if ast.isparam then
          class = class .. ' param'
          desc_html = desc_html .. ' param'
        end
        if ast.localmasking then
          class = class .. ' masking'
          desc_html = desc_html .. ' masking'
        end
        if ast.localmasked then
          class = class .. ' masked'
          desc_html = desc_html .. ' masked'
        end
        if ast.localdefinition.lineinfo then
          local linenum = ast.localdefinition.lineinfo.first[1]
          desc_html = desc_html .. ' defined-line:' .. linenum
        end
      elseif ast.isfield then
        class = class .. ' field'
        desc_html = desc_html .. ' field'
        local val = ast.seevalue.value
        if ast.definedglobal or val ~= T.universal and not T.iserror[val] and val ~= nil then
          class = class .. ' recognized'
          desc_html = desc_html .. ' recognized'
        else
          class = class .. ' unrecognized'
          desc_html = desc_html .. ' unrecognized'
        end
      else -- global
        class = class .. ' global'
        desc_html = desc_html .. ' global'
        if ast.definedglobal then
          class = class .. ' recognized'
          desc_html = desc_html .. ' recognized'
        else
          class = class .. ' unrecognized'
          desc_html = desc_html .. ' unrecognized'
        end
      end
      
      if ast.id then
        class = class .. " id" .. ast.id
      elseif ast.id then
        class = class .. " id" .. ast.localdefinition.id
      end

      if ast.resolvedname and LS.global_signatures[ast.resolvedname] then
        local name = ast.resolvedname
        desc_html = desc_html .. "<br>" .. escape_html(LS.global_signatures[name])
      end
      return "<span class='" .. class .. "'>" .. snip_html .. "</span><span class='info'>" .. desc_html .. "</span>"
    elseif token.tag == 'Comment' then
      return "<span class='comment'>" .. snip_html .. "</span>"
    elseif token.tag == 'String' then -- note: excludes ast.isfield
      return "<span class='string'>" .. snip_html .. "</span>"
    elseif token.tag == 'Keyword' then
      local id = token.keywordid and 'idk' .. tostring(token.keywordid) or ''
      return "<span class='keyword " .. id .. "'>" .. snip_html .. "</span>"
    end
  end
  return snip_html
 end)

 local function add_linenums(src_html)
  local out_htmls = {}
  local linenum = 1
  for line in src_html:gmatch("[^\n]*\n?") do
    if line == "" then break end
    table.insert(out_htmls, string.format("%5d: ", linenum) .. line)
    linenum = linenum + 1
  end
  return table.concat(out_htmls)
 end

 src_html = add_linenums(src_html)

 src_html = [[
 <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
  "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
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
