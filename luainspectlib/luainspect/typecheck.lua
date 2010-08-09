-- luainspect.typecheck - Type definitions used to check LuaInspect itself.
--
-- (c) 2010 David Manura, MIT License.

return function(context)
  -- AST type.
  local ast = {
    tag = '', lineinfo={first={comments={}},last={comments={}}},
    isfield=true, tag2='', value=1, valueknown=true, idxvalue=1, idxvalueknown=true,
    resolvedname='', definedglobal=true, id=1, isparam=true, isset=true, isused=true,
    functionlevel=1, localmasked=true}
  ast.localdefinition=ast; ast.previous = ast; ast.seevalue = ast; ast.localmasking = ast
  ast[1] = ast; ast[2] = ast
  context.apply_value('ast$', ast)

  -- Token type.
  context.apply_value('token$', {
    tag='?', fpos=1, lpos=1, keywordid=1, ast=ast, [1]='?'
  })
  
  -- Lua source code string type.
  context.apply_value('src$', '')

  -- SciTE syler object type.
  local nf = function()end
  context.apply_value('^styler$', {SetState=nf, More=nf, Current=nf, Forward=nf, StartStyling=nf, EndStyling=nf, language=''})
end
