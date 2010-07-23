LuaInspect - LuaInspect is a tool that does Lua code analysis.  The
result of this analysis can also be rendered as HTML.
This tool can also plug into the SciTE [1] text editor.

WARNING: This is a preliminary, and there likely are
bugs, so it's not yet intended for production.  It is usable though,
but you may need to fix things yourself.  Many additional features
could be added too.  No commitment has been made by the author to
advance this project beyond its prototype stage.

Usage: lua luainspect.lua myprogram.lua > myprogram.html

For further details, see
  http://lua-users.org/wiki/LuaInspect

== Files in this directory ==

metalualib/* - Copy of Metalua libraries under here
luainspectlib/* - LuaInspect libraries under here
htmllib/* - HTML resources under here

== Command-line Usage (HTML output) ==

Example:

  $ lua luainspectlib/luainspect/command.lua  luainspectlib/luainspect/init.lua
> test-output/init.html

== Installation in SciTE ==

Install SciTE version.  Version 2.12 works (older versions might not work).  

First install http://lua-users.org/wiki/SciteExtMan .

Add this to your SciTEGlobal.properties, SciTEUser.properties, or SciTE.properties file:

=============================
lexer.*.lua=script_lua
style.script_lua.0=fore:#000000 # default
style.script_lua.1=fore:#000080 # local
style.script_lua.2=fore:#600000  # recognized global
style.script_lua.3=fore:#ffffff,back:#ff0000,bold  # unrecognized global
style.script_lua.4=fore:#008000  # comment
style.script_lua.5=fore:#00c000  # string
style.script_lua.6=fore:#000080,italics,  # local mutate
style.script_lua.7=fore:#ffffff,back:#0000ff  # local unused
style.script_lua.8=fore:#000040 # local param
style.script_lua.9=fore:#800000,back:#ffffc0  # compiler error
style.script_lua.10=fore:#0000ff # local upvalue
style.script_lua.11=fore:#c00000 # table field
style.script_lua.12=fore:#600000 # table field recognized
=============================

Add this to your SciTE Lua startup script (but change LUAINSPECT_PATH):

=============================
local LUAINSPECT_PATH = "c:/luainspect"
package.path = package.path .. ";" .. LUAINSPECT_PATH .. "/metalualib/?.lua"
package.path = package.path .. ";" .. LUAINSPECT_PATH .. "/luainspectlib/?.lua"
require "luainspect.scite" : install()
=============================

Depenedencies:
  Tested with SciTE version 2.12 (older versions might not work).  
  Requires http://lua-users.org/wiki/SciteExtMan

== LICENSE ==

See LICENSE file.

== Credits ==

David Manura, original author.
Steve Donovan for discussions on design and SciTE.
Fabien Fleutot for Metalua.  

== References ==

[1] http://www.scintilla.org/SciTE.html

== Changes ==

?
  SciTE: Mark all range of selected variable's scope in margin
  SciTE: New command to rename all occurrences of selected variable
  SciTE: Significant performance gain utilizing loadstring in addition
    to metalua libraries
  SciTE: Mark upvalues (lighter blue)
  SciTE: Fix handling multiple buffers.
  SciTE: display variable info on double click
  SciTE/HTML: Initial support for table fields
  Note: SciTE*.properties and luainspect.css have been updated; please update when upgrading

20100720
  core: support for detecting unused locals (white on blue)
  SciTE: display callinfo help on top-level standard library globals
  SciTE: display local parameters distinctly (dark blue)
  SciTE: display compiler errors as annotations
  SciTE: partial workaround for conflict with other lexers
  SciTE: option to recompile only when cursor line number changes to improve performance
            and reduce error reporting (set UPDATE_ALWAYS to true in scite.lua to enable this)
  SciTE: workaround for Metalua libraries sometimes not returning line number in error report
  Note: SciTE*.properties and luainspect.css have been updated; please update when upgrading

20100719 -
  core: Fixed "repeat" statement scope handling (globals.lua)
  SciTE: Improve performance (not recompile when code not changing)
  SciTE: Add "!" marker near compiler error.
  SciTE: Add hotspots on local variables

20100717-2
  SciTE: highlight all instances of selected identifier
  Now requires http://lua-users.org/wiki/SciteExtMan

20100717
  added initial SciTE text editor plugin

20100622
  initial version with HTML output

--David Manura
