LuaInspect - LuaInspect is a tool that does Lua code analysis.
It includes an extensive plugin for the SciTE [1] text editor,
there is also a plugin for the VIM editor [2], and it includes
an export to DHTML as well.

WARNING: This code is not yet stable.  It is usable though
you may need to fix things yourself.  Many additional features
could be added too.

For further details, see http://lua-users.org/wiki/LuaInspect .

== Features ==

    * cross-references local variables with their definitions and uses (pink highlight)
    * shows all keywords in selected block (underline)
    * identifies global (red) and local variables (blue)
    * identifies function arguments (dark blue)
    * identifies global variables that are probably undefined (white-on-red)
    * identifies local variables that have non-constant binding (local x = 1; x = 2) (italic)
    * identifies unused variables: e.g. do local x=1 end
    * identifies local variables making other locals (same name): e.g. local x=1; local x=2 (strikethrough)
    * displays signatures of known global functions
    * identifies range of lines/scope where the local is defined (currently SciTE only) 
    * identifies fields and methods as interrogatable variables (e.g. a.b or a:b)
    * infers values of variables (e.g. `local sum = math.pi + 2` is 5.14.
       and defined-ness of members of imported modules:
       `local mt = require "math"; math.sqrtt(2) -- undefined`
    * jump (goto) definition of selected variable (currently locals only in SciTE)
    * list all uses of selected variable (currently locals only in SciTE)
    * select statement or comment containing current cursor selection (SciTE only)
    * command to rename all occurrences of selected variable (SciTE only)
    * inspect members of selected table.
    * display real-time annotations of all local variables, like an Excel/Mathcad worksheet
      (experimental feature via ANNOTATE_ALL_LOCALS) (currently SciTE only)
    * Evaluate special comments (prefixed by '!') to inject semantic information into analysis
       (similar to luaanalyze).

== Files in this directory ==

metalualib/* - Copy of Metalua libraries under here
luainspectlib/* - LuaInspect libraries under here
htmllib/* - HTML resources under here

== Command-line Usage (HTML output) ==

Example:

  $ lua luainspectlib/luainspect/command.lua  examples.lua > test-output/examples.html

== Installation in SciTE ==

Install SciTE version.  Version 2.12 works (older versions might not work).  

First install http://lua-users.org/wiki/SciteExtMan .

Add this to your SciTE Lua startup script (but change LUAINSPECT_PATH):

=============================
local LUAINSPECT_PATH = "c:/luainspect"
package.path = package.path .. ";" .. LUAINSPECT_PATH .. "/metalualib/?.lua"
package.path = package.path .. ";" .. LUAINSPECT_PATH .. "/luainspectlib/?.lua"
require "luainspect.scite" : install()
=============================

Dependencies:
  Tested with SciTE version 2.12 (older versions might not work).  
  Requires http://lua-users.org/wiki/SciteExtMan .
  ctagsdx.lua from the full SciteExtMan is optional (allows "goto mark" command
    to return to previous location following a "go to definition" or "show all variable uses").

If you want to customize styles, add the contents of the
`light_styles` or `dark_styles` variable in the scite.lua file to your
SciTEGlobal.properties, SciTEUser.properties, or SciTE.properties file.

== Configuring SciTE options ==

The following LuaInspect options can be configured in one of your
SciTE property files:

  luainspect.update.always (0 or 1, default 1)
  luainspect.delay.count (integer >= 1, default 5)
  luainspect.annotate.all.locals (0 or 1, default 0)
  luainspect.incremental.compilation (0 or 1, default 1)
  luainspect.performance.tests (0 or 1, default 0)

For details, see scite.lua.

== Installation on VIM ==

See [2] for VIM editor support.

== Preliminary support for luaanalyze style comments ==

To make all variables in scope match name 'ast$' be recognized by LuaInspect as a
table with field 'tag' of type string, add this to your code:

  --! context.apply_value('ast$', {tag=''})

The LuaInspect code itself uses this:

  --! require 'luainspect.typecheck' (context)

== Design Notes ==

The font styles are intended to make the more dangerous
or questionable code stand out more.

== LICENSE ==

See LICENSE file.

== Credits ==

David Manura, original author.
Steve Donovan for discussions on design and SciTE.
Fabien Fleutot for Metalua and discussions.
SciTE suggestions by Tymur Gubayev.
Peter Odding for VIM editor support [2]

== References ==

[1] http://www.scintilla.org/SciTE.html
[2] http://peterodding.com/code/vim/lua-inspect/ - VIM editor support

== Changes ==

20100806
  SciTE: jump to uses, not jumps to exact position, not just line number
  SciTE: mark lines of invalidated code upon introducing code errors and display
            error message below invalidated code (not on exact line of error)
  SciTE: add styling delay option to improve performance (luainspect.update.delay)

20100805
  core: Major internal refactoring to simplify incremental compilation
          (lineinfo managed in tokenlist).  Breaks API.
  core/SciTE/HTML: identifies local variables that mask other locals (same name):
         e.g. local x=1; local x=2 (strikethrough)
  core: added version number variable APIVERSION to luainspect.init.
  HTML: highlight keywords in selected block
  SciTE: the incremental compilation feature is now on by default.

20100803
  core:Evaluate special comments (prefixed by '!') to inject semantic information into analysis
         (similar to luaanalyze).
  core: Further work on incremental compilation feature.

20100802
  core: improve field value inferences
  SciTE: improve dark style clarity
  SciTE: make margin markers for variable scope and block mutually exclusive

20100731
  SciTE: allow styles in properties to be specified by name and more flexibly overridden.
  SciTE: add optional dark style
  SciTE/HTML: support mutate upvalues, cleanup styles
  SciTE: improve keyword highlighting (always highlight containing block)

20100730
  core: fix scoping of `for` statements (in globals.lua)
  core/SciTE: highlight keywords and show all keywords in selected statement.

20100729
  SciTE: options can now be set with SciTE properties.
  SciTE: refactor: select statement
  core/SciTE: more work on incremental compilation (luainspect.incremental.compilation)

20100728
  core/SciTE: add command to select statement or comment containing current cursor selection.
  core/SciTE: experimental incremental compilation option (ALLOW_INCREMENTAL_COMPILATION)
  core/SciTE: add special styling (background color) for tab whitespace

20100727
  SciTE: Fix limited styling range may skip styling (broke in 20100726)

20100726
  SciTE: apply default styles in script if not specified in properties file.
  SciTE: initial implementation of folding (but currently disabled due to SciTE problems)
  SciTE: improve OnStyle only over provided byte range
  Note: you may now remove LuaInspect styles from your properties file.

20100725
  SciTE: fix memory overflow when code contains buffer.notes.

20100724
  SciTE: list all uses of selected variable (currently locals only)
  SciTE: display errors about mismatched blocks or parens at both top and bottom of problem
  SciTE: support shebang line

20100723
  core/SciTE/HTML: Initial support for table fields
  core/SciTE: initial dynamic value determination
  core: fix recursive local scoping (`Localrec) in globals.lua
  SciTE: Mark all range of selected variable's scope in margin
  SciTE: New command to rename all occurrences of selected variable
  SciTE: Significant performance gain utilizing loadstring in addition
    to metalua libraries
  SciTE: Mark upvalues (lighter blue)
  SciTE: Fix handling multiple buffers.
  SciTE: display variable info on double click
  SciTE: display real-time annotations of all local variables, like a Mathcad worksheet
         (experimental feature via ANNOTATE_ALL_LOCALS)
  SciTE: jump (goto) definition of selected variable (currently locals only)
           ctagsdx.lua from the full SciteExtMan is optional (allows "goto mark" command
           to return to previous location following a "go to definition").
  SciTE: add command to inspect table contents.
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

20100719
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
