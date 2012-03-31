LuaInspect - LuaInspect is a tool that does Lua code analysis.
It includes an extensive plugin for the SciTE [1] text editor,
there is also a plugin for the VIM editor [2], and it includes
an export to DHTML as well.

== Project Page ==

For further details, see http://lua-users.org/wiki/LuaInspect .

== Status ==

WARNING: Some of this code might not yet be stable or complete,
particularly with regards to inferencing.  It is usable for daily code editing
but you may need to sometimes fix things yourself.  Many additional
features could be added too.

== Features ==

    * analysis:
        * identifies global (red) and local variables (blue), including locals that are
	   function arguments (dark blue) and upvalues (light blue)
        * identifies unused local variables: e.g. `do local x=1 end` (white-on-blue)
        * identifies local variables masking other locals (same name): e.g. `local x=1; local x=2`
	   (strikethrough and squiggle line)
        * identifies local variables that have non-constant binding (`local x = 1; x = 2`) (italic)
        * identifies unknown global variables (white-on-red) and table fields (red), inferred by
	   static and dynamic evaluation.
        * infers values of variables (e.g. `local sum = math.pi + 2` is 5.14.
           and defined-ness of members of imported modules
          (`local mt = require "math"; math.sqrtt(2) -- undefined`)
        * infers signatures of functions (including local, global, and module functions)
        * checks number of function arguments against signatures
        * cross-references variables (locals and module fields) with their definitions and uses
	  (pink highlight), identifies range of lines/scope where the local is defined
	   and (SciTE only) supports jump-to-definition and jump-to-uses
        * identifies all keywords in selected block (underline)
        * evaluate special comments (prefixed by '!') to inject semantic information into analysis
           (similar to luaanalyze / lint).
        * basic type inferences (e.g. number + number -> number)
	* infer function return values (e.g. `function f(x) if x then return 1,2,3 else return 1,3,'z' end end`
	   returns 1, number, unknown).
	* detect dead-code (e.g. `do return end dead()`) (SciTE only) (diagonal hatching)
    * refactoring:
        * command to rename all occurrences of selected variable (SciTE only)
    * browsing:
        * inspect members of selected table.
        * select statement or comment containing current cursor selection (SciTE only)
        * display real-time annotations of all local variables, like an Excel/Mathcad worksheet
          (experimental feature via ANNOTATE_ALL_LOCALS) (currently SciTE only)
    * auto-complete typing support (SciTE only) (experimental)
    * interfaces: SciTE plugin, VIM plugin, and HTML output.

== Files in this directory ==

metalualib/* - Copy of Metalua libraries.
  Based on http://github.com/fab13n/metalua/tree/fcee97b8d0091ceb471902ee457dbccaab98234e
  with a few bug fixes (search for "PATCHED:LuaInspect" in the source).
lib/* - LuaInspect libraries.
htmllib/* - HTML resources under here.
extman/* - SciTE extman.
  Recent version compatible with LuaInspect.

== Command-line Usage (HTML output) ==

Example:

  $ ./luainspect -fhtml -lhtmllib examples.lua > examples.html

(Alternately just run "lua test.lua".  You should also do "lua luainspect"
rather than "./luainspect" on Windows.)

You will need to ensure that the JavaScript and CSS files in the
path after the "-l" argument can be found relative to the HTML file;
otherwise, the page will not display properly.

== Command-line Usage (delimited CSV output) ==

Example:

  $ ./luainspect -fdelimited examples.lua > examples.csv

== Installation in SciTE ==

First install SciTE <http://www.scintilla.org/SciTE.html>.
Version 2.12 and 2.20 work (older versions might not work).

The simple way to install LuaInspect into SciTE is to just place the
"luainspect" folder inside the same folder where your SciTE binary is
installed and add the following line to one of your SciTE properties
files (e.g. SciTEGlobal.properties or SciTEUser.properties -- consult
the SciTE documentation for where these are located):

  ext.lua.startup.script=$(SciteDefaultHome)/luainspect/extman/extman.lua

That normally is all you need to do.

If you placed LuaInspect somewhere else or are using your own version
of SciTE ExtMan (extman.lua), you will need to adjust the above to
reference the absolute path where extman.lua is installed.  LuaInspect
includes its own copy of SciTE ExtMan
<http://lua-users.org/wiki/SciteExtMan>, and it's recommended to use
the included version because older versions might not work
properly.  The files in the scite_lua subfolder are not strictly
necessary but are suggested.  In particularly, scite_lua/luainspect.lua
allows ExtMan to find LuaInspect, and you will need to adjust this if
you move LuaInspect somewhere else relative to ExtMan.

Dependencies:
  Tested with SciTE version 2.12/2.20 (older versions might not work).
  Requires http://lua-users.org/wiki/SciteExtMan (version included).
    Note: ExtMan's ctagsdx.lua is recommended (allows "goto mark"
    command to return to previous location following a "go to
    definition" or "show all variable uses").

If you want to customize styles, add the contents of the
`light_styles` or `dark_styles` variable in the scite.lua file to a
SciTE properties file.

== Configuring SciTE options ==

The following LuaInspect options can be configured in one of your
SciTE properties files:

  luainspect.update.always (0 or 1, default 1)
  luainspect.delay.count (integer >= 1, default 5)
  luainspect.annotate.all.locals (0 or 1, default 0)
  luainspect.incremental.compilation (0 or 1, default 1)
  luainspect.performance.tests (0 or 1, default 0)
  luainspect.autocomplete.vars (0 or 1, default 0)
  luainspect.autocomplete.syntax (0 or 1, default 0)
  luainspect.path.append (string, default '')
  luainspect.cpath.append (string, default '')
  style.script_lua.scheme (string, '' or 'dark', default '')

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

Local variables named '_' are ignored for purposes of unused/masking variable
reporting.  Typical use case: `for _, v in ipairs(t) do <. . .> end`.

== LICENSE ==

See LICENSE file.

== Credits ==

David Manura, original author.
Steve Donovan for discussions on design, SciTE and ExtMan.
Fabien Fleutot for Metalua and discussions.
SciTE suggestions/fixes by Tymur Gubayev.
Peter Odding for VIM editor support [2].
Jon Akhtar - csv output and IntelliJ discussions.

== Bugs ==

Please report bugs via github <http://github.com/davidm/lua-inspect/issues>
or just "dee em dot el you ae at em ae tee ayche two dot ow ar gee", or
if you prefer neither then append to the wiki page
<http://lua-users.org/wiki/LuaInspect>.

== References ==

[1] http://www.scintilla.org/SciTE.html
[2] http://peterodding.com/code/vim/lua-inspect/ - VIM editor support
