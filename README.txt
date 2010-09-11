LuaInspect - LuaInspect is a tool that does Lua code analysis.
It includes an extensive plugin for the SciTE [1] text editor,
there is also a plugin for the VIM editor [2], and it includes
an export to DHTML as well.

== Project Page ==

For further details, see http://lua-users.org/wiki/LuaInspect .

== Status ==

WARNING: This code is not yet stable.  It is usable but you
may need to sometimes fix things yourself.  Many additional
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
        * checks number of function arguments against signatures (SciTE only)
        * cross-references variables (locals and module fields) with their definitions and uses
	  (pink highlight), identifies range of lines/scope where the local is defined
	   (currently SciTE only), and supports jump-to-definition and jump-to-uses (SciTE only)
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
luainspectlib/* - LuaInspect libraries.
htmllib/* - HTML resources under here.
extman/* - SciTE extman.
  Recent version compatible with LuaInspect.

== Command-line Usage (HTML output) ==

Example:

  $ lua luainspectlib/luainspect/command.lua  examples.lua > test-output/examples.html

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

== LICENSE ==

See LICENSE file.

== Credits ==

David Manura, original author.
Steve Donovan for discussions on design, SciTE and ExtMan.
Fabien Fleutot for Metalua and discussions.
SciTE suggestions/fixes by Tymur Gubayev.
Peter Odding for VIM editor support [2].

== Bugs ==

Please report bugs via github <http://github.com/davidm/lua-inspect/issues>
or just "dee em dot el you ae at em ae tee ayche two dot ow ar gee", or
if you prefer neither then append to the wiki page
<http://lua-users.org/wiki/LuaInspect>.

== References ==

[1] http://www.scintilla.org/SciTE.html
[2] http://peterodding.com/code/vim/lua-inspect/ - VIM editor support

== Changes ==

20100911
  [+] core: infer types of for loop variables.

20100827
  [+] core: infer sets involving functions with multiple returns.
     e.g. local a,b = (function() return 1,2 end)()
  [!] core:fix: do not infer table sets on LuaInspect types.

20100825
  [*] SciTE: simplify install (use default path)
  [!] core: fix: function params should infer to unknown values
  [!] core: fix: infer: unknown functions return unknown values

20100823
  [*] SciTE: change Ctrl-Alt-W to Ctrl-Alt-E
  [!] SciTE: fix bookmarking (Ctrl+F2)
  [+] SciTE: bundle copy of extman.lua

20100821
  [+!] core: return analysis enabled following fixes

20100820
  [!] SciTE: fix folding performance problem (though folding still disabled by default
      due to OnStyle recursion problem)

20100819
  [!] core: fix tokenlist when opcode operands reversed lexically
  [*] metalua/performance - avoid overriding builtin pairs/ipairs
  [*] SciTE: plugin now loaded as Lua extension script (not globally).

20100818
  [!] HTML: fix missing chars at end-of-file
  [!] Metalua: fix lexer line number count off-by-one error
  [!] SciTE: fix Unicode/UTF-8 encoding breaking formatting
  [!] core: fix performance problem with tinsertlist function
  [!] core/performance: cleanup invalidated_code function

20100817
  [!] core: fix keyword token recognition problems
  [!] core: skip inspection on require loops
  [+] core: infer function return values (temporarily disabled)
  [+] core: detect dead-code (temporarily disabled)
  [*] core: internal refactoring (ast.valueknown)

20100816
  core: make reporting optional
  metalua: patches to metalua lineinfo
    (was corrupting HTML output and SciTE highlighting)

20100814
  core: add basic type inferences (e.g. number+number -> number)

20100813
  core: inspect required modules too
          (e.g. enables use of imported function signatures)
  core/SciTE: add list all warnings command (SciTE: Ctrl+Alt+W lists, and F4 iterates them)

20100811
  SciTE: autocomplete functions arguments when cursor after '('
  core: fix signatures for os/debug libraries
  core/SciTE: display function argument list or helpinfo for variables
  SciTE: Ctrl+Alt+I changed to Ctrl+Alt+B to avoid conflict with
            SciTE 2.20 incremental search

20100810
  SciTE: improved "inspect variable" command, supports browsing nested tables.
  SciTE: split luainspect.autocomplete property into two properties
  SciTE: add autocomplete function
  SciTE: autocomplete table fields.

20100809
  core/SciTE: add function argument count check
  core/SciTE: jump to definition now supports functions in different files.
  core/SciTE/HTML: improvements to displaying masking/masked lexicals.
  core/SciTE: add command to just to previous statement
  core/SciTE: preliminary variable autocomplete support
                   (luainspect.autocomplete currently disabled by default)
  SciTE: add missing style.script_lua.local_param_mutate style.

20100807
  SciTE: Add luainspect.path.append/luainspect.cpath.append properties
            to append to package.path/cpath
  SciTE: Add custom searcher function to locate modules in same path as current buffer.
  SciTE: Added "force reinspect" command to force full reinspection of code.
            Note: this will also attempt to unload any modules loaded by previous inspection.
  SciTE: Improve luainspect.update.delay to delay inspection for given tick count
           following user typing.  Also displays blue '+' marker when inspection has been delayed.

20100806
  SciTE: jump to uses, not jumps to exact position, not just line number
  SciTE: mark lines of invalidated code upon introducing code errors and display
            error message below invalidated code (not on exact line of error)
  SciTE: add styling delay option to improve performance (luainspect.update.delay)
  SciTE: preliminary auto-complete typing support (luainspect.autocomplete)
            (experimental and currently off by default)

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
