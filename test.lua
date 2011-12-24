#!/usr/bin/env lua

-- test writing examples.lua to examples.html
arg = {'-fhtml', '-lhtmllib', '-oexamples.html', 'examples.lua'}
dofile 'luainspectlib/luainspect/command.lua'
print 'output written to examples.html'

