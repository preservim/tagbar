" ============================================================================
" File:        tagbar.vim
" Description: List the current file's tags in a sidebar, ordered by class etc
" Author:      Jan Larres <jan@majutsushi.net>
" Licence:     Vim licence
" Website:     http://majutsushi.github.com/tagbar/
" Version:     1.5
" Note:        This plugin was heavily inspired by the 'Taglist' plugin by
"              Yegappan Lakshmanan and uses a small amount of code from it.
"
" Original taglist copyright notice:
"              Permission is hereby granted to use and distribute this code,
"              with or without modifications, provided that this copyright
"              notice is copied with it. Like anything else that's free,
"              taglist.vim is provided *as is* and comes with no warranty of
"              any kind, either expressed or implied. In no event will the
"              copyright holder be liable for any damamges resulting from the
"              use of this software.
" ============================================================================

if &cp || exists('g:loaded_tagbar')
    finish
endif

" Initialization {{{1
if !exists('g:tagbar_ctags_bin')
    if executable('ctags-exuberant')
        let g:tagbar_ctags_bin = 'ctags-exuberant'
    elseif executable('exctags')
        let g:tagbar_ctags_bin = 'exctags'
    elseif executable('ctags')
        let g:tagbar_ctags_bin = 'ctags'
    elseif executable('ctags.exe')
        let g:tagbar_ctags_bin = 'ctags.exe'
    elseif executable('tags')
        let g:tagbar_ctags_bin = 'tags'
    else
        echomsg 'Tagbar: Exuberant ctags not found, skipping plugin'
        finish
    endif
else
    let g:tagbar_ctags_bin = expand(g:tagbar_ctags_bin)
    if !executable(g:tagbar_ctags_bin)
        echomsg 'Tagbar: Exuberant ctags not found in specified place,'
              \ 'skipping plugin'
        finish
    endif
endif

let g:loaded_tagbar = 1

if !exists('g:tagbar_left')
    let g:tagbar_left = 0
endif

if !exists('g:tagbar_width')
    let g:tagbar_width = 40
endif

if !exists('g:tagbar_autoclose')
    let g:tagbar_autoclose = 0
endif

if !exists('g:tagbar_autofocus')
    let g:tagbar_autofocus = 0
endif

if !exists('g:tagbar_sort')
    let g:tagbar_sort = 1
endif

if !exists('g:tagbar_compact')
    let g:tagbar_compact = 0
endif

if !exists('g:tagbar_expand')
    let g:tagbar_expand = 0
endif

if !exists('g:tagbar_foldlevel')
    let g:tagbar_foldlevel = 99
endif

if !exists('g:tagbar_usearrows')
    let g:tagbar_usearrows = 0
endif


if has('multi_byte') && has('unix') && &encoding == 'utf-8' &&
 \ (empty(&termencoding) || &termencoding == 'utf-8')
    let s:icon_closed = '▶'
    let s:icon_open   = '▼'
elseif has('multi_byte') && (has('win32') || has('win64')) && g:tagbar_usearrows
    let s:icon_closed = '▷'
    let s:icon_open   = '◢'
else
    let s:icon_closed = '+'
    let s:icon_open   = '-'
endif

let s:type_init_done    = 0
let s:autocommands_done = 0
let s:window_expanded   = 0

" s:InitTypes() {{{2
function! s:InitTypes()
    " Dictionary of the already processed files, indexed by file name with
    " complete path.
    " The entries are again dictionaries with the following fields:
    " - fpath:     The complete file path
    " - mtime:     File modification time
    " - ftype:     The vim file type
    " - tags:      List of the tags that are present in the file, sorted
    "              according to the value of 'g:tagbar_sort'
    " - fline:     Dictionary of the tags, indexed by line number in the file
    " - tline:     Dictionary of the tags, indexed by line number in the tagbar
    " - kindfolds: Dictionary of the folding state of 'kind's, indexed by short
    "              name
    " - tagfolds:  Dictionary of dictionaries of the folding state of
    "              individual tags, indexed by kind and full path
    " - foldlevel: The current foldlevel of the file
    let s:known_files = {}

    let s:known_types = {}

    " Ant {{{3
    let type_ant = {}
    let type_ant.ctagstype = 'ant'
    let type_ant.kinds     = [
        \ {'short' : 'p', 'long' : 'projects', 'fold' : 0},
        \ {'short' : 't', 'long' : 'targets',  'fold' : 0}
    \ ]
    let s:known_types.ant = type_ant
    " Asm {{{3
    let type_asm = {}
    let type_asm.ctagstype = 'asm'
    let type_asm.kinds     = [
        \ {'short' : 'm', 'long' : 'macros',  'fold' : 0},
        \ {'short' : 't', 'long' : 'types',   'fold' : 0},
        \ {'short' : 'd', 'long' : 'defines', 'fold' : 0},
        \ {'short' : 'l', 'long' : 'labels',  'fold' : 0}
    \ ]
    let s:known_types.asm = type_asm
    " ASP {{{3
    let type_aspvbs = {}
    let type_aspvbs.ctagstype = 'asp'
    let type_aspvbs.kinds     = [
        \ {'short' : 'd', 'long' : 'constants',   'fold' : 0},
        \ {'short' : 'c', 'long' : 'classes',     'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',   'fold' : 0},
        \ {'short' : 's', 'long' : 'subroutines', 'fold' : 0},
        \ {'short' : 'v', 'long' : 'variables',   'fold' : 0}
    \ ]
    let s:known_types.aspvbs = type_aspvbs
    " Awk {{{3
    let type_awk = {}
    let type_awk.ctagstype = 'awk'
    let type_awk.kinds     = [
        \ {'short' : 'f', 'long' : 'functions', 'fold' : 0}
    \ ]
    let s:known_types.awk = type_awk
    " Basic {{{3
    let type_basic = {}
    let type_basic.ctagstype = 'basic'
    let type_basic.kinds     = [
        \ {'short' : 'c', 'long' : 'constants',    'fold' : 0},
        \ {'short' : 'g', 'long' : 'enumerations', 'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',    'fold' : 0},
        \ {'short' : 'l', 'long' : 'labels',       'fold' : 0},
        \ {'short' : 't', 'long' : 'types',        'fold' : 0},
        \ {'short' : 'v', 'long' : 'variables',    'fold' : 0}
    \ ]
    let s:known_types.basic = type_basic
    " BETA {{{3
    let type_beta = {}
    let type_beta.ctagstype = 'beta'
    let type_beta.kinds     = [
        \ {'short' : 'f', 'long' : 'fragments', 'fold' : 0},
        \ {'short' : 's', 'long' : 'slots',     'fold' : 0},
        \ {'short' : 'v', 'long' : 'patterns',  'fold' : 0}
    \ ]
    let s:known_types.beta = type_beta
    " C {{{3
    let type_c = {}
    let type_c.ctagstype = 'c'
    let type_c.kinds     = [
        \ {'short' : 'd', 'long' : 'macros',      'fold' : 1},
        \ {'short' : 'p', 'long' : 'prototypes',  'fold' : 1},
        \ {'short' : 'g', 'long' : 'enums',       'fold' : 0},
        \ {'short' : 'e', 'long' : 'enumerators', 'fold' : 0},
        \ {'short' : 't', 'long' : 'typedefs',    'fold' : 0},
        \ {'short' : 's', 'long' : 'structs',     'fold' : 0},
        \ {'short' : 'u', 'long' : 'unions',      'fold' : 0},
        \ {'short' : 'm', 'long' : 'members',     'fold' : 0},
        \ {'short' : 'v', 'long' : 'variables',   'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',   'fold' : 0}
    \ ]
    let type_c.sro        = '::'
    let type_c.kind2scope = {
        \ 'g' : 'enum',
        \ 's' : 'struct',
        \ 'u' : 'union'
    \ }
    let type_c.scope2kind = {
        \ 'enum'   : 'g',
        \ 'struct' : 's',
        \ 'union'  : 'u'
    \ }
    let s:known_types.c = type_c
    " C++ {{{3
    let type_cpp = {}
    let type_cpp.ctagstype = 'c++'
    let type_cpp.kinds     = [
        \ {'short' : 'd', 'long' : 'macros',      'fold' : 1},
        \ {'short' : 'p', 'long' : 'prototypes',  'fold' : 1},
        \ {'short' : 'g', 'long' : 'enums',       'fold' : 0},
        \ {'short' : 'e', 'long' : 'enumerators', 'fold' : 0},
        \ {'short' : 't', 'long' : 'typedefs',    'fold' : 0},
        \ {'short' : 'n', 'long' : 'namespaces',  'fold' : 0},
        \ {'short' : 'c', 'long' : 'classes',     'fold' : 0},
        \ {'short' : 's', 'long' : 'structs',     'fold' : 0},
        \ {'short' : 'u', 'long' : 'unions',      'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',   'fold' : 0},
        \ {'short' : 'm', 'long' : 'members',     'fold' : 0},
        \ {'short' : 'v', 'long' : 'variables',   'fold' : 0}
    \ ]
    let type_cpp.sro        = '::'
    let type_cpp.kind2scope = {
        \ 'g' : 'enum',
        \ 'n' : 'namespace',
        \ 'c' : 'class',
        \ 's' : 'struct',
        \ 'u' : 'union'
    \ }
    let type_cpp.scope2kind = {
        \ 'enum'      : 'g',
        \ 'namespace' : 'n',
        \ 'class'     : 'c',
        \ 'struct'    : 's',
        \ 'union'     : 'u'
    \ }
    let s:known_types.cpp = type_cpp
    " C# {{{3
    let type_cs = {}
    let type_cs.ctagstype = 'c#'
    let type_cs.kinds     = [
        \ {'short' : 'd', 'long' : 'macros',      'fold' : 1},
        \ {'short' : 'f', 'long' : 'fields',      'fold' : 0},
        \ {'short' : 'g', 'long' : 'enums',       'fold' : 0},
        \ {'short' : 'e', 'long' : 'enumerators', 'fold' : 0},
        \ {'short' : 't', 'long' : 'typedefs',    'fold' : 0},
        \ {'short' : 'n', 'long' : 'namespaces',  'fold' : 0},
        \ {'short' : 'i', 'long' : 'interfaces',  'fold' : 0},
        \ {'short' : 'c', 'long' : 'classes',     'fold' : 0},
        \ {'short' : 's', 'long' : 'structs',     'fold' : 0},
        \ {'short' : 'E', 'long' : 'events',      'fold' : 0},
        \ {'short' : 'm', 'long' : 'methods',     'fold' : 0},
        \ {'short' : 'p', 'long' : 'properties',  'fold' : 0}
    \ ]
    let type_cs.sro        = '.'
    let type_cs.kind2scope = {
        \ 'n' : 'namespace',
        \ 'i' : 'interface',
        \ 'c' : 'class',
        \ 's' : 'struct',
        \ 'g' : 'enum'
    \ }
    let type_cs.scope2kind = {
        \ 'namespace' : 'n',
        \ 'interface' : 'i',
        \ 'class'     : 'c',
        \ 'struct'    : 's',
        \ 'enum'      : 'g'
    \ }
    let s:known_types.cs = type_cs
    " COBOL {{{3
    let type_cobol = {}
    let type_cobol.ctagstype = 'cobol'
    let type_cobol.kinds     = [
        \ {'short' : 'd', 'long' : 'data items',        'fold' : 0},
        \ {'short' : 'f', 'long' : 'file descriptions', 'fold' : 0},
        \ {'short' : 'g', 'long' : 'group items',       'fold' : 0},
        \ {'short' : 'p', 'long' : 'paragraphs',        'fold' : 0},
        \ {'short' : 'P', 'long' : 'program ids',       'fold' : 0},
        \ {'short' : 's', 'long' : 'sections',          'fold' : 0}
    \ ]
    let s:known_types.cobol = type_cobol
    " DOS Batch {{{3
    let type_dosbatch = {}
    let type_dosbatch.ctagstype = 'dosbatch'
    let type_dosbatch.kinds     = [
        \ {'short' : 'l', 'long' : 'labels',    'fold' : 0},
        \ {'short' : 'v', 'long' : 'variables', 'fold' : 0}
    \ ]
    let s:known_types.dosbatch = type_dosbatch
    " Eiffel {{{3
    let type_eiffel = {}
    let type_eiffel.ctagstype = 'eiffel'
    let type_eiffel.kinds     = [
        \ {'short' : 'c', 'long' : 'classes',  'fold' : 0},
        \ {'short' : 'f', 'long' : 'features', 'fold' : 0}
    \ ]
    let type_eiffel.sro        = '.' " Not sure, is nesting even possible?
    let type_eiffel.kind2scope = {
        \ 'c' : 'class',
        \ 'f' : 'feature'
    \ }
    let type_eiffel.scope2kind = {
        \ 'class'   : 'c',
        \ 'feature' : 'f'
    \ }
    let s:known_types.eiffel = type_eiffel
    " Erlang {{{3
    let type_erlang = {}
    let type_erlang.ctagstype = 'erlang'
    let type_erlang.kinds     = [
        \ {'short' : 'm', 'long' : 'modules',            'fold' : 0},
        \ {'short' : 'd', 'long' : 'macro definitions',  'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',          'fold' : 0},
        \ {'short' : 'r', 'long' : 'record definitions', 'fold' : 0}
    \ ]
    let type_erlang.sro        = '.' " Not sure, is nesting even possible?
    let type_erlang.kind2scope = {
        \ 'm' : 'module'
    \ }
    let type_erlang.scope2kind = {
        \ 'module' : 'm'
    \ }
    let s:known_types.erlang = type_erlang
    " Flex {{{3
    " Vim doesn't support Flex out of the box, this is based on rough
    " guesses and probably requires
    " http://www.vim.org/scripts/script.php?script_id=2909
    " Improvements welcome!
    let type_mxml = {}
    let type_mxml.ctagstype = 'flex'
    let type_mxml.kinds     = [
        \ {'short' : 'v', 'long' : 'global variables', 'fold' : 0},
        \ {'short' : 'c', 'long' : 'classes',          'fold' : 0},
        \ {'short' : 'm', 'long' : 'methods',          'fold' : 0},
        \ {'short' : 'p', 'long' : 'properties',       'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',        'fold' : 0},
        \ {'short' : 'x', 'long' : 'mxtags',           'fold' : 0}
    \ ]
    let type_mxml.sro        = '.'
    let type_mxml.kind2scope = {
        \ 'c' : 'class'
    \ }
    let type_mxml.scope2kind = {
        \ 'class' : 'c'
    \ }
    let s:known_types.mxml = type_mxml
    " Fortran {{{3
    let type_fortran = {}
    let type_fortran.ctagstype = 'fortran'
    let type_fortran.kinds     = [
        \ {'short' : 'm', 'long' : 'modules',                      'fold' : 0},
        \ {'short' : 'p', 'long' : 'programs',                     'fold' : 0},
        \ {'short' : 'k', 'long' : 'components',                   'fold' : 0},
        \ {'short' : 't', 'long' : 'derived types and structures', 'fold' : 0},
        \ {'short' : 'c', 'long' : 'common blocks',                'fold' : 0},
        \ {'short' : 'b', 'long' : 'block data',                   'fold' : 0},
        \ {'short' : 'e', 'long' : 'entry points',                 'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',                    'fold' : 0},
        \ {'short' : 's', 'long' : 'subroutines',                  'fold' : 0},
        \ {'short' : 'l', 'long' : 'labels',                       'fold' : 0},
        \ {'short' : 'n', 'long' : 'namelists',                    'fold' : 0},
        \ {'short' : 'v', 'long' : 'variables',                    'fold' : 0}
    \ ]
    let type_fortran.sro        = '.' " Not sure, is nesting even possible?
    let type_fortran.kind2scope = {
        \ 'm' : 'module',
        \ 'p' : 'program',
        \ 'f' : 'function',
        \ 's' : 'subroutine'
    \ }
    let type_fortran.scope2kind = {
        \ 'module'     : 'm',
        \ 'program'    : 'p',
        \ 'function'   : 'f',
        \ 'subroutine' : 's'
    \ }
    let s:known_types.fortran = type_fortran
    " HTML {{{3
    let type_html = {}
    let type_html.ctagstype = 'html'
    let type_html.kinds     = [
        \ {'short' : 'f', 'long' : 'JavaScript funtions', 'fold' : 0},
        \ {'short' : 'a', 'long' : 'named anchors',       'fold' : 0}
    \ ]
    let s:known_types.html = type_html
    " Java {{{3
    let type_java = {}
    let type_java.ctagstype = 'java'
    let type_java.kinds     = [
        \ {'short' : 'p', 'long' : 'packages',       'fold' : 1},
        \ {'short' : 'f', 'long' : 'fields',         'fold' : 0},
        \ {'short' : 'g', 'long' : 'enum types',     'fold' : 0},
        \ {'short' : 'e', 'long' : 'enum constants', 'fold' : 0},
        \ {'short' : 'i', 'long' : 'interfaces',     'fold' : 0},
        \ {'short' : 'c', 'long' : 'classes',        'fold' : 0},
        \ {'short' : 'm', 'long' : 'methods',        'fold' : 0}
    \ ]
    let type_java.sro        = '.'
    let type_java.kind2scope = {
        \ 'g' : 'enum',
        \ 'i' : 'interface',
        \ 'c' : 'class'
    \ }
    let type_java.scope2kind = {
        \ 'enum'      : 'g',
        \ 'interface' : 'i',
        \ 'class'     : 'c'
    \ }
    let s:known_types.java = type_java
    " JavaScript {{{3
    " JavaScript is weird -- it does have scopes, but ctags doesn't seem to
    " properly generate the information for them, instead it simply uses the
    " complete name. So ctags has to be fixed before I can do anything here.
    let type_javascript = {}
    let type_javascript.ctagstype = 'javascript'
    let type_javascript.kinds     = [
        \ {'short' : 'v', 'long' : 'global variables', 'fold' : 0},
        \ {'short' : 'c', 'long' : 'classes',          'fold' : 0},
        \ {'short' : 'p', 'long' : 'properties',       'fold' : 0},
        \ {'short' : 'm', 'long' : 'methods',          'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',        'fold' : 0}
    \ ]
    let s:known_types.javascript = type_javascript
    " Lisp {{{3
    let type_lisp = {}
    let type_lisp.ctagstype = 'lisp'
    let type_lisp.kinds     = [
        \ {'short' : 'f', 'long' : 'functions', 'fold' : 0}
    \ ]
    let s:known_types.lisp = type_lisp
    " Lua {{{3
    let type_lua = {}
    let type_lua.ctagstype = 'lua'
    let type_lua.kinds     = [
        \ {'short' : 'f', 'long' : 'functions', 'fold' : 0}
    \ ]
    let s:known_types.lua = type_lua
    " Make {{{3
    let type_make = {}
    let type_make.ctagstype = 'make'
    let type_make.kinds     = [
        \ {'short' : 'm', 'long' : 'macros', 'fold' : 0}
    \ ]
    let s:known_types.make = type_make
    " Matlab {{{3
    let type_matlab = {}
    let type_matlab.ctagstype = 'matlab'
    let type_matlab.kinds     = [
        \ {'short' : 'f', 'long' : 'functions', 'fold' : 0}
    \ ]
    let s:known_types.matlab = type_matlab
    " Ocaml {{{3
    let type_ocaml = {}
    let type_ocaml.ctagstype = 'ocaml'
    let type_ocaml.kinds     = [
        \ {'short' : 'M', 'long' : 'modules or functors', 'fold' : 0},
        \ {'short' : 'v', 'long' : 'global variables',    'fold' : 0},
        \ {'short' : 'c', 'long' : 'classes',             'fold' : 0},
        \ {'short' : 'C', 'long' : 'constructors',        'fold' : 0},
        \ {'short' : 'm', 'long' : 'methods',             'fold' : 0},
        \ {'short' : 'e', 'long' : 'exceptions',          'fold' : 0},
        \ {'short' : 't', 'long' : 'type names',          'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',           'fold' : 0},
        \ {'short' : 'r', 'long' : 'structure fields',    'fold' : 0}
    \ ]
    let type_ocaml.sro        = '.' " Not sure, is nesting even possible?
    let type_ocaml.kind2scope = {
        \ 'M' : 'Module',
        \ 'c' : 'class',
        \ 't' : 'type'
    \ }
    let type_ocaml.scope2kind = {
        \ 'Module' : 'M',
        \ 'class'  : 'c',
        \ 'type'   : 't'
    \ }
    let s:known_types.ocaml = type_ocaml
    " Pascal {{{3
    let type_pascal = {}
    let type_pascal.ctagstype = 'pascal'
    let type_pascal.kinds     = [
        \ {'short' : 'f', 'long' : 'functions',  'fold' : 0},
        \ {'short' : 'p', 'long' : 'procedures', 'fold' : 0}
    \ ]
    let s:known_types.pascal = type_pascal
    " Perl {{{3
    let type_perl = {}
    let type_perl.ctagstype = 'perl'
    let type_perl.kinds     = [
        \ {'short' : 'p', 'long' : 'packages',    'fold' : 1},
        \ {'short' : 'c', 'long' : 'constants',   'fold' : 0},
        \ {'short' : 'f', 'long' : 'formats',     'fold' : 0},
        \ {'short' : 'l', 'long' : 'labels',      'fold' : 0},
        \ {'short' : 's', 'long' : 'subroutines', 'fold' : 0}
    \ ]
    let s:known_types.perl = type_perl
    " PHP {{{3
    let type_php = {}
    let type_php.ctagstype = 'php'
    let type_php.kinds     = [
        \ {'short' : 'i', 'long' : 'interfaces',           'fold' : 0},
        \ {'short' : 'c', 'long' : 'classes',              'fold' : 0},
        \ {'short' : 'd', 'long' : 'constant definitions', 'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',            'fold' : 0},
        \ {'short' : 'v', 'long' : 'variables',            'fold' : 0},
        \ {'short' : 'j', 'long' : 'javascript functions', 'fold' : 0}
    \ ]
    let s:known_types.php = type_php
    " Python {{{3
    let type_python = {}
    let type_python.ctagstype = 'python'
    let type_python.kinds     = [
        \ {'short' : 'i', 'long' : 'imports',   'fold' : 1},
        \ {'short' : 'c', 'long' : 'classes',   'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions', 'fold' : 0},
        \ {'short' : 'm', 'long' : 'members',   'fold' : 0},
        \ {'short' : 'v', 'long' : 'variables', 'fold' : 0}
    \ ]
    let type_python.sro        = '.'
    let type_python.kind2scope = {
        \ 'c' : 'class',
        \ 'f' : 'function',
        \ 'm' : 'function'
    \ }
    let type_python.scope2kind = {
        \ 'class'    : 'c',
        \ 'function' : 'f'
    \ }
    let s:known_types.python = type_python
    " REXX {{{3
    let type_rexx = {}
    let type_rexx.ctagstype = 'rexx'
    let type_rexx.kinds     = [
        \ {'short' : 's', 'long' : 'subroutines', 'fold' : 0}
    \ ]
    let s:known_types.rexx = type_rexx
    " Ruby {{{3
    let type_ruby = {}
    let type_ruby.ctagstype = 'ruby'
    let type_ruby.kinds     = [
        \ {'short' : 'm', 'long' : 'modules',           'fold' : 0},
        \ {'short' : 'c', 'long' : 'classes',           'fold' : 0},
        \ {'short' : 'f', 'long' : 'methods',           'fold' : 0},
        \ {'short' : 'F', 'long' : 'singleton methods', 'fold' : 0}
    \ ]
    let type_ruby.sro        = '.'
    let type_ruby.kind2scope = {
        \ 'c' : 'class',
        \ 'm' : 'class'
    \ }
    let type_ruby.scope2kind = {
        \ 'class' : 'c'
    \ }
    let s:known_types.ruby = type_ruby
    " Scheme {{{3
    let type_scheme = {}
    let type_scheme.ctagstype = 'scheme'
    let type_scheme.kinds     = [
        \ {'short' : 'f', 'long' : 'functions', 'fold' : 0},
        \ {'short' : 's', 'long' : 'sets',      'fold' : 0}
    \ ]
    let s:known_types.scheme = type_scheme
    " Shell script {{{3
    let type_sh = {}
    let type_sh.ctagstype = 'sh'
    let type_sh.kinds     = [
        \ {'short' : 'f', 'long' : 'functions', 'fold' : 0}
    \ ]
    let s:known_types.sh = type_sh
    let s:known_types.csh = type_sh
    let s:known_types.zsh = type_sh
    " SLang {{{3
    let type_slang = {}
    let type_slang.ctagstype = 'slang'
    let type_slang.kinds     = [
        \ {'short' : 'n', 'long' : 'namespaces', 'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',  'fold' : 0}
    \ ]
    let s:known_types.slang = type_slang
    " SML {{{3
    let type_sml = {}
    let type_sml.ctagstype = 'sml'
    let type_sml.kinds     = [
        \ {'short' : 'e', 'long' : 'exception declarations', 'fold' : 0},
        \ {'short' : 'f', 'long' : 'function definitions',   'fold' : 0},
        \ {'short' : 'c', 'long' : 'functor definitions',    'fold' : 0},
        \ {'short' : 's', 'long' : 'signature declarations', 'fold' : 0},
        \ {'short' : 'r', 'long' : 'structure declarations', 'fold' : 0},
        \ {'short' : 't', 'long' : 'type definitions',       'fold' : 0},
        \ {'short' : 'v', 'long' : 'value bindings',         'fold' : 0}
    \ ]
    let s:known_types.sml = type_sml
    " SQL {{{3
    " The SQL ctags parser seems to be buggy for me, so this just uses the
    " normal kinds even though scopes should be available. Improvements
    " welcome!
    let type_sql = {}
    let type_sql.ctagstype = 'sql'
    let type_sql.kinds     = [
        \ {'short' : 'P', 'long' : 'packages',               'fold' : 1},
        \ {'short' : 'c', 'long' : 'cursors',                'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',              'fold' : 0},
        \ {'short' : 'F', 'long' : 'record fields',          'fold' : 0},
        \ {'short' : 'L', 'long' : 'block label',            'fold' : 0},
        \ {'short' : 'p', 'long' : 'procedures',             'fold' : 0},
        \ {'short' : 's', 'long' : 'subtypes',               'fold' : 0},
        \ {'short' : 't', 'long' : 'tables',                 'fold' : 0},
        \ {'short' : 'T', 'long' : 'triggers',               'fold' : 0},
        \ {'short' : 'v', 'long' : 'variables',              'fold' : 0},
        \ {'short' : 'i', 'long' : 'indexes',                'fold' : 0},
        \ {'short' : 'e', 'long' : 'events',                 'fold' : 0},
        \ {'short' : 'U', 'long' : 'publications',           'fold' : 0},
        \ {'short' : 'R', 'long' : 'services',               'fold' : 0},
        \ {'short' : 'D', 'long' : 'domains',                'fold' : 0},
        \ {'short' : 'V', 'long' : 'views',                  'fold' : 0},
        \ {'short' : 'n', 'long' : 'synonyms',               'fold' : 0},
        \ {'short' : 'x', 'long' : 'MobiLink Table Scripts', 'fold' : 0},
        \ {'short' : 'y', 'long' : 'MobiLink Conn Scripts',  'fold' : 0}
    \ ]
    let s:known_types.sql = type_sql
    " Tcl {{{3
    let type_tcl = {}
    let type_tcl.ctagstype = 'tcl'
    let type_tcl.kinds     = [
        \ {'short' : 'c', 'long' : 'classes',    'fold' : 0},
        \ {'short' : 'm', 'long' : 'methods',    'fold' : 0},
        \ {'short' : 'p', 'long' : 'procedures', 'fold' : 0}
    \ ]
    let s:known_types.tcl = type_tcl
    " LaTeX {{{3
    let type_tex = {}
    let type_tex.ctagstype = 'tex'
    let type_tex.kinds     = [
        \ {'short' : 'p', 'long' : 'parts',          'fold' : 0},
        \ {'short' : 'c', 'long' : 'chapters',       'fold' : 0},
        \ {'short' : 's', 'long' : 'sections',       'fold' : 0},
        \ {'short' : 'u', 'long' : 'subsections',    'fold' : 0},
        \ {'short' : 'b', 'long' : 'subsubsections', 'fold' : 0},
        \ {'short' : 'P', 'long' : 'paragraphs',     'fold' : 0},
        \ {'short' : 'G', 'long' : 'subparagraphs',  'fold' : 0}
    \ ]
    let s:known_types.tex = type_tex
    " Vera {{{3
    " Why are variables 'virtual'?
    let type_vera = {}
    let type_vera.ctagstype = 'vera'
    let type_vera.kinds     = [
        \ {'short' : 'd', 'long' : 'macros',      'fold' : 1},
        \ {'short' : 'g', 'long' : 'enums',       'fold' : 0},
        \ {'short' : 'T', 'long' : 'typedefs',    'fold' : 0},
        \ {'short' : 'c', 'long' : 'classes',     'fold' : 0},
        \ {'short' : 'e', 'long' : 'enumerators', 'fold' : 0},
        \ {'short' : 'm', 'long' : 'members',     'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',   'fold' : 0},
        \ {'short' : 't', 'long' : 'tasks',       'fold' : 0},
        \ {'short' : 'v', 'long' : 'variables',   'fold' : 0},
        \ {'short' : 'p', 'long' : 'programs',    'fold' : 0}
    \ ]
    let type_vera.sro        = '.' " Nesting doesn't seem to be possible
    let type_vera.kind2scope = {
        \ 'g' : 'enum',
        \ 'c' : 'class',
        \ 'v' : 'virtual'
    \ }
    let type_vera.scope2kind = {
        \ 'enum'      : 'g',
        \ 'class'     : 'c',
        \ 'virtual'   : 'v'
    \ }
    let s:known_types.vera = type_vera
    " Verilog {{{3
    let type_verilog = {}
    let type_verilog.ctagstype = 'verilog'
    let type_verilog.kinds     = [
        \ {'short' : 'c', 'long' : 'constants',           'fold' : 0},
        \ {'short' : 'e', 'long' : 'events',              'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',           'fold' : 0},
        \ {'short' : 'm', 'long' : 'modules',             'fold' : 0},
        \ {'short' : 'n', 'long' : 'net data types',      'fold' : 0},
        \ {'short' : 'p', 'long' : 'ports',               'fold' : 0},
        \ {'short' : 'r', 'long' : 'register data types', 'fold' : 0},
        \ {'short' : 't', 'long' : 'tasks',               'fold' : 0}
    \ ]
    let s:known_types.verilog = type_verilog
    " VHDL {{{3
    " The VHDL ctags parser unfortunately doesn't generate proper scopes
    let type_vhdl = {}
    let type_vhdl.ctagstype = 'vhdl'
    let type_vhdl.kinds     = [
        \ {'short' : 'P', 'long' : 'packages',   'fold' : 1},
        \ {'short' : 'c', 'long' : 'constants',  'fold' : 0},
        \ {'short' : 't', 'long' : 'types',      'fold' : 0},
        \ {'short' : 'T', 'long' : 'subtypes',   'fold' : 0},
        \ {'short' : 'r', 'long' : 'records',    'fold' : 0},
        \ {'short' : 'e', 'long' : 'entities',   'fold' : 0},
        \ {'short' : 'f', 'long' : 'functions',  'fold' : 0},
        \ {'short' : 'p', 'long' : 'procedures', 'fold' : 0}
    \ ]
    let s:known_types.vhdl = type_vhdl
    " Vim {{{3
    let type_vim = {}
    let type_vim.ctagstype = 'vim'
    let type_vim.kinds     = [
        \ {'short' : 'v', 'long' : 'variables',          'fold' : 1},
        \ {'short' : 'f', 'long' : 'functions',          'fold' : 0},
        \ {'short' : 'a', 'long' : 'autocommand groups', 'fold' : 1},
        \ {'short' : 'c', 'long' : 'commands',           'fold' : 0},
        \ {'short' : 'm', 'long' : 'maps',               'fold' : 1}
    \ ]
    let s:known_types.vim = type_vim
    " YACC {{{3
    let type_yacc = {}
    let type_yacc.ctagstype = 'yacc'
    let type_yacc.kinds     = [
        \ {'short' : 'l', 'long' : 'labels', 'fold' : 0}
    \ ]
    let s:known_types.yacc = type_yacc
    " }}}3

    let user_defs = s:GetUserTypeDefs()
    for [key, value] in items(user_defs)
        if !has_key(s:known_types, key) ||
         \ (has_key(value, 'replace') && value.replace)
            let s:known_types[key] = value
        else
            call extend(s:known_types[key], value)
        endif
    endfor

    " Create a dictionary of the kind order for fast
    " access in sorting functions
    for type in values(s:known_types)
        let i = 0
        let type.kinddict = {}
        for kind in type.kinds
            let type.kinddict[kind.short] = i
            let i += 1
        endfor
    endfor

    let s:access_symbols = {
        \ 'public'    : '+',
        \ 'protected' : '#',
        \ 'private'   : '-'
    \ }

    let s:type_init_done = 1
endfunction

" s:GetUserTypeDefs() {{{2
function! s:GetUserTypeDefs()
    redir => defs
    silent! execute 'let g:'
    redir END

    let deflist = split(defs, '\n')
    call map(deflist, 'substitute(v:val, ''^\S\+\zs.*'', "", "")')
    call filter(deflist, 'v:val =~ "^tagbar_type_"')

    let defdict = {}
    for defstr in deflist
        let type = substitute(defstr, '^tagbar_type_', '', '')
        execute 'let defdict["' . type . '"] = g:' . defstr
    endfor

    " If the user only specified one of kind2scope and scope2kind use it to
    " generate the other one
    " Also, transform the 'kind' definitions into dictionary format
    for def in values(defdict)
        let kinds = def.kinds
        let def.kinds = []
        for kind in kinds
            let kindlist = split(kind, ':')
            let kinddict = {'short' : kindlist[0], 'long' : kindlist[1]}
            if len(kindlist) == 3
                let kinddict.fold = kindlist[2]
            else
                let kinddict.fold = 0
            endif
            call add(def.kinds, kinddict)
        endfor

        if has_key(def, 'kind2scope') && !has_key(def, 'scope2kind')
            let def.scope2kind = {}
            for [key, value] in items(def.kind2scope)
                let def.scope2kind[value] = key
            endfor
        elseif has_key(def, 'scope2kind') && !has_key(def, 'kind2scope')
            let def.kind2scope = {}
            for [key, value] in items(def.scope2kind)
                let def.kind2scope[value] = key
            endfor
        endif
    endfor

    return defdict
endfunction

" s:MapKeys() {{{2
function! s:MapKeys()
    nnoremap <script> <silent> <buffer> <CR>    :call <SID>JumpToTag()<CR>
    nnoremap <script> <silent> <buffer> <2-LeftMouse>
                                              \ :call <SID>JumpToTag()<CR>
    nnoremap <script> <silent> <buffer> <LeftRelease>
                \ <LeftRelease>:call <SID>CheckMouseClick()<CR>
    nnoremap <script> <silent> <buffer> <Space> :call <SID>ShowPrototype()<CR>

    nnoremap <script> <silent> <buffer> +        :call <SID>OpenFold()<CR>
    nnoremap <script> <silent> <buffer> <kPlus>  :call <SID>OpenFold()<CR>
    nnoremap <script> <silent> <buffer> o        :call <SID>ToggleFold()<CR>
    nnoremap <script> <silent> <buffer> -        :call <SID>CloseFold()<CR>
    nnoremap <script> <silent> <buffer> <kMinus> :call <SID>CloseFold()<CR>
    nnoremap <script> <silent> <buffer> x        :call <SID>CloseParent()<CR>

    nnoremap <script> <silent> <buffer> *     :call <SID>SetFoldLevel(99, 1)<CR>
    nnoremap <script> <silent> <buffer> <kMultiply>
                                            \ :call <SID>SetFoldLevel(99, 1)<CR>
    nnoremap <script> <silent> <buffer> =     :call <SID>SetFoldLevel(0,  1)<CR>

    nnoremap <script> <silent> <buffer> s    :call <SID>ToggleSort()<CR>
    nnoremap <script> <silent> <buffer> z    :call <SID>ZoomWindow()<CR>
    nnoremap <script> <silent> <buffer> q    :close<CR>
    nnoremap <script> <silent> <buffer> <F1> :call <SID>ToggleHelp()<CR>
endfunction

" s:CreateAutocommands() {{{2
function! s:CreateAutocommands()
    augroup TagbarAutoCmds
        autocmd!
        autocmd BufEnter   __Tagbar__ nested call s:QuitIfOnlyWindow()
        autocmd BufUnload  __Tagbar__ call s:CleanUp()
        autocmd CursorHold __Tagbar__ call s:ShowPrototype()

        autocmd BufEnter,CursorHold * call
                    \ s:AutoUpdate(fnamemodify(bufname('%'), ':p'))
    augroup END

    let s:autocommands_done = 1
endfunction

" Window management {{{1
" s:ToggleWindow() {{{2
function! s:ToggleWindow()
    let tagbarwinnr = bufwinnr("__Tagbar__")
    if tagbarwinnr != -1
        call s:CloseWindow()
        return
    endif

    call s:OpenWindow(0)
endfunction

" s:OpenWindow() {{{2
function! s:OpenWindow(autoclose)
    if !s:type_init_done
        call s:InitTypes()
    endif

    " If the tagbar window is already open jump to it
    let tagbarwinnr = bufwinnr('__Tagbar__')
    if tagbarwinnr != -1
        if winnr() != tagbarwinnr
            execute tagbarwinnr . 'wincmd w'
        endif
        return
    endif

    " Expand the Vim window to accomodate for the Tagbar window if requested
    if g:tagbar_expand && !s:window_expanded && has('gui_running')
        let &columns += g:tagbar_width + 1
        let s:window_expanded = 1
    endif

    let openpos = g:tagbar_left ? 'topleft vertical ' : 'botright vertical '
    exe 'silent! keepalt ' . openpos . g:tagbar_width . 'split ' . '__Tagbar__'

    setlocal noreadonly " in case the "view" mode is used
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal nobuflisted
    setlocal nomodifiable
    setlocal filetype=tagbar
    setlocal nolist
    setlocal nonumber
    setlocal nowrap
    setlocal winfixwidth
    setlocal textwidth=0

    if exists('+relativenumber')
        setlocal norelativenumber
    endif

    setlocal nofoldenable

    setlocal statusline=%!TagbarGenerateStatusline()

    " Variable for saving the current file for functions that are called from
    " the tagbar window
    let s:current_file = ''

    " Script-local variable needed since compare functions can't
    " take extra arguments
    let s:compare_typeinfo = {}

    let s:is_maximized = 0
    let s:short_help   = 1

    let w:autoclose = a:autoclose

    if has('balloon_eval')
        setlocal balloonexpr=TagbarBalloonExpr()
        set ballooneval
    endif

    let cpoptions_save = &cpoptions
    set cpoptions&vim

    if !hasmapto('JumpToTag', 'n')
        call s:MapKeys()
    endif

    if !s:autocommands_done
        call s:CreateAutocommands()
    endif

    let &cpoptions = cpoptions_save

    execute 'wincmd p'

    " Jump back to the tagbar window if autoclose or autofocus is set. Can't
    " just stay in it since it wouldn't trigger the update event
    if g:tagbar_autoclose || a:autoclose || g:tagbar_autofocus
        let tagbarwinnr = bufwinnr('__Tagbar__')
        execute tagbarwinnr . 'wincmd w'
    endif
endfunction

" s:CloseWindow() {{{2
function! s:CloseWindow()
    let tagbarwinnr = bufwinnr('__Tagbar__')
    if tagbarwinnr == -1
        return
    endif

    let tagbarbufnr = winbufnr(tagbarwinnr)

    if winnr() == tagbarwinnr
        if winbufnr(2) != -1
            " Other windows are open, only close the tagbar one
            close
        endif
    else
        " Go to the tagbar window, close it and then come back to the
        " original window
        let curbufnr = bufnr('%')
        execute tagbarwinnr . 'wincmd w'
        close
        " Need to jump back to the original window only if we are not
        " already in that window
        let winnum = bufwinnr(curbufnr)
        if winnr() != winnum
            exe winnum . 'wincmd w'
        endif
    endif

    " If the Vim window has been expanded, and Tagbar is not open in any other
    " tabpages, shrink the window again
    if s:window_expanded
        let tablist = []
        for i in range(tabpagenr('$'))
            call extend(tablist, tabpagebuflist(i + 1))
        endfor

        if index(tablist, tagbarbufnr) == -1
            let &columns -= g:tagbar_width + 1
            let s:window_expanded = 0
        endif
    endif
endfunction

" s:ZoomWindow() {{{2
function! s:ZoomWindow()
    if s:is_maximized
        execute 'vert resize ' . g:tagbar_width
        let s:is_maximized = 0
    else
        vert resize
        let s:is_maximized = 1
    endif
endfunction

" Tag processing {{{1
" s:ProcessFile() {{{2
function! s:ProcessFile(fname, ftype)
    if !s:IsValidFile(a:fname, a:ftype)
        return
    endif

    let typeinfo = s:known_types[a:ftype]

    let ctags_args  = ' -f - '
    let ctags_args .= ' --format=2 '
    let ctags_args .= ' --excmd=pattern '
    let ctags_args .= ' --fields=nksSaz '
    let ctags_args .= ' --extra= '
    let ctags_args .= ' --sort=yes '

    " Include extra type definitions
    if has_key(typeinfo, 'deffile')
        let ctags_args .= ' --options=' . typeinfo.deffile . ' '
    endif

    let ctags_type = typeinfo.ctagstype

    let ctags_kinds = ""
    for kind in typeinfo.kinds
        let ctags_kinds .= kind.short
    endfor

    let ctags_args .= ' --language-force=' . ctags_type .
                    \ ' --' . ctags_type . '-kinds=' . ctags_kinds . ' '

    if has('win32') || has('win64')
        let ctags_bin = fnamemodify(g:tagbar_ctags_bin, ':8')
    else
        let ctags_bin = shellescape(g:tagbar_ctags_bin)
    endif
    let ctags_cmd = ctags_bin . ctags_args . shellescape(a:fname)
    let ctags_output = system(ctags_cmd)

    if v:shell_error
        let msg = 'Tagbar: Could not generate tags for ' . a:fname
        echohl WarningMsg | echomsg msg | echohl None
        if !empty(ctags_output)
            echohl WarningMsg | echomsg ctags_output | echohl None
        endif
        return
    endif

    " If the file has only been updated preserve the fold states, otherwise
    " create a new entry
    if has_key(s:known_files, a:fname)
        let fileinfo = s:known_files[a:fname]
        let tagfolds_old = fileinfo.tagfolds
    else
        let fileinfo = {}
        let fileinfo.fpath = a:fname

        let fileinfo.kindfolds = {}
        for kind in typeinfo.kinds
            let fileinfo.kindfolds[kind.short] =
                        \ g:tagbar_foldlevel == 0 ? 1 : kind.fold
        endfor

        let fileinfo.foldlevel = g:tagbar_foldlevel
    endif
    let fileinfo.tagfolds = {}
    for kind in typeinfo.kinds
        let fileinfo.tagfolds[kind.short]  = {}
    endfor

    let fileinfo.mtime = getftime(a:fname)
    let fileinfo.ftype = a:ftype
    let fileinfo.tags  = []
    let fileinfo.fline = {}
    let fileinfo.tline = {}

    " Parse the ctags output lines
    let rawtaglist = split(ctags_output, '\n\+')
    for line in rawtaglist
        let parts = split(line, ';"')
        if len(parts) == 2 " Is a valid tag line
            let taginfo = s:ParseTagline(parts[0], parts[1], typeinfo, fileinfo)
            let fileinfo.fline[taginfo.fields.line] = taginfo
            call add(fileinfo.tags, taginfo)
        endif
    endfor

    " Process scoped tags
    let processedtags = []
    if has_key(typeinfo, 'kind2scope')
        let scopedtags = []
        let is_scoped = 'has_key(typeinfo.kind2scope, v:val.fields.kind) ||
                       \ has_key(v:val, "scope")'
        let scopedtags += filter(copy(fileinfo.tags), is_scoped)
        call filter(fileinfo.tags, '!(' . is_scoped . ')')

        call s:AddScopedTags(scopedtags, processedtags, {}, 0,
                           \ typeinfo, fileinfo)

        if !empty(scopedtags)
            echoerr 'Tagbar: ''scopedtags'' not empty after processing,'
                  \ 'this should never happen!'
                  \ 'Please contact the script maintainer with an example.'
        endif
    endif

    " Create a placeholder tag for the 'kind' header for folding purposes
    for kind in typeinfo.kinds
        let curtags = filter(copy(fileinfo.tags),
                           \ 'v:val.fields.kind ==# kind.short')

        if empty(curtags)
            continue
        endif

        let kindtag = {
            \ 'short'   : kind.short,
            \ 'name'    : kind.long,
            \ 'numtags' : len(curtags),
            \ 'tline'   : 0
        \ }

        for tag in curtags
            let tag.parent = kindtag
        endfor
    endfor

    if !empty(processedtags)
        call extend(fileinfo.tags, processedtags)
    endif

    " Copy over the saved tag folding states to avoid memory leaks in case of
    " deleted tags
    if has_key(s:known_files, a:fname)
        unlet kind
        for [kind, val] in items(tagfolds_old)
            for path in keys(val)
                if has_key(fileinfo.tagfolds[kind], path)
                    let fileinfo.tagfolds[kind][path] = tagfolds_old[kind][path]
                endif
            endfor
        endfor
        unlet tagfolds_old
    endif

    " Sort the tags
    let s:compare_typeinfo = typeinfo
    if has_key(typeinfo, 'sort')
        if typeinfo.sort
            call s:SortTags(fileinfo.tags, 's:CompareByKind')
        else
            call s:SortTags(fileinfo.tags, 's:CompareByLine')
        endif
    elseif g:tagbar_sort
        call s:SortTags(fileinfo.tags, 's:CompareByKind')
    else
        call s:SortTags(fileinfo.tags, 's:CompareByLine')
    endif

    let s:known_files[a:fname] = fileinfo
endfunction

" s:ParseTagline() {{{2
" Structure of a tag line:
" tagname<TAB>filename<TAB>expattern;"fields
" fields: <TAB>name:value
" fields that are always present: kind, line
function! s:ParseTagline(part1, part2, typeinfo, fileinfo)
    let taginfo = {}

    let basic_info      = split(a:part1, '\t')
    let taginfo.name    = basic_info[0]
    let taginfo.file    = basic_info[1]

    " the pattern can contain tabs and thus may have been split up, so join
    " the rest of the items together again
    let pattern = join(basic_info[2:], "\t")
    let start   = 2 " skip the slash and the ^
    let end     = strlen(pattern) - 1
    if pattern[end - 1] == '$'
        let end -= 1
        let dollar = '\$'
    else
        let dollar = ''
    endif
    let pattern           = strpart(pattern, start, end - start)
    let taginfo.pattern   = '\V\^' . pattern . dollar
    let prototype         = substitute(pattern,   '^[[:space:]]\+', '', '')
    let prototype         = substitute(prototype, '[[:space:]]\+$', '', '')
    let taginfo.prototype = prototype

    let taginfo.fields = {}
    let fields = split(a:part2, '\t')
    for field in fields
        " can't use split() since the value can contain ':'
        let delimit             = stridx(field, ':')
        let key                 = strpart(field, 0, delimit)
        let val                 = strpart(field, delimit + 1)
        let taginfo.fields[key] = val
    endfor

    " Make some information easier accessible
    let taginfo.path     = ''
    let taginfo.fullpath = taginfo.name
    let taginfo.depth    = 0
    if has_key(a:typeinfo, 'scope2kind')
        for scope in keys(a:typeinfo.scope2kind)
            if has_key(taginfo.fields, scope)
                let taginfo.scope = scope
                let taginfo.path  = taginfo.fields[scope]

                let taginfo.fullpath = taginfo.path . a:typeinfo.sro .
                                     \ taginfo.name
                break
            endif
        endfor
        let taginfo.depth = len(split(taginfo.path, '\V' . a:typeinfo.sro))
    endif

    " Needed for folding
    let taginfo.parent = {}
    if !has_key(s:known_files, a:fileinfo.fpath) &&
     \ taginfo.depth >= a:fileinfo.foldlevel
        let a:fileinfo.tagfolds[taginfo.fields.kind][taginfo.fullpath] = 1
    else
        let a:fileinfo.tagfolds[taginfo.fields.kind][taginfo.fullpath] =
                    \ a:fileinfo.kindfolds[taginfo.fields.kind]
    endif
    let taginfo.tline = -1

    return taginfo
endfunction

" s:AddScopedTags() {{{2
" Recursively process tags. Unfortunately there is a problem: not all tags in
" a hierarchy are actually there. For example, in C++ a class can be defined
" in a header file and implemented in a .cpp file (so the class itself doesn't
" appear in the .cpp file and thus doesn't generate a tag). Another example
" are anonymous structures like namespaces, structs, enums, and unions, that
" also don't get a tag themselves. These tags are thus called 'pseudo-tags' in
" Tagbar. Properly parsing them is quite tricky, so try not to think about it
" too much.
function! s:AddScopedTags(tags, processedtags, parent, depth,
                        \ typeinfo, fileinfo)
    if !empty(a:parent)
        let curpath = a:parent.fullpath
        let pscope  = a:typeinfo.kind2scope[a:parent.fields.kind]
    else
        let curpath = ''
        let pscope  = ''
    endif

    let is_cur_tag = 'v:val.depth == a:depth'

    if !empty(curpath)
        " Check whether the tag is either a direct child at the current depth
        " or at least a proper grandchild with pseudo-tags in between. If it
        " is a direct child also check for matching scope.
        let is_cur_tag .= ' &&
        \ (v:val.path == curpath ||
         \ match(v:val.path, ''\V\^\C'' . curpath . a:typeinfo.sro) == 0) &&
        \ (v:val.path == curpath ? (v:val.scope == pscope) : 1)'
    endif

    let curtags = filter(copy(a:tags), is_cur_tag)

    if !empty(curtags)
        call filter(a:tags, '!(' . is_cur_tag . ')')

        let realtags   = []
        let pseudotags = []

        while !empty(curtags)
            let tag = remove(curtags, 0)

            if tag.path != curpath
                " tag is child of a pseudo-tag, so create a new pseudo-tag and
                " add all its children to it
                let pseudotag = s:ProcessPseudoTag(curtags, tag, a:parent,
                                                 \ a:typeinfo, a:fileinfo)

                call add(pseudotags, pseudotag)
            else
                call add(realtags, tag)
            endif
        endwhile

        " Recursively add the children of the tags on the current level
        for tag in realtags
            let tag.parent = a:parent

            if !has_key(a:typeinfo.kind2scope, tag.fields.kind)
                continue
            endif

            if !has_key(tag, 'children')
                let tag.children = []
            endif

            call s:AddScopedTags(a:tags, tag.children, tag, a:depth + 1,
                               \ a:typeinfo, a:fileinfo)
        endfor
        call extend(a:processedtags, realtags)

        " Recursively add the children of the tags that are children of the
        " pseudo-tags on the current level
        for tag in pseudotags
            call s:ProcessPseudoChildren(a:tags, tag, a:depth, a:typeinfo,
                                       \ a:fileinfo)
        endfor
        call extend(a:processedtags, pseudotags)
    endif

    " Now we have to check if there are any pseudo-tags at the current level
    " so we have to check for real tags at a lower level, i.e. grandchildren
    let is_grandchild = 'v:val.depth > a:depth'

    if !empty(curpath)
        let is_grandchild .=
        \ ' && match(v:val.path, ''\V\^\C'' . curpath . a:typeinfo.sro) == 0'
    endif

    let grandchildren = filter(copy(a:tags), is_grandchild)

    if !empty(grandchildren)
        call s:AddScopedTags(a:tags, a:processedtags, a:parent, a:depth + 1,
                           \ a:typeinfo, a:fileinfo)
    endif
endfunction

" s:ProcessPseudoTag() {{{2
function! s:ProcessPseudoTag(curtags, tag, parent, typeinfo, fileinfo)
    let curpath = !empty(a:parent) ? a:parent.fullpath : ''

    let pseudoname = substitute(a:tag.path, curpath, '', '')
    let pseudoname = substitute(pseudoname, '\V\^' . a:typeinfo.sro, '', '')
    let pseudotag  = s:CreatePseudoTag(pseudoname, a:parent, a:tag.scope,
                                     \ a:typeinfo, a:fileinfo)
    let pseudotag.children = [a:tag]

    " get all the other (direct) children of the current pseudo-tag
    let ispseudochild = 'v:val.path == a:tag.path && v:val.scope == a:tag.scope'
    let pseudochildren = filter(copy(a:curtags), ispseudochild)
    if !empty(pseudochildren)
        call filter(a:curtags, '!(' . ispseudochild . ')')
        call extend(pseudotag.children, pseudochildren)
    endif

    return pseudotag
endfunction

" s:ProcessPseudoChildren() {{{2
function! s:ProcessPseudoChildren(tags, tag, depth, typeinfo, fileinfo)
    for childtag in a:tag.children
        let childtag.parent = a:tag

        if !has_key(a:typeinfo.kind2scope, childtag.fields.kind)
            continue
        endif

        if !has_key(childtag, 'children')
            let childtag.children = []
        endif

        call s:AddScopedTags(a:tags, childtag.children, childtag, a:depth + 1,
                           \ a:typeinfo, a:fileinfo)
    endfor

    let is_grandchild = 'v:val.depth > a:depth &&
                       \ match(v:val.path, ''^\C'' . a:tag.fullpath) == 0'
    let grandchildren = filter(copy(a:tags), is_grandchild)
    if !empty(grandchildren)
        call s:AddScopedTags(a:tags, a:tag.children, a:tag, a:depth + 1,
                           \ a:typeinfo, a:fileinfo)
    endif
endfunction

" s:CreatePseudoTag() {{{2
function! s:CreatePseudoTag(name, parent, scope, typeinfo, fileinfo)
    if !empty(a:parent)
        let curpath = a:parent.fullpath
        let pscope  = a:typeinfo.kind2scope[a:parent.fields.kind]
    else
        let curpath = ''
        let pscope  = ''
    endif

    let pseudotag             = {}
    let pseudotag.name        = a:name
    let pseudotag.fields      = {}
    let pseudotag.fields.kind = a:typeinfo.scope2kind[a:scope]
    let pseudotag.fields.line = 0

    let parentscope = substitute(curpath, a:name . '$', '', '')
    let parentscope = substitute(parentscope,
                               \ '\V\^' . a:typeinfo.sro . '\$', '', '')

    let pseudotag.path     = ''
    let pseudotag.fullpath = pseudotag.name
    if pscope != ''
        let pseudotag.fields[pscope] = parentscope
        let pseudotag.scope    = pscope
        let pseudotag.path     = parentscope
        let pseudotag.fullpath =
                    \ pseudotag.path . a:typeinfo.sro . pseudotag.name
    endif
    let pseudotag.depth = len(split(pseudotag.path, '\V' . a:typeinfo.sro))

    let pseudotag.parent = a:parent
    if !has_key(s:known_files, a:fileinfo.fpath) &&
     \ pseudotag.depth >= a:fileinfo.foldlevel
        let a:fileinfo.tagfolds[pseudotag.fields.kind][pseudotag.fullpath] = 1
    else
        let a:fileinfo.tagfolds[pseudotag.fields.kind][pseudotag.fullpath] =
                    \ a:fileinfo.kindfolds[pseudotag.fields.kind]
    endif
    let pseudotag.tline = -1

    return pseudotag
endfunction

" Sorting {{{1
" s:SortTags() {{{2
function! s:SortTags(tags, comparemethod)
    call sort(a:tags, a:comparemethod)

    for tag in a:tags
        if has_key(tag, 'children')
            call s:SortTags(tag.children, a:comparemethod)
        endif
    endfor
endfunction

" s:CompareByKind() {{{2
function! s:CompareByKind(tag1, tag2)
    let typeinfo = s:compare_typeinfo

    if typeinfo.kinddict[a:tag1.fields.kind] <
     \ typeinfo.kinddict[a:tag2.fields.kind]
        return -1
    elseif typeinfo.kinddict[a:tag1.fields.kind] >
         \ typeinfo.kinddict[a:tag2.fields.kind]
        return 1
    else
        " Ignore '~' prefix for C++ destructors to sort them directly under
        " the constructors
        if a:tag1.name[0] == '~'
            let name1 = a:tag1.name[1:]
        else
            let name1 = a:tag1.name
        endif
        if a:tag2.name[0] == '~'
            let name2 = a:tag2.name[1:]
        else
            let name2 = a:tag2.name
        endif

        if name1 <= name2
            return -1
        else
            return 1
        endif
    endif
endfunction

" s:CompareByLine() {{{2
function! s:CompareByLine(tag1, tag2)
    return a:tag1.fields.line - a:tag2.fields.line
endfunction

" s:ToggleSort() {{{2
function! s:ToggleSort()
    if !has_key(s:known_files, s:current_file)
        return
    endif

    let curline = line('.')

    let fileinfo = s:known_files[s:current_file]

    match none

    let s:compare_typeinfo = s:known_types[fileinfo.ftype]

    if has_key(s:compare_typeinfo, 'sort')
        let s:compare_typeinfo.sort = !s:compare_typeinfo.sort
    else
        let g:tagbar_sort = !g:tagbar_sort
    endif

    if has_key(s:compare_typeinfo, 'sort')
        if s:compare_typeinfo.sort
            call s:SortTags(fileinfo.tags, 's:CompareByKind')
        else
            call s:SortTags(fileinfo.tags, 's:CompareByLine')
        endif
    elseif g:tagbar_sort
        call s:SortTags(fileinfo.tags, 's:CompareByKind')
    else
        call s:SortTags(fileinfo.tags, 's:CompareByLine')
    endif

    call s:RenderContent()

    execute curline
endfunction

" Display {{{1
" s:RenderContent() {{{2
function! s:RenderContent(...)
    if a:0 == 1
        let fname = a:1
    else
        let fname = s:current_file
    endif

    if fname == ''
        return
    endif

    let tagbarwinnr = bufwinnr('__Tagbar__')

    if &filetype == 'tagbar'
        let in_tagbar = 1
    else
        let in_tagbar = 0
        let prevwinnr = winnr()
        execute tagbarwinnr . 'wincmd w'
    endif

    if fname == s:current_file
        " We're redisplaying the same file, so save the view
        let saveline = line('.')
        let savecol  = col('.')
        let topline  = line('w0')
    endif

    let lazyredraw_save = &lazyredraw
    set lazyredraw

    setlocal modifiable

    silent! %delete _

    call s:PrintHelp()

    " If we don't have an entry for the file by now something must have gone
    " wrong
    if !has_key(s:known_files, fname)
        silent! put ='There was an error processing the file. Please run ' .
                   \ 'ctags manually to determine what the problem is.'
        normal! gqq

        let s:current_file = ''

        setlocal nomodifiable
        let &lazyredraw = lazyredraw_save

        if !in_tagbar
            execute prevwinnr . 'wincmd w'
        endif

        return
    endif
    let fileinfo = s:known_files[fname]

    let typeinfo = s:known_types[fileinfo.ftype]

    " Print tags
    call s:PrintKinds(typeinfo, fileinfo)

    setlocal nomodifiable

    if fname == s:current_file
        let scrolloff_save = &scrolloff
        set scrolloff=0

        call cursor(topline, 1)
        normal! zt
        call cursor(saveline, savecol)

        let &scrolloff = scrolloff_save
    else
        " Make sure as much of the Tagbar content as possible is shown in the
        " window by jumping to the top after drawing
        execute 1
        call winline()
    endif

    let &lazyredraw = lazyredraw_save

    if !in_tagbar
        execute prevwinnr . 'wincmd w'
    endif
endfunction

" s:PrintKinds() {{{2
function! s:PrintKinds(typeinfo, fileinfo)
    let first_kind = 1

    for kind in a:typeinfo.kinds
        let curtags = filter(copy(a:fileinfo.tags),
                           \ 'v:val.fields.kind ==# kind.short')

        if empty(curtags)
            continue
        endif

        if has_key(a:typeinfo, 'kind2scope') &&
         \ has_key(a:typeinfo.kind2scope, kind.short)
            " Scoped tags
            for tag in curtags
                let taginfo = ''

                if tag.fields.line == 0 " Tag is a pseudo-tag
                    let taginfo .= '*'
                endif
                if has_key(tag.fields, 'signature')
                    let taginfo .= tag.fields.signature
                endif
                let taginfo .= ' : ' . a:typeinfo.kind2scope[kind.short]

                let prefix = s:GetPrefix(tag, a:fileinfo)

                if g:tagbar_compact && first_kind && s:short_help
                    silent! 0put =prefix . tag.name . taginfo
                else
                    silent!  put =prefix . tag.name . taginfo
                endif

                " Save the current tagbar line in the tag for easy
                " highlighting access
                let curline                   = line('.')
                let tag.tline                 = curline
                let a:fileinfo.tline[curline] = tag

                if has_key(tag, 'children') &&
                         \ !a:fileinfo.tagfolds[tag.fields.kind][tag.fullpath]
                    for childtag in tag.children
                        call s:PrintTag(childtag, 1, a:fileinfo, a:typeinfo)
                    endfor
                endif

                if !g:tagbar_compact
                    silent! put _
                endif

            endfor
            let first_kind = 0
        else
            " Non-scoped tags
            if a:fileinfo.kindfolds[kind.short]
                let foldmarker = s:icon_closed
            else
                let foldmarker = s:icon_open
            endif

            if g:tagbar_compact && first_kind && s:short_help
                silent! 0put =foldmarker . ' ' . kind.long
            else
                silent!  put =foldmarker . ' ' . kind.long
            endif

            let kindtag                   = curtags[0].parent
            let curline                   = line('.')
            let kindtag.tline             = curline
            let a:fileinfo.tline[curline] = kindtag

            if !a:fileinfo.kindfolds[kind.short]
                for tag in curtags
                    let taginfo = ''

                    if has_key(tag.fields, 'signature')
                        let taginfo .= tag.fields.signature
                    endif

                    let prefix = s:GetPrefix(tag, a:fileinfo)

                    silent! put ='  ' . prefix . tag.name . taginfo

                    " Save the current tagbar line in the tag for easy
                    " highlighting access
                    let curline                   = line('.')
                    let tag.tline                 = curline
                    let a:fileinfo.tline[curline] = tag
                    let tag.depth                 = 1
                endfor
            endif

            if !g:tagbar_compact
                silent! put _
            endif

            let first_kind = 0
        endif
    endfor
endfunction

" s:PrintTag() {{{2
function! s:PrintTag(tag, depth, fileinfo, typeinfo)
    let taginfo = ''

    if a:tag.fields.line == 0 " Tag is a pseudo-tag
        let taginfo .= '*'
    endif
    if has_key(a:tag.fields, 'signature')
        let taginfo .= a:tag.fields.signature
    endif
    if has_key(a:typeinfo.kind2scope, a:tag.fields.kind)
        let taginfo .= ' : ' . a:typeinfo.kind2scope[a:tag.fields.kind]
    endif

    let prefix = s:GetPrefix(a:tag, a:fileinfo)

    " Print tag indented according to depth
    silent! put =repeat(' ', a:depth * 2) . prefix . a:tag.name . taginfo

    " Save the current tagbar line in the tag for easy
    " highlighting access
    let curline                   = line('.')
    let a:tag.tline               = curline
    let a:fileinfo.tline[curline] = a:tag

    " Recursively print children
    if has_key(a:tag, 'children') &&
     \ !a:fileinfo.tagfolds[a:tag.fields.kind][a:tag.fullpath]
        for childtag in a:tag.children
            call s:PrintTag(childtag, a:depth + 1, a:fileinfo, a:typeinfo)
        endfor
    endif
endfunction

" s:GetPrefix() {{{2
function! s:GetPrefix(tag, fileinfo)
    if has_key(a:tag, 'children') && !empty(a:tag.children)
        if a:fileinfo.tagfolds[a:tag.fields.kind][a:tag.fullpath]
            let prefix = s:icon_closed
        else
            let prefix = s:icon_open
        endif
    else
        let prefix = ' '
    endif

    if has_key(a:tag.fields, 'access') &&
     \ has_key(s:access_symbols, a:tag.fields.access)
        let prefix .= s:access_symbols[a:tag.fields.access]
    else
        let prefix .= ' '
    endif

    return prefix
endfunction

" s:PrintHelp() {{{2
function! s:PrintHelp()
    if !g:tagbar_compact && s:short_help
        silent! 0put ='\" Press <F1> for help'
        silent!  put _
    elseif !s:short_help
        silent! 0put ='\" Tagbar keybindings'
        silent!  put ='\"'
        silent!  put ='\" --------- General ---------'
        silent!  put ='\" <Enter> : Jump to tag definition'
        silent!  put ='\" <Space> : Display tag prototype'
        silent!  put ='\"'
        silent!  put ='\" ---------- Folds ----------'
        silent!  put ='\" o       : Toggle fold'
        silent!  put ='\" +       : Open fold'
        silent!  put ='\" -       : Close fold'
        silent!  put ='\" x       : Close parent'
        silent!  put ='\" *       : Open all folds'
        silent!  put ='\" =       : Close all folds'
        silent!  put ='\"'
        silent!  put ='\" ---------- Misc -----------'
        silent!  put ='\" s       : Toggle sort'
        silent!  put ='\" z       : Zoom window in/out'
        silent!  put ='\" q       : Close window'
        silent!  put ='\" <F1>    : Remove help'
        silent!  put _
    endif
endfunction

" s:RenderKeepView() {{{2
" The gist of this function was taken from NERDTree by Martin Grenfell.
function! s:RenderKeepView(...)
    if a:0 == 1
        let line = a:1
    else
        let line = line('.')
    endif

    let curcol  = col('.')
    let topline = line('w0')

    call s:RenderContent()

    let scrolloff_save = &scrolloff
    set scrolloff=0

    call cursor(topline, 1)
    normal! zt
    call cursor(line, curcol)

    let &scrolloff = scrolloff_save

    redraw
endfunction

" User actions {{{1
" s:HighlightTag() {{{2
function! s:HighlightTag()
    let fileinfo = s:known_files[s:current_file]

    let curline = line('.')

    let tagline = 0

    " If a tag appears in a file more than once (for example namespaces in
    " C++) only one of them has a 'tline' entry and can thus be highlighted.
    " The only way to solve this would be to go over the whole tag list again,
    " making everything slower. Since this should be a rare occurence and
    " highlighting isn't /that/ important ignore it for now.
    for line in range(curline, 1, -1)
        if has_key(fileinfo.fline, line)
            let tag     = fileinfo.fline[line]
            let tagline = tag.tline
            break
        endif
    endfor

    let eventignore_save = &eventignore
    set eventignore=all

    let tagbarwinnr = bufwinnr('__Tagbar__')
    let prevwinnr   = winnr()
    execute tagbarwinnr . 'wincmd w'

    match none

    " No tag above cursor position so don't do anything
    if tagline == 0
        execute prevwinnr . 'wincmd w'
        let &eventignore = eventignore_save
        redraw
        return
    endif

    " Check whether the tag is inside a closed fold and highlight the parent
    " instead in that case
    let parent = tag.parent
    while !empty(parent)
        if has_key(parent, 'numtags')
            if fileinfo.kindfolds[parent.short]
                let tagline = parent.tline
            endif
            break
        else
            if fileinfo.tagfolds[parent.fields.kind][parent.fullpath]
                let tagline = parent.tline
            endif
            break
        endif
        let parent = parent.parent
    endwhile

    " Go to the line containing the tag
    execute tagline

    " Make sure the tag is visible in the window
    call winline()

    let foldpat = '[' . s:icon_open . s:icon_closed . ' ]'
    let pattern = '/^\%' . tagline . 'l\s*' . foldpat . '[-+# ]\zs[^( ]\+\ze/'
    execute 'match Search ' . pattern

    execute prevwinnr . 'wincmd w'

    let &eventignore = eventignore_save

    redraw
endfunction

" s:JumpToTag() {{{2
function! s:JumpToTag()
    let taginfo = s:GetTagInfo(line('.'), 1)

    let autoclose = w:autoclose

    if empty(taginfo) || has_key(taginfo, 'numtags')
        return
    endif

    execute 'wincmd p'

    " Mark current position so it can be jumped back to
    mark '

    " Jump to the line where the tag is defined. Don't use the search pattern
    " since it doesn't take the scope into account and thus can fail if tags
    " with the same name are defined in different scopes (e.g. classes)
    execute taginfo.fields.line

    " Center the tag in the window
    normal! z.

    if foldclosed('.') != -1
        .foldopen!
    endif

    redraw

    if g:tagbar_autoclose || autoclose
        call s:CloseWindow()
    else
        call s:HighlightTag()
    endif
endfunction

" s:ShowPrototype() {{{2
function! s:ShowPrototype()
    let taginfo = s:GetTagInfo(line('.'), 1)

    if empty(taginfo)
        return
    endif

    if has_key(taginfo, 'prototype')
        echo taginfo.prototype
    else
        echo taginfo.name . ': ' .
           \ taginfo.numtags . ' ' . (taginfo.numtags > 1 ? 'tags' : 'tag')
    endif
endfunction

" s:ToggleHelp() {{{2
function! s:ToggleHelp()
    let s:short_help = !s:short_help

    " Prevent highlighting from being off after adding/removing the help text
    match none

    call s:RenderContent()

    execute 1
    redraw
endfunction

" Folding {{{1
" s:OpenFold() {{{2
function! s:OpenFold()
    if !has_key(s:known_files, s:current_file)
        return
    endif

    let curline = line('.')

    let tag = s:GetTagInfo(curline, 0)
    if empty(tag)
        return
    endif

    let fileinfo = s:known_files[s:current_file]

    if has_key(tag, 'numtags')
        " Tag is 'kind' header
        let fileinfo.kindfolds[tag.short] = 0
    elseif has_key(tag, 'children') && !empty(tag.children) &&
         \ fileinfo.tagfolds[tag.fields.kind][tag.fullpath]
        " Tag is parent of a scope
        let fileinfo.tagfolds[tag.fields.kind][tag.fullpath] = 0
    endif

    call s:RenderKeepView()
endfunction

" s:CloseFold() {{{2
function! s:CloseFold()
    if !has_key(s:known_files, s:current_file)
        return
    endif

    match none

    let curline = line('.')
    let newline = curline

    let curtag = s:GetTagInfo(curline, 0)
    if empty(curtag)
        return
    endif

    let nexttag = s:GetTagInfo(curline + 1, 0)

    let fileinfo = s:known_files[s:current_file]

    if !has_key(curtag, 'depth')
        " Tag is 'kind' header
        let fileinfo.kindfolds[curtag.short] = 1
    elseif curtag.depth == 1 && has_key(curtag.parent, 'numtags')
        " Tag is child of generic 'kind'
        let fileinfo.kindfolds[curtag.parent.short] = 1
        let newline = curtag.parent.tline
    elseif has_key(nexttag, 'depth') && nexttag.depth > curtag.depth
        " Tag is parent of a scope
        let fileinfo.tagfolds[curtag.fields.kind][curtag.fullpath] = 1
    elseif empty(curtag.parent)
        " Tag is top-level parent of a scope without children, so there's
        " nothing to do
        return
    else
        " Tag is normal child, so close parent
        let parent = curtag.parent
        let fileinfo.tagfolds[parent.fields.kind][parent.fullpath] = 1
        let newline = parent.tline
    endif

    call s:RenderKeepView(newline)
endfunction

" s:CloseParent() {{{2
function! s:CloseParent()
    if !has_key(s:known_files, s:current_file)
        return
    endif

    match none

    let curline = line('.')
    let newline = curline

    let curtag  = s:GetTagInfo(curline, 0)

    let fileinfo = s:known_files[s:current_file]

    if has_key(curtag, 'depth') && curtag.depth == 1 &&
     \ has_key(curtag.parent, 'numtags')
        " Tag is child of generic 'kind'
        let fileinfo.kindfolds[curtag.parent.short] = 1
        let newline = curtag.parent.tline
    elseif has_key(curtag, 'parent') && !empty(curtag.parent)
        " Tag is normal child, so close parent
        let parent = curtag.parent
        let fileinfo.tagfolds[parent.fields.kind][parent.fullpath] = 1
        let newline = parent.tline
    endif

    call s:RenderKeepView(newline)
endfunction

" s:ToggleFold() {{{2
function! s:ToggleFold()
    if !has_key(s:known_files, s:current_file)
        return
    endif

    match none

    let curtag = s:GetTagInfo(line('.'), 0)
    if empty(curtag)
        return
    endif

    let fileinfo = s:known_files[s:current_file]

    if !has_key(curtag, 'depth')
        let fileinfo.kindfolds[curtag.short] = !fileinfo.kindfolds[curtag.short]
    elseif has_key(curtag, 'children') && !empty(curtag.children)
        if fileinfo.tagfolds[curtag.fields.kind][curtag.fullpath]
            call s:OpenFold()
        else
            call s:CloseFold()
        endif
    endif

    call s:RenderKeepView()
endfunction

" s:SetFoldLevel() {{{2
function! s:SetFoldLevel(level, force)
    if a:level < 0
        echoerr 'Foldlevel can''t be negative'
        return
    endif

    if !has_key(s:known_files, s:current_file)
        return
    endif

    let fileinfo = s:known_files[s:current_file]

    call s:SetFoldLevelRecursive(fileinfo, fileinfo.tags, a:level, a:force)

    let typeinfo = s:known_types[fileinfo.ftype]

    " Apply foldlevel to 'kind's
    if min([a:level, fileinfo.foldlevel]) == 0 || a:force
        if a:level == 0
            for kind in typeinfo.kinds
                let fileinfo.kindfolds[kind.short] = 1
            endfor
        else
            for kind in typeinfo.kinds
                let fileinfo.kindfolds[kind.short] = 0
            endfor
        endif
    endif

    let fileinfo.foldlevel = a:level

    call s:RenderContent()
endfunction

" s:SetFoldLevelRecursive() {{{2
" Apply foldlevel to normal tags
function! s:SetFoldLevelRecursive(fileinfo, tags, level, force)
    " Only change folds in the range between the old and the new foldlevel
    " (unless 'force' is true)
    let left  = min([a:level, a:fileinfo.foldlevel])
    let right = max([a:level, a:fileinfo.foldlevel])

    for tag in a:tags
        if (left <= tag.depth && tag.depth <= right) || a:force
            if tag.depth >= a:level
                let a:fileinfo.tagfolds[tag.fields.kind][tag.fullpath] = 1
            else
                let a:fileinfo.tagfolds[tag.fields.kind][tag.fullpath] = 0
            endif
        endif

        if has_key(tag, 'children')
            call s:SetFoldLevelRecursive(a:fileinfo, tag.children, a:level,
                                       \ a:force)
        endif
    endfor
endfunction

" Helper functions {{{1
" s:CleanUp() {{{2
function! s:CleanUp()
    silent! autocmd! TagbarAutoCmds

    unlet s:current_file
    unlet s:is_maximized
    unlet s:compare_typeinfo
    unlet s:short_help
endfunction

" s:QuitIfOnlyWindow() {{{2
function! s:QuitIfOnlyWindow()
    " Before quitting Vim, delete the tagbar buffer so that
    " the '0 mark is correctly set to the previous buffer.
    if winbufnr(2) == -1
        " Check if there is more than one tab page
        if tabpagenr('$') == 1
            bdelete
            quit
        else
            close
        endif
    endif
endfunction

" s:AutoUpdate() {{{2
function! s:AutoUpdate(fname)
    " Don't do anything if tagbar is not open or if we're in the tagbar window
    let tagbarwinnr = bufwinnr('__Tagbar__')
    if tagbarwinnr == -1 || &filetype == 'tagbar'
        return
    endif

    " Don't do anything if the file isn't supported
    if !s:IsValidFile(a:fname, &filetype)
        return
    endif

    " Process the file if it's unknown or the information is outdated
    if has_key(s:known_files, a:fname)
        if s:known_files[a:fname].mtime != getftime(a:fname)
            call s:ProcessFile(a:fname, &filetype)
        endif
    else
        call s:ProcessFile(a:fname, &filetype)
    endif

    " Display the tagbar content
    call s:RenderContent(a:fname)

    " If we don't have an entry for the file by now something must have gone
    " wrong
    if !has_key(s:known_files, a:fname)
        return
    endif

    let s:current_file = a:fname

    call s:HighlightTag()
endfunction

" s:IsValidFile() {{{2
function! s:IsValidFile(fname, ftype)
    if a:fname == '' || a:ftype == ''
        return 0
    endif

    if !filereadable(a:fname)
        return 0
    endif

    if !has_key(s:known_types, a:ftype)
        return 0
    endif

    return 1
endfunction

" s:GetTagInfo() {{{2
" Return the info dictionary of the tag on the specified line. If the line
" does not contain a valid tag (for example because it is empty or only
" contains a pseudo-tag) return an empty dictionary.
function! s:GetTagInfo(linenr, ignorepseudo)
    if !has_key(s:known_files, s:current_file)
        return {}
    endif

    " Don't do anything in empty and comment lines
    let curline = getline(a:linenr)
    if curline =~ '^\s*$' || curline[0] == '"'
        return {}
    endif

    let fileinfo = s:known_files[s:current_file]

    " Check if there is a tag on the current line
    if !has_key(fileinfo.tline, a:linenr)
        return {}
    endif

    let taginfo = fileinfo.tline[a:linenr]

    " Check if the current tag is not a pseudo-tag
    if !has_key(taginfo, 'numtags')
     \ && (a:ignorepseudo && taginfo.fields.line == 0)
        return {}
    endif

    return taginfo
endfunction

" s:CheckMouseClick() {{{2
function! s:CheckMouseClick()
    let line   = getline('.')
    let curcol = col('.')

    if (match(line, s:icon_open . '[-+ ]') + 1) == curcol
        call s:CloseFold()
    elseif (match(line, s:icon_closed . '[-+ ]') + 1) == curcol
        call s:OpenFold()
    endif
endfunction

" TagbarBalloonExpr() {{{2
function! TagbarBalloonExpr()
    let taginfo = s:GetTagInfo(v:beval_lnum, 1)

    if empty(taginfo)
        return
    endif

    if has_key(taginfo, 'prototype')
        return taginfo.prototype
    else
        return taginfo.name . ': ' .
             \ taginfo.numtags . ' ' . (taginfo.numtags > 1 ? 'tags' : 'tag')
    endif
endfunction

" TagbarGenerateStatusline() {{{2
function! TagbarGenerateStatusline()
    if g:tagbar_sort
        let text = '[Name]'
    else
        let text = '[Order]'
    endif

    let filename = fnamemodify(s:current_file, ':t')
    let text .= ' ' . filename

    return text
endfunction

" Commands {{{1
command! -nargs=0 TagbarToggle        call s:ToggleWindow()
command! -nargs=0 TagbarOpen          call s:OpenWindow(0)
command! -nargs=0 TagbarOpenAutoClose call s:OpenWindow(1)
command! -nargs=0 TagbarClose         call s:CloseWindow()
command! -nargs=1 -bang TagbarSetFoldlevel
                                  \ call s:SetFoldLevel(<args>, '<bang>' == '!')

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
