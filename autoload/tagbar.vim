" ============================================================================
" File:        tagbar.vim
" Description: List the current file's tags in a sidebar, ordered by class etc
" Author:      Jan Larres <jan@majutsushi.net>
" Licence:     Vim licence
" Website:     http://majutsushi.github.com/tagbar/
" Version:     2.2
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

scriptencoding utf-8

" Initialization {{{1

" Basic init {{{2

if !exists('g:tagbar_ctags_bin')
    if executable('ctags-exuberant')
        let g:tagbar_ctags_bin = 'ctags-exuberant'
    elseif executable('exuberant-ctags')
        let g:tagbar_ctags_bin = 'exuberant-ctags'
    elseif executable('exctags')
        let g:tagbar_ctags_bin = 'exctags'
    elseif has('macunix') && executable('/usr/local/bin/ctags')
        " Homebrew default location
        let g:tagbar_ctags_bin = '/usr/local/bin/ctags'
    elseif has('macunix') && executable('/opt/local/bin/ctags')
        " Macports default location
        let g:tagbar_ctags_bin = '/opt/local/bin/ctags'
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
    " reset 'wildignore' temporarily in case *.exe is included in it
    let wildignore_save = &wildignore
    set wildignore&

    let g:tagbar_ctags_bin = expand(g:tagbar_ctags_bin)

    let &wildignore = wildignore_save

    if !executable(g:tagbar_ctags_bin)
        echomsg 'Tagbar: Exuberant ctags not found in specified place,'
              \ 'skipping plugin'
        finish
    endif
endif

redir => s:ftype_out
silent filetype
redir END
if s:ftype_out !~# 'detection:ON'
    echomsg 'Tagbar: Filetype detection is turned off, skipping plugin'
    unlet s:ftype_out
    finish
endif
unlet s:ftype_out

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
let s:checked_ctags     = 0
let s:window_expanded   = 0

let s:access_symbols = {
    \ 'public'    : '+',
    \ 'protected' : '#',
    \ 'private'   : '-'
\ }

let g:loaded_tagbar = 1

let s:last_highlight_tline = 0

" s:InitTypes() {{{2
function! s:InitTypes()
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
    " Alternatively jsctags/doctorjs will be used if available.
    let type_javascript = {}
    let type_javascript.ctagstype = 'javascript'
    if executable('jsctags')
        let type_javascript.kinds = [
            \ {'short' : 'v', 'long' : 'variables', 'fold' : 0},
            \ {'short' : 'f', 'long' : 'functions', 'fold' : 0}
        \ ]
        let type_javascript.sro        = '.'
        let type_javascript.kind2scope = {
            \ 'v' : 'namespace',
            \ 'f' : 'namespace'
        \ }
        let type_javascript.scope2kind = {
            \ 'namespace' : 'v'
        \ }
        let type_javascript.ctagsbin   = 'jsctags'
        let type_javascript.ctagsargs  = '-f -'
    else
        let type_javascript.kinds = [
            \ {'short' : 'v', 'long' : 'global variables', 'fold' : 0},
            \ {'short' : 'c', 'long' : 'classes',          'fold' : 0},
            \ {'short' : 'p', 'long' : 'properties',       'fold' : 0},
            \ {'short' : 'm', 'long' : 'methods',          'fold' : 0},
            \ {'short' : 'f', 'long' : 'functions',        'fold' : 0}
        \ ]
    endif
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

    let s:type_init_done = 1
endfunction

" s:GetUserTypeDefs() {{{2
function! s:GetUserTypeDefs()
    redir => defs
    silent execute 'let g:'
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

" s:RestoreSession() {{{2
" Properly restore Tagbar after a session got loaded
function! s:RestoreSession()
    let tagbarwinnr = bufwinnr('__Tagbar__')
    if tagbarwinnr == -1
        " Tagbar wasn't open in the saved session, nothing to do
        return
    else
        let in_tagbar = 1
        if winnr() != tagbarwinnr
            execute tagbarwinnr . 'wincmd w'
            let in_tagbar = 0
        endif
    endif

    if !s:type_init_done
        call s:InitTypes()
    endif

    if !s:checked_ctags
        if !s:CheckForExCtags()
            return
        endif
    endif

    call s:InitWindow(g:tagbar_autoclose)

    " Leave the Tagbar window and come back so the update event gets triggered
    wincmd p
    execute tagbarwinnr . 'wincmd w'

    if !in_tagbar
        wincmd p
    endif
endfunction

" s:MapKeys() {{{2
function! s:MapKeys()
    nnoremap <script> <silent> <buffer> <2-LeftMouse>
                                              \ :call <SID>JumpToTag(0)<CR>
    nnoremap <script> <silent> <buffer> <LeftRelease>
                                 \ <LeftRelease>:call <SID>CheckMouseClick()<CR>

    inoremap <script> <silent> <buffer> <2-LeftMouse>
                                              \ <C-o>:call <SID>JumpToTag(0)<CR>
    inoremap <script> <silent> <buffer> <LeftRelease>
                            \ <LeftRelease><C-o>:call <SID>CheckMouseClick()<CR>

    nnoremap <script> <silent> <buffer> <CR>    :call <SID>JumpToTag(0)<CR>
    nnoremap <script> <silent> <buffer> p       :call <SID>JumpToTag(1)<CR>
    nnoremap <script> <silent> <buffer> <Space> :call <SID>ShowPrototype()<CR>

    nnoremap <script> <silent> <buffer> +        :call <SID>OpenFold()<CR>
    nnoremap <script> <silent> <buffer> <kPlus>  :call <SID>OpenFold()<CR>
    nnoremap <script> <silent> <buffer> zo       :call <SID>OpenFold()<CR>
    nnoremap <script> <silent> <buffer> -        :call <SID>CloseFold()<CR>
    nnoremap <script> <silent> <buffer> <kMinus> :call <SID>CloseFold()<CR>
    nnoremap <script> <silent> <buffer> zc       :call <SID>CloseFold()<CR>
    nnoremap <script> <silent> <buffer> o        :call <SID>ToggleFold()<CR>
    nnoremap <script> <silent> <buffer> za       :call <SID>ToggleFold()<CR>

    nnoremap <script> <silent> <buffer> *    :call <SID>SetFoldLevel(99)<CR>
    nnoremap <script> <silent> <buffer> <kMultiply>
                                           \ :call <SID>SetFoldLevel(99)<CR>
    nnoremap <script> <silent> <buffer> zR   :call <SID>SetFoldLevel(99)<CR>
    nnoremap <script> <silent> <buffer> =    :call <SID>SetFoldLevel(0)<CR>
    nnoremap <script> <silent> <buffer> zM   :call <SID>SetFoldLevel(0)<CR>

    nnoremap <script> <silent> <buffer> <C-N>
                                        \ :call <SID>GotoNextToplevelTag(1)<CR>
    nnoremap <script> <silent> <buffer> <C-P>
                                        \ :call <SID>GotoNextToplevelTag(-1)<CR>

    nnoremap <script> <silent> <buffer> s    :call <SID>ToggleSort()<CR>
    nnoremap <script> <silent> <buffer> x    :call <SID>ZoomWindow()<CR>
    nnoremap <script> <silent> <buffer> q    :call <SID>CloseWindow()<CR>
    nnoremap <script> <silent> <buffer> <F1> :call <SID>ToggleHelp()<CR>
endfunction

" s:CreateAutocommands() {{{2
function! s:CreateAutocommands()
    augroup TagbarAutoCmds
        autocmd!
        autocmd BufEnter   __Tagbar__ nested call s:QuitIfOnlyWindow()
        autocmd BufUnload  __Tagbar__ call s:CleanUp()
        autocmd CursorHold __Tagbar__ call s:ShowPrototype()

        autocmd BufWritePost *
            \ if line('$') < g:tagbar_updateonsave_maxlines |
                \ call s:AutoUpdate(fnamemodify(expand('<afile>'), ':p')) |
            \ endif
        autocmd BufEnter,CursorHold,FileType * call
                    \ s:AutoUpdate(fnamemodify(expand('<afile>'), ':p'))
        autocmd BufDelete * call
                    \ s:CleanupFileinfo(fnamemodify(expand('<afile>'), ':p'))
    augroup END

    let s:autocommands_done = 1
endfunction

" s:CheckForExCtags() {{{2
" Test whether the ctags binary is actually Exuberant Ctags and not GNU ctags
" (or something else)
function! s:CheckForExCtags()
    let ctags_cmd = s:EscapeCtagsCmd(g:tagbar_ctags_bin, '--version')
    if ctags_cmd == ''
        return
    endif

    let ctags_output = s:ExecuteCtags(ctags_cmd)

    if v:shell_error || ctags_output !~# 'Exuberant Ctags'
        echoerr 'Tagbar: Ctags doesn''t seem to be Exuberant Ctags!'
        echomsg 'GNU ctags will NOT WORK.'
              \ 'Please download Exuberant Ctags from ctags.sourceforge.net'
              \ 'and install it in a directory in your $PATH'
              \ 'or set g:tagbar_ctags_bin.'
        echomsg 'Executed command: "' . ctags_cmd . '"'
        if !empty(ctags_output)
            echomsg 'Command output:'
            for line in split(ctags_output, '\n')
                echomsg line
            endfor
        endif
        return 0
    elseif !s:CheckExCtagsVersion(ctags_output)
        echoerr 'Tagbar: Exuberant Ctags is too old!'
        echomsg 'You need at least version 5.5 for Tagbar to work.'
              \ 'Please download a newer version from ctags.sourceforge.net.'
        echomsg 'Executed command: "' . ctags_cmd . '"'
        if !empty(ctags_output)
            echomsg 'Command output:'
            for line in split(ctags_output, '\n')
                echomsg line
            endfor
        endif
        return 0
    else
        let s:checked_ctags = 1
        return 1
    endif
endfunction

" s:CheckExCtagsVersion() {{{2
function! s:CheckExCtagsVersion(output)
    if a:output =~ 'Exuberant Ctags Development'
        return 1
    endif

    let matchlist = matchlist(a:output, '\vExuberant Ctags (\d+)\.(\d+)')
    let major     = matchlist[1]
    let minor     = matchlist[2]

    return major >= 6 || (major == 5 && minor >= 5)
endfunction

" Prototypes {{{1
" Normal tag {{{2
let s:NormalTag = {}

" s:NormalTag._init() {{{3
function! s:NormalTag._init(name) dict
    let self.name        = a:name
    let self.fields      = {}
    let self.fields.line = 0
    let self.path        = ''
    let self.fullpath    = a:name
    let self.depth       = 0
    let self.parent      = {}
    let self.tline       = -1
    let self.fileinfo    = {}
endfunction

" s:NormalTag.New() {{{3
function! s:NormalTag.New(name) dict
    let newobj = copy(self)

    call newobj._init(a:name)

    return newobj
endfunction

" s:NormalTag.isNormalTag() {{{3
function! s:NormalTag.isNormalTag() dict
    return 1
endfunction

" s:NormalTag.isPseudoTag() {{{3
function! s:NormalTag.isPseudoTag() dict
    return 0
endfunction

" s:NormalTag.isKindheader() {{{3
function! s:NormalTag.isKindheader() dict
    return 0
endfunction

" s:NormalTag.getPrototype() {{{3
function! s:NormalTag.getPrototype() dict
    return self.prototype
endfunction

" s:NormalTag._getPrefix() {{{3
function! s:NormalTag._getPrefix() dict
    let fileinfo = self.fileinfo

    if has_key(self, 'children') && !empty(self.children)
        if fileinfo.tagfolds[self.fields.kind][self.fullpath]
            let prefix = s:icon_closed
        else
            let prefix = s:icon_open
        endif
    else
        let prefix = ' '
    endif
    if has_key(self.fields, 'access')
        let prefix .= get(s:access_symbols, self.fields.access, ' ')
    else
        let prefix .= ' '
    endif

    return prefix
endfunction

" s:NormalTag.initFoldState() {{{3
function! s:NormalTag.initFoldState() dict
    let fileinfo = self.fileinfo

    if s:known_files.has(fileinfo.fpath) &&
     \ has_key(fileinfo._tagfolds_old[self.fields.kind], self.fullpath)
        " The file has been updated and the tag was there before, so copy its
        " old fold state
        let fileinfo.tagfolds[self.fields.kind][self.fullpath] =
                    \ fileinfo._tagfolds_old[self.fields.kind][self.fullpath]
    elseif self.depth >= fileinfo.foldlevel
        let fileinfo.tagfolds[self.fields.kind][self.fullpath] = 1
    else
        let fileinfo.tagfolds[self.fields.kind][self.fullpath] =
                    \ fileinfo.kindfolds[self.fields.kind]
    endif
endfunction

" s:NormalTag.getClosedParentTline() {{{3
function! s:NormalTag.getClosedParentTline() dict
    let tagline = self.tline
    let fileinfo = self.fileinfo

    let parent = self.parent
    while !empty(parent)
        if parent.isFolded()
            let tagline = parent.tline
            break
        endif
        let parent = parent.parent
    endwhile

    return tagline
endfunction

" s:NormalTag.isFoldable() {{{3
function! s:NormalTag.isFoldable() dict
    return has_key(self, 'children') && !empty(self.children)
endfunction

" s:NormalTag.isFolded() {{{3
function! s:NormalTag.isFolded() dict
    return self.fileinfo.tagfolds[self.fields.kind][self.fullpath]
endfunction

" s:NormalTag.openFold() {{{3
function! s:NormalTag.openFold() dict
    if self.isFoldable()
        let self.fileinfo.tagfolds[self.fields.kind][self.fullpath] = 0
    endif
endfunction

" s:NormalTag.closeFold() {{{3
function! s:NormalTag.closeFold() dict
    let newline = line('.')

    if !empty(self.parent) && self.parent.isKindheader()
        " Tag is child of generic 'kind'
        call self.parent.closeFold()
        let newline = self.parent.tline
    elseif self.isFoldable() && !self.isFolded()
        " Tag is parent of a scope and is not folded
        let self.fileinfo.tagfolds[self.fields.kind][self.fullpath] = 1
        let newline = self.tline
    elseif !empty(self.parent)
        " Tag is normal child, so close parent
        let parent = self.parent
        let self.fileinfo.tagfolds[parent.fields.kind][parent.fullpath] = 1
        let newline = parent.tline
    endif

    return newline
endfunction

" s:NormalTag.setFolded() {{{3
function! s:NormalTag.setFolded(folded) dict
    let self.fileinfo.tagfolds[self.fields.kind][self.fullpath] = a:folded
endfunction

" s:NormalTag.openParents() {{{3
function! s:NormalTag.openParents() dict
    let parent = self.parent

    while !empty(parent)
        call parent.openFold()
        let parent = parent.parent
    endwhile
endfunction

" s:NormalTag.str() {{{3
function! s:NormalTag.str() dict
    let fileinfo = self.fileinfo
    let typeinfo = s:known_types[fileinfo.ftype]

    let suffix = get(self.fields, 'signature', '')
    if has_key(self.fields, 'type')
        let suffix .= ' : ' . self.fields.type
    elseif has_key(typeinfo, 'kind2scope') &&
         \ has_key(typeinfo.kind2scope, self.fields.kind)
        let suffix .= ' : ' . typeinfo.kind2scope[self.fields.kind]
    endif

    return self._getPrefix() . self.name . suffix . "\n"
endfunction

" Pseudo tag {{{2
let s:PseudoTag = {}

" s:PseudoTag._init() {{{3
function! s:PseudoTag._init(name) dict
    let self.name        = a:name
    let self.fields      = {}
    let self.fields.line = 0
    let self.path        = ''
    let self.fullpath    = a:name
    let self.depth       = 0
    let self.parent      = {}
    let self.tline       = -1
    let self.fileinfo    = {}
endfunction

" s:PseudoTag.New() {{{3
function! s:PseudoTag.New(name) dict
    let newobj = copy(self)

    call newobj._init(a:name)

    return newobj
endfunction

" s:PseudoTag.isNormalTag() {{{3
function! s:PseudoTag.isNormalTag() dict
    return 0
endfunction

" s:PseudoTag.isPseudoTag() {{{3
function! s:PseudoTag.isPseudoTag() dict
    return 1
endfunction

" s:PseudoTag.isKindheader() {{{3
function! s:PseudoTag.isKindheader() dict
    return 0
endfunction

" s:PseudoTag.getPrototype() {{{3
function! s:PseudoTag.getPrototype() dict
    return ''
endfunction

" s:PseudoTag._getPrefix() {{{3
function! s:PseudoTag._getPrefix() dict
    let fileinfo = self.fileinfo

    if has_key(self, 'children') && !empty(self.children)
        if fileinfo.tagfolds[self.fields.kind][self.fullpath]
            let prefix = s:icon_closed
        else
            let prefix = s:icon_open
        endif
    else
        let prefix = ' '
    endif
    if has_key(self.fields, 'access')
        let prefix .= get(s:access_symbols, self.fields.access, ' ')
    else
        let prefix .= ' '
    endif

    return prefix
endfunction

" s:PseudoTag.initFoldState() {{{3
function! s:PseudoTag.initFoldState() dict
    let fileinfo = self.fileinfo

    if s:known_files.has(fileinfo.fpath) &&
     \ has_key(fileinfo._tagfolds_old[self.fields.kind], self.fullpath)
        " The file has been updated and the tag was there before, so copy its
        " old fold state
        let fileinfo.tagfolds[self.fields.kind][self.fullpath] =
                    \ fileinfo._tagfolds_old[self.fields.kind][self.fullpath]
    elseif self.depth >= fileinfo.foldlevel
        let fileinfo.tagfolds[self.fields.kind][self.fullpath] = 1
    else
        let fileinfo.tagfolds[self.fields.kind][self.fullpath] =
                    \ fileinfo.kindfolds[self.fields.kind]
    endif
endfunction

" s:PseudoTag.getClosedParentTline() {{{3
function! s:PseudoTag.getClosedParentTline() dict
    let tagline = self.tline
    let fileinfo = self.fileinfo

    let parent = self.parent
    while !empty(parent)
        if parent.isFolded()
            let tagline = parent.tline
            break
        endif
        let parent = parent.parent
    endwhile

    return tagline
endfunction

" s:PseudoTag.isFoldable() {{{3
function! s:PseudoTag.isFoldable() dict
    return has_key(self, 'children') && !empty(self.children)
endfunction

" s:PseudoTag.isFolded() {{{3
function! s:PseudoTag.isFolded() dict
    return self.fileinfo.tagfolds[self.fields.kind][self.fullpath]
endfunction

" s:PseudoTag.openFold() {{{3
function! s:PseudoTag.openFold() dict
    if self.isFoldable()
        let self.fileinfo.tagfolds[self.fields.kind][self.fullpath] = 0
    endif
endfunction

" s:PseudoTag.closeFold() {{{3
function! s:PseudoTag.closeFold() dict
    let newline = line('.')

    if !empty(self.parent) && self.parent.isKindheader()
        " Tag is child of generic 'kind'
        call self.parent.closeFold()
        let newline = self.parent.tline
    elseif self.isFoldable() && !self.isFolded()
        " Tag is parent of a scope and is not folded
        let self.fileinfo.tagfolds[self.fields.kind][self.fullpath] = 1
        let newline = self.tline
    elseif !empty(self.parent)
        " Tag is normal child, so close parent
        let parent = self.parent
        let self.fileinfo.tagfolds[parent.fields.kind][parent.fullpath] = 1
        let newline = parent.tline
    endif

    return newline
endfunction

" s:PseudoTag.setFolded() {{{3
function! s:PseudoTag.setFolded(folded) dict
    let self.fileinfo.tagfolds[self.fields.kind][self.fullpath] = a:folded
endfunction

" s:PseudoTag.openParents() {{{3
function! s:PseudoTag.openParents() dict
    let parent = self.parent

    while !empty(parent)
        call parent.openFold()
        let parent = parent.parent
    endwhile
endfunction

" s:PseudoTag.str() {{{3
function! s:PseudoTag.str() dict
    let fileinfo = self.fileinfo
    let typeinfo = s:known_types[fileinfo.ftype]

    let suffix = get(self.fields, 'signature', '')
    if has_key(typeinfo.kind2scope, self.fields.kind)
        let suffix .= ' : ' . typeinfo.kind2scope[self.fields.kind]
    endif

    return self._getPrefix() . self.name . '*' . suffix
endfunction

" Kind header {{{2
let s:KindheaderTag = {}

" s:KindheaderTag._init() {{{3
function! s:KindheaderTag._init(name) dict
    let self.name        = a:name
    let self.fields      = {}
    let self.fields.line = 0
    let self.path        = ''
    let self.fullpath    = a:name
    let self.depth       = 0
    let self.parent      = {}
    let self.tline       = -1
    let self.fileinfo    = {}
endfunction

" s:KindheaderTag.New() {{{3
function! s:KindheaderTag.New(name) dict
    let newobj = copy(self)

    call newobj._init(a:name)

    return newobj
endfunction

" s:KindheaderTag.isNormalTag() {{{3
function! s:KindheaderTag.isNormalTag() dict
    return 0
endfunction

" s:KindheaderTag.isPseudoTag() {{{3
function! s:KindheaderTag.isPseudoTag() dict
    return 0
endfunction

" s:KindheaderTag.isKindheader() {{{3
function! s:KindheaderTag.isKindheader() dict
    return 1
endfunction

" s:KindheaderTag.getPrototype() {{{3
function! s:KindheaderTag.getPrototype() dict
    return self.name . ': ' .
         \ self.numtags . ' ' . (self.numtags > 1 ? 'tags' : 'tag')
endfunction

" s:KindheaderTag._getPrefix() {{{3
function! s:KindheaderTag._getPrefix() dict
    let fileinfo = self.fileinfo

    if has_key(self, 'children') && !empty(self.children)
        if fileinfo.tagfolds[self.fields.kind][self.fullpath]
            let prefix = s:icon_closed
        else
            let prefix = s:icon_open
        endif
    else
        let prefix = ' '
    endif
    if has_key(self.fields, 'access')
        let prefix .= get(s:access_symbols, self.fields.access, ' ')
    else
        let prefix .= ' '
    endif

    return prefix
endfunction

" s:KindheaderTag.initFoldState() {{{3
function! s:KindheaderTag.initFoldState() dict
    let fileinfo = self.fileinfo

    if s:known_files.has(fileinfo.fpath) &&
     \ has_key(fileinfo._tagfolds_old[self.fields.kind], self.fullpath)
        " The file has been updated and the tag was there before, so copy its
        " old fold state
        let fileinfo.tagfolds[self.fields.kind][self.fullpath] =
                    \ fileinfo._tagfolds_old[self.fields.kind][self.fullpath]
    elseif self.depth >= fileinfo.foldlevel
        let fileinfo.tagfolds[self.fields.kind][self.fullpath] = 1
    else
        let fileinfo.tagfolds[self.fields.kind][self.fullpath] =
                    \ fileinfo.kindfolds[self.fields.kind]
    endif
endfunction

" s:KindheaderTag.getClosedParentTline() {{{3
function! s:KindheaderTag.getClosedParentTline() dict
    let tagline = self.tline
    let fileinfo = self.fileinfo

    let parent = self.parent
    while !empty(parent)
        if parent.isFolded()
            let tagline = parent.tline
            break
        endif
        let parent = parent.parent
    endwhile

    return tagline
endfunction

" s:KindheaderTag.isFoldable() {{{3
function! s:KindheaderTag.isFoldable() dict
    return 1
endfunction

" s:KindheaderTag.isFolded() {{{3
function! s:KindheaderTag.isFolded() dict
    return self.fileinfo.kindfolds[self.short]
endfunction

" s:KindheaderTag.openFold() {{{3
function! s:KindheaderTag.openFold() dict
    let self.fileinfo.kindfolds[self.short] = 0
endfunction

" s:KindheaderTag.closeFold() {{{3
function! s:KindheaderTag.closeFold() dict
    let self.fileinfo.kindfolds[self.short] = 1
    return line('.')
endfunction

" s:KindheaderTag.setFolded() {{{3
function! s:KindheaderTag.setFolded(folded) dict
    let self.fileinfo.tagfolds[self.fields.kind][self.fullpath] = a:folded
endfunction

" s:KindheaderTag.openParents() {{{3
function! s:KindheaderTag.openParents() dict
    let parent = self.parent

    while !empty(parent)
        call parent.openFold()
        let parent = parent.parent
    endwhile
endfunction

" s:KindheaderTag.toggleFold() {{{3
function! s:KindheaderTag.toggleFold() dict
    let fileinfo = s:known_files.getCurrent()

    let fileinfo.kindfolds[self.short] = !fileinfo.kindfolds[self.short]
endfunction

" File info {{{2
let s:FileInfo = {}

" s:FileInfo.New() {{{3
function! s:FileInfo.New(fname, ftype) dict
    let newobj = copy(self)

    " The complete file path
    let newobj.fpath = a:fname

    " File modification time
    let newobj.mtime = getftime(a:fname)

    " The vim file type
    let newobj.ftype = a:ftype

    " List of the tags that are present in the file, sorted according to the
    " value of 'g:tagbar_sort'
    let newobj.tags = []

    " Dictionary of the tags, indexed by line number in the file
    let newobj.fline = {}

    " Dictionary of the tags, indexed by line number in the tagbar
    let newobj.tline = {}

    " Dictionary of the folding state of 'kind's, indexed by short name
    let newobj.kindfolds = {}
    let typeinfo = s:known_types[a:ftype]
    " copy the default fold state from the type info
    for kind in typeinfo.kinds
        let newobj.kindfolds[kind.short] =
                    \ g:tagbar_foldlevel == 0 ? 1 : kind.fold
    endfor

    " Dictionary of dictionaries of the folding state of individual tags,
    " indexed by kind and full path
    let newobj.tagfolds = {}
    for kind in typeinfo.kinds
        let newobj.tagfolds[kind.short] = {}
    endfor

    " The current foldlevel of the file
    let newobj.foldlevel = g:tagbar_foldlevel

    return newobj
endfunction

" s:FileInfo.reset() {{{3
" Reset stuff that gets regenerated while processing a file and save the old
" tag folds
function! s:FileInfo.reset() dict
    let self.mtime = getftime(self.fpath)
    let self.tags  = []
    let self.fline = {}
    let self.tline = {}

    let self._tagfolds_old = self.tagfolds
    let self.tagfolds = {}

    let typeinfo = s:known_types[self.ftype]
    for kind in typeinfo.kinds
        let self.tagfolds[kind.short] = {}
    endfor
endfunction

" s:FileInfo.clearOldFolds() {{{3
function! s:FileInfo.clearOldFolds() dict
    if exists('self._tagfolds_old')
        unlet self._tagfolds_old
    endif
endfunction

" s:FileInfo.sortTags() {{{3
function! s:FileInfo.sortTags() dict
    if has_key(s:compare_typeinfo, 'sort')
        if s:compare_typeinfo.sort
            call s:SortTags(self.tags, 's:CompareByKind')
        else
            call s:SortTags(self.tags, 's:CompareByLine')
        endif
    elseif g:tagbar_sort
        call s:SortTags(self.tags, 's:CompareByKind')
    else
        call s:SortTags(self.tags, 's:CompareByLine')
    endif
endfunction

" s:FileInfo.openKindFold() {{{3
function! s:FileInfo.openKindFold(kind) dict
    let self.kindfolds[a:kind.short] = 0
endfunction

" s:FileInfo.closeKindFold() {{{3
function! s:FileInfo.closeKindFold(kind) dict
    let self.kindfolds[a:kind.short] = 1
endfunction

" Known files {{{2
let s:known_files = {
    \ '_current' : {},
    \ '_files'   : {}
\ }

" s:known_files.getCurrent() {{{3
function! s:known_files.getCurrent() dict
    return self._current
endfunction

" s:known_files.setCurrent() {{{3
function! s:known_files.setCurrent(fileinfo) dict
    let self._current = a:fileinfo
endfunction

" s:known_files.get() {{{3
function! s:known_files.get(fname) dict
    return get(self._files, a:fname, {})
endfunction

" s:known_files.put() {{{3
" Optional second argument is the filename
function! s:known_files.put(fileinfo, ...) dict
    if a:0 == 1
        let self._files[a:1] = a:fileinfo
    else
        let fname = a:fileinfo.fpath
        let self._files[fname] = a:fileinfo
    endif
endfunction

" s:known_files.has() {{{3
function! s:known_files.has(fname) dict
    return has_key(self._files, a:fname)
endfunction

" s:known_files.rm() {{{3
function! s:known_files.rm(fname) dict
    if s:known_files.has(a:fname)
        call remove(self._files, a:fname)
    endif
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
function! s:OpenWindow(flags)
    let autofocus = a:flags =~# 'f'
    let jump      = a:flags =~# 'j'
    let autoclose = a:flags =~# 'c'

    " If the tagbar window is already open check jump flag
    " Also set the autoclose flag if requested
    let tagbarwinnr = bufwinnr('__Tagbar__')
    if tagbarwinnr != -1
        if winnr() != tagbarwinnr && jump
            execute tagbarwinnr . 'wincmd w'
            if autoclose
                let w:autoclose = autoclose
            endif
        endif
        return
    endif

    if !s:type_init_done
        call s:InitTypes()
    endif

    if !s:checked_ctags
        if !s:CheckForExCtags()
            return
        endif
    endif

    " Expand the Vim window to accomodate for the Tagbar window if requested
    if g:tagbar_expand && !s:window_expanded && has('gui_running')
        let &columns += g:tagbar_width + 1
        let s:window_expanded = 1
    endif

    let eventignore_save = &eventignore
    set eventignore=all

    let openpos = g:tagbar_left ? 'topleft vertical ' : 'botright vertical '
    exe 'silent keepalt ' . openpos . g:tagbar_width . 'split ' . '__Tagbar__'

    let &eventignore = eventignore_save

    call s:InitWindow(autoclose)

    wincmd p

    " Jump back to the tagbar window if autoclose or autofocus is set. Can't
    " just stay in it since it wouldn't trigger the update event
    if g:tagbar_autoclose || autofocus || g:tagbar_autofocus
        let tagbarwinnr = bufwinnr('__Tagbar__')
        execute tagbarwinnr . 'wincmd w'
    endif
endfunction

" s:InitWindow() {{{2
function! s:InitWindow(autoclose)
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
    setlocal nocursorline
    setlocal nocursorcolumn

    if exists('+relativenumber')
        setlocal norelativenumber
    endif

    setlocal nofoldenable
    setlocal foldcolumn=0
    " Reset fold settings in case a plugin set them globally to something
    " expensive. Apparently 'foldexpr' gets executed even if 'foldenable' is
    " off, and then for every appended line (like with :put).
    setlocal foldmethod&
    setlocal foldexpr&

    setlocal statusline=Tagbar

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
            wincmd p
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
" Execute ctags and put the information into a 'FileInfo' object
function! s:ProcessFile(fname, ftype)
    if !s:IsValidFile(a:fname, a:ftype)
        return
    endif

    let ctags_output = s:ExecuteCtagsOnFile(a:fname, a:ftype)

    if ctags_output == -1
        " put an empty entry into known_files so the error message is only
        " shown once
        call s:known_files.put({}, a:fname)
        return
    elseif ctags_output == ''
        return
    endif

    " If the file has only been updated preserve the fold states, otherwise
    " create a new entry
    if s:known_files.has(a:fname)
        let fileinfo = s:known_files.get(a:fname)
        call fileinfo.reset()
    else
        let fileinfo = s:FileInfo.New(a:fname, a:ftype)
    endif

    let typeinfo = s:known_types[a:ftype]

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

        let kindtag          = s:KindheaderTag.New(kind.long)
        let kindtag.short    = kind.short
        let kindtag.numtags  = len(curtags)
        let kindtag.fileinfo = fileinfo

        for tag in curtags
            let tag.parent = kindtag
        endfor
    endfor

    if !empty(processedtags)
        call extend(fileinfo.tags, processedtags)
    endif

    " Clear old folding information from previous file version
    call fileinfo.clearOldFolds()

    " Sort the tags
    let s:compare_typeinfo = typeinfo
    call fileinfo.sortTags()

    call s:known_files.put(fileinfo)
endfunction

" s:ExecuteCtagsOnFile() {{{2
function! s:ExecuteCtagsOnFile(fname, ftype)
    let typeinfo = s:known_types[a:ftype]

    if has_key(typeinfo, 'ctagsargs')
        let ctags_args = ' ' . typeinfo.ctagsargs . ' '
    else
        let ctags_args  = ' -f - '
        let ctags_args .= ' --format=2 '
        let ctags_args .= ' --excmd=pattern '
        let ctags_args .= ' --fields=nksSa '
        let ctags_args .= ' --extra= '
        let ctags_args .= ' --sort=yes '

        " Include extra type definitions
        if has_key(typeinfo, 'deffile')
            let ctags_args .= ' --options=' . typeinfo.deffile . ' '
        endif

        let ctags_type = typeinfo.ctagstype

        let ctags_kinds = ''
        for kind in typeinfo.kinds
            let ctags_kinds .= kind.short
        endfor

        let ctags_args .= ' --language-force=' . ctags_type .
                        \ ' --' . ctags_type . '-kinds=' . ctags_kinds . ' '
    endif

    if has_key(typeinfo, 'ctagsbin')
        " reset 'wildignore' temporarily in case *.exe is included in it
        let wildignore_save = &wildignore
        set wildignore&
        let ctags_bin = expand(typeinfo.ctagsbin)
        let &wildignore = wildignore_save
    else
        let ctags_bin = g:tagbar_ctags_bin
    endif

    let ctags_cmd = s:EscapeCtagsCmd(ctags_bin, ctags_args, a:fname)
    if ctags_cmd == ''
        return ''
    endif

    let ctags_output = s:ExecuteCtags(ctags_cmd)

    if v:shell_error || ctags_output =~ 'Warning: cannot open source file'
        echoerr 'Tagbar: Could not execute ctags for ' . a:fname . '!'
        echomsg 'Executed command: "' . ctags_cmd . '"'
        if !empty(ctags_output)
            echomsg 'Command output:'
            for line in split(ctags_output, '\n')
                echomsg line
            endfor
        endif
        return -1
    endif

    return ctags_output
endfunction

" s:ParseTagline() {{{2
" Structure of a tag line:
" tagname<TAB>filename<TAB>expattern;"fields
" fields: <TAB>name:value
" fields that are always present: kind, line
function! s:ParseTagline(part1, part2, typeinfo, fileinfo)
    let basic_info  = split(a:part1, '\t')

    let taginfo      = s:NormalTag.New(basic_info[0])
    let taginfo.file = basic_info[1]

    " the pattern can contain tabs and thus may have been split up, so join
    " the rest of the items together again
    let pattern = join(basic_info[2:], "\t")
    let start   = 2 " skip the slash and the ^
    let end     = strlen(pattern) - 1
    if pattern[end - 1] ==# '$'
        let end -= 1
        let dollar = '\$'
    else
        let dollar = ''
    endif
    let pattern           = strpart(pattern, start, end - start)
    let taginfo.pattern   = '\V\^\C' . pattern . dollar
    let prototype         = substitute(pattern,   '^[[:space:]]\+', '', '')
    let prototype         = substitute(prototype, '[[:space:]]\+$', '', '')
    let taginfo.prototype = prototype

    let fields = split(a:part2, '\t')
    let taginfo.fields.kind = remove(fields, 0)
    for field in fields
        " can't use split() since the value can contain ':'
        let delimit             = stridx(field, ':')
        let key                 = strpart(field, 0, delimit)
        let val                 = strpart(field, delimit + 1)
        let taginfo.fields[key] = val
    endfor
    " Needed for jsctags
    if has_key(taginfo.fields, 'lineno')
        let taginfo.fields.line = taginfo.fields.lineno
    endif

    " Make some information easier accessible
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

    let taginfo.fileinfo = a:fileinfo

    " Needed for folding
    try
        call taginfo.initFoldState()
    catch /^Vim(\a\+):E716:/ " 'Key not present in Dictionary'
        " The tag has a 'kind' that doesn't exist in the type definition
        echoerr 'Your ctags and Tagbar configurations are out of sync!'
              \ 'Please read '':help tagbar-extend''.'
    endtry

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
        \ (v:val.path ==# curpath ||
         \ match(v:val.path, ''\V\^\C'' . curpath . a:typeinfo.sro) == 0) &&
        \ (v:val.path ==# curpath ? (v:val.scope ==# pscope) : 1)'
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
    let ispseudochild = 'v:val.path ==# a:tag.path && v:val.scope ==# a:tag.scope'
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

    let pseudotag             = s:PseudoTag.New(a:name)
    let pseudotag.fields.kind = a:typeinfo.scope2kind[a:scope]

    let parentscope = substitute(curpath, a:name . '$', '', '')
    let parentscope = substitute(parentscope,
                               \ '\V\^' . a:typeinfo.sro . '\$', '', '')

    if pscope != ''
        let pseudotag.fields[pscope] = parentscope
        let pseudotag.scope    = pscope
        let pseudotag.path     = parentscope
        let pseudotag.fullpath =
                    \ pseudotag.path . a:typeinfo.sro . pseudotag.name
    endif
    let pseudotag.depth = len(split(pseudotag.path, '\V' . a:typeinfo.sro))

    let pseudotag.parent = a:parent

    let pseudotag.fileinfo = a:fileinfo

    call pseudotag.initFoldState()

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

    if typeinfo.kinddict[a:tag1.fields.kind] <#
     \ typeinfo.kinddict[a:tag2.fields.kind]
        return -1
    elseif typeinfo.kinddict[a:tag1.fields.kind] >#
         \ typeinfo.kinddict[a:tag2.fields.kind]
        return 1
    else
        " Ignore '~' prefix for C++ destructors to sort them directly under
        " the constructors
        if a:tag1.name[0] ==# '~'
            let name1 = a:tag1.name[1:]
        else
            let name1 = a:tag1.name
        endif
        if a:tag2.name[0] ==# '~'
            let name2 = a:tag2.name[1:]
        else
            let name2 = a:tag2.name
        endif

        if name1 <=# name2
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
    let fileinfo = s:known_files.getCurrent()
    if empty(fileinfo)
        return
    endif

    let curline = line('.')

    match none

    let s:compare_typeinfo = s:known_types[fileinfo.ftype]

    if has_key(s:compare_typeinfo, 'sort')
        let s:compare_typeinfo.sort = !s:compare_typeinfo.sort
    else
        let g:tagbar_sort = !g:tagbar_sort
    endif

    call fileinfo.sortTags()

    call s:RenderContent()

    execute curline
endfunction

" Display {{{1
" s:RenderContent() {{{2
function! s:RenderContent(...)
    if a:0 == 1
        let fileinfo = a:1
    else
        let fileinfo = s:known_files.getCurrent()
    endif

    if empty(fileinfo)
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

    if !empty(s:known_files.getCurrent()) &&
     \ fileinfo.fpath ==# s:known_files.getCurrent().fpath
        " We're redisplaying the same file, so save the view
        let saveline = line('.')
        let savecol  = col('.')
        let topline  = line('w0')
    endif

    let lazyredraw_save = &lazyredraw
    set lazyredraw
    let eventignore_save = &eventignore
    set eventignore=all

    setlocal modifiable

    silent %delete _

    call s:PrintHelp()

    let typeinfo = s:known_types[fileinfo.ftype]

    " Print tags
    call s:PrintKinds(typeinfo, fileinfo)

    " Delete empty lines at the end of the buffer
    for linenr in range(line('$'), 1, -1)
        if getline(linenr) =~ '^$'
            execute 'silent ' . linenr . 'delete _'
        else
            break
        endif
    endfor

    setlocal nomodifiable

    if !empty(s:known_files.getCurrent()) &&
     \ fileinfo.fpath ==# s:known_files.getCurrent().fpath
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

        " Invalidate highlight cache from old file
        let s:last_highlight_tline = 0
    endif

    let &lazyredraw  = lazyredraw_save
    let &eventignore = eventignore_save

    if !in_tagbar
        execute prevwinnr . 'wincmd w'
    endif
endfunction

" s:PrintKinds() {{{2
function! s:PrintKinds(typeinfo, fileinfo)
    let first_tag = 1

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
                if g:tagbar_compact && first_tag && s:short_help
                    silent 0put =tag.str()
                else
                    silent  put =tag.str()
                endif

                " Save the current tagbar line in the tag for easy
                " highlighting access
                let curline                   = line('.')
                let tag.tline                 = curline
                let a:fileinfo.tline[curline] = tag

                " Print children
                if tag.isFoldable() && !tag.isFolded()
                    for ckind in a:typeinfo.kinds
                        let childtags = filter(copy(tag.children),
                                          \ 'v:val.fields.kind ==# ckind.short')
                        if len(childtags) > 0
                            " Print 'kind' header of following children
                            if !has_key(a:typeinfo.kind2scope, ckind.short)
                                silent put ='    [' . ckind.long . ']'
                                let a:fileinfo.tline[line('.')] = tag
                            endif
                            for childtag in childtags
                                call s:PrintTag(childtag, 1,
                                              \ a:fileinfo, a:typeinfo)
                            endfor
                        endif
                    endfor
                endif

                if !g:tagbar_compact
                    silent put _
                endif

                let first_tag = 0
            endfor
        else
            " Non-scoped tags
            let kindtag = curtags[0].parent

            if kindtag.isFolded()
                let foldmarker = s:icon_closed
            else
                let foldmarker = s:icon_open
            endif

            if g:tagbar_compact && first_tag && s:short_help
                silent 0put =foldmarker . ' ' . kind.long
            else
                silent  put =foldmarker . ' ' . kind.long
            endif

            let curline                   = line('.')
            let kindtag.tline             = curline
            let a:fileinfo.tline[curline] = kindtag

            if !kindtag.isFolded()
                for tag in curtags
                    let str = tag.str()
                    silent put ='  ' . str

                    " Save the current tagbar line in the tag for easy
                    " highlighting access
                    let curline                   = line('.')
                    let tag.tline                 = curline
                    let a:fileinfo.tline[curline] = tag
                    let tag.depth                 = 1
                endfor
            endif

            if !g:tagbar_compact
                silent put _
            endif

            let first_tag = 0
        endif
    endfor
endfunction

" s:PrintTag() {{{2
function! s:PrintTag(tag, depth, fileinfo, typeinfo)
    " Print tag indented according to depth
    silent put =repeat(' ', a:depth * 2) . a:tag.str()

    " Save the current tagbar line in the tag for easy
    " highlighting access
    let curline                   = line('.')
    let a:tag.tline               = curline
    let a:fileinfo.tline[curline] = a:tag

    " Recursively print children
    if a:tag.isFoldable() && !a:tag.isFolded()
        for ckind in a:typeinfo.kinds
            let childtags = filter(copy(a:tag.children),
                                 \ 'v:val.fields.kind ==# ckind.short')
            if len(childtags) > 0
                " Print 'kind' header of following children
                if !has_key(a:typeinfo.kind2scope, ckind.short)
                    silent put ='    ' . repeat(' ', a:depth * 2) .
                              \ '[' . ckind.long . ']'
                    let a:fileinfo.tline[line('.')] = a:tag
                endif
                for childtag in childtags
                    call s:PrintTag(childtag, a:depth + 1,
                                  \ a:fileinfo, a:typeinfo)
                endfor
            endif
        endfor
    endif
endfunction

" s:PrintHelp() {{{2
function! s:PrintHelp()
    if !g:tagbar_compact && s:short_help
        silent 0put ='\" Press <F1> for help'
        silent  put _
    elseif !s:short_help
        silent 0put ='\" Tagbar keybindings'
        silent  put ='\"'
        silent  put ='\" --------- General ---------'
        silent  put ='\" <Enter>   : Jump to tag definition'
        silent  put ='\" <Space>   : Display tag prototype'
        silent  put ='\"'
        silent  put ='\" ---------- Folds ----------'
        silent  put ='\" +, zo     : Open fold'
        silent  put ='\" -, zc     : Close fold'
        silent  put ='\" o, za     : Toggle fold'
        silent  put ='\" *, zR     : Open all folds'
        silent  put ='\" =, zM     : Close all folds'
        silent  put ='\"'
        silent  put ='\" ---------- Misc -----------'
        silent  put ='\" s          : Toggle sort'
        silent  put ='\" x          : Zoom window in/out'
        silent  put ='\" q          : Close window'
        silent  put ='\" <F1>       : Remove help'
        silent  put _
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
    let tagline = 0

    let tag = s:GetNearbyTag()
    if !empty(tag)
        let tagline = tag.tline
    endif

    " Don't highlight the tag again if it's the same one as last time.
    " This prevents the Tagbar window from jumping back after scrolling with
    " the mouse.
    if tagline == s:last_highlight_tline
        return
    else
        let s:last_highlight_tline = tagline
    endif

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

    if g:tagbar_autoshowtag
        call s:OpenParents(tag)
    endif

    " Check whether the tag is inside a closed fold and highlight the parent
    " instead in that case
    let tagline = tag.getClosedParentTline()

    " Go to the line containing the tag
    execute tagline

    " Make sure the tag is visible in the window
    call winline()

    let foldpat = '[' . s:icon_open . s:icon_closed . ' ]'
    let pattern = '/^\%' . tagline . 'l\s*' . foldpat . '[-+# ]\zs[^( ]\+\ze/'
    execute 'match TagbarHighlight ' . pattern

    execute prevwinnr . 'wincmd w'

    let &eventignore = eventignore_save

    redraw
endfunction

" s:JumpToTag() {{{2
function! s:JumpToTag(stay_in_tagbar)
    let taginfo = s:GetTagInfo(line('.'), 1)

    let autoclose = w:autoclose

    if empty(taginfo) || has_key(taginfo, 'numtags')
        return
    endif

    let tagbarwinnr = winnr()

    let eventignore_save = &eventignore
    set eventignore=all

    " This elaborate construct will try to switch to the correct
    " buffer/window; if the buffer isn't currently shown in a window it will
    " open it in the first window with a non-special buffer in it
    wincmd p
    let filebufnr = bufnr(taginfo.fileinfo.fpath)
    if bufnr('%') != filebufnr
        let filewinnr = bufwinnr(filebufnr)
        if filewinnr != -1
            execute filewinnr . 'wincmd w'
        else
            for i in range(1, winnr('$'))
                execute i . 'wincmd w'
                if &buftype == ''
                    execute 'buffer ' . filebufnr
                    break
                endif
            endfor
        endif
        " To make ctrl-w_p work we switch between the Tagbar window and the
        " correct window once
        execute tagbarwinnr . 'wincmd w'
        wincmd p
    endif

    " Mark current position so it can be jumped back to
    mark '

    " Jump to the line where the tag is defined. Don't use the search pattern
    " since it doesn't take the scope into account and thus can fail if tags
    " with the same name are defined in different scopes (e.g. classes)
    execute taginfo.fields.line

    " If the file has been changed but not saved, the tag may not be on the
    " saved line anymore, so search for it in the vicinity of the saved line
    if match(getline('.'), taginfo.pattern) == -1
        let interval = 1
        let forward  = 1
        while search(taginfo.pattern, 'W' . forward ? '' : 'b') == 0
            if !forward
                if interval > line('$')
                    break
                else
                    let interval = interval * 2
                endif
            endif
            let forward = !forward
        endwhile
    endif

    " If the tag is on a different line after unsaved changes update the tag
    " and file infos/objects
    let curline = line('.')
    if taginfo.fields.line != curline
        let taginfo.fields.line = curline
        let taginfo.fileinfo.fline[curline] = taginfo
    endif

    " Center the tag in the window
    normal! z.

    if foldclosed('.') != -1
        .foldopen!
    endif

    redraw

    let &eventignore = eventignore_save

    if a:stay_in_tagbar
        call s:HighlightTag()
        execute tagbarwinnr . 'wincmd w'
    elseif g:tagbar_autoclose || autoclose
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

    echo taginfo.getPrototype()
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

" s:GotoNextToplevelTag() {{{2
function! s:GotoNextToplevelTag(direction)
    let curlinenr = line('.')
    let newlinenr = line('.')

    if a:direction == 1
        let range = range(line('.') + 1, line('$'))
    else
        let range = range(line('.') - 1, 1, -1)
    endif

    for tmplinenr in range
        let taginfo = s:GetTagInfo(tmplinenr, 0)

        if empty(taginfo)
            continue
        elseif empty(taginfo.parent)
            let newlinenr = tmplinenr
            break
        endif
    endfor

    if curlinenr != newlinenr
        execute newlinenr
        call winline()
    endif

    redraw
endfunction

" Folding {{{1
" s:OpenFold() {{{2
function! s:OpenFold()
    let fileinfo = s:known_files.getCurrent()
    if empty(fileinfo)
        return
    endif

    let curline = line('.')

    let tag = s:GetTagInfo(curline, 0)
    if empty(tag)
        return
    endif

    call tag.openFold()

    call s:RenderKeepView()
endfunction

" s:CloseFold() {{{2
function! s:CloseFold()
    let fileinfo = s:known_files.getCurrent()
    if empty(fileinfo)
        return
    endif

    match none

    let curline = line('.')

    let curtag = s:GetTagInfo(curline, 0)
    if empty(curtag)
        return
    endif

    let newline = curtag.closeFold()

    call s:RenderKeepView(newline)
endfunction

" s:ToggleFold() {{{2
function! s:ToggleFold()
    let fileinfo = s:known_files.getCurrent()
    if empty(fileinfo)
        return
    endif

    match none

    let curtag = s:GetTagInfo(line('.'), 0)
    if empty(curtag)
        return
    endif

    let newline = line('.')

    if curtag.isKindheader()
        call curtag.toggleFold()
    elseif curtag.isFoldable()
        if curtag.isFolded()
            call curtag.openFold()
        else
            let newline = curtag.closeFold()
        endif
    else
        let newline = curtag.closeFold()
    endif

    call s:RenderKeepView(newline)
endfunction

" s:SetFoldLevel() {{{2
function! s:SetFoldLevel(level)
    if a:level < 0
        echoerr 'Foldlevel can''t be negative'
        return
    endif

    let fileinfo = s:known_files.getCurrent()
    if empty(fileinfo)
        return
    endif

    call s:SetFoldLevelRecursive(fileinfo, fileinfo.tags, a:level)

    let typeinfo = s:known_types[fileinfo.ftype]

    " Apply foldlevel to 'kind's
    if a:level == 0
        for kind in typeinfo.kinds
            call fileinfo.closeKindFold(kind)
        endfor
    else
        for kind in typeinfo.kinds
            call fileinfo.openKindFold(kind)
        endfor
    endif

    let fileinfo.foldlevel = a:level

    call s:RenderContent()
endfunction

" s:SetFoldLevelRecursive() {{{2
" Apply foldlevel to normal tags
function! s:SetFoldLevelRecursive(fileinfo, tags, level)
    for tag in a:tags
        if tag.depth >= a:level
            call tag.setFolded(1)
        else
            call tag.setFolded(0)
        endif

        if has_key(tag, 'children')
            call s:SetFoldLevelRecursive(a:fileinfo, tag.children, a:level)
        endif
    endfor
endfunction

" s:OpenParents() {{{2
function! s:OpenParents(...)
    let tagline = 0

    if a:0 == 1
        let tag = a:1
    else
        let tag = s:GetNearbyTag()
    endif

    call tag.openParents()

    call s:RenderKeepView()
endfunction

" Helper functions {{{1
" s:CleanUp() {{{2
function! s:CleanUp()
    silent autocmd! TagbarAutoCmds

    unlet s:is_maximized
    unlet s:compare_typeinfo
    unlet s:short_help
endfunction

" s:CleanupFileinfo() {{{2
function! s:CleanupFileinfo(fname)
    call s:known_files.rm(a:fname)
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

    " Only consider the main filetype in cases like 'python.django'
    let ftype = get(split(&filetype, '\.'), 0, '')

    " Don't do anything if the file isn't supported
    if !s:IsValidFile(a:fname, ftype)
        return
    endif

    " Process the file if it's unknown or the information is outdated
    " Also test for entries that exist but are empty, which will be the case
    " if there was an error during the ctags execution
    if s:known_files.has(a:fname) && !empty(s:known_files.get(a:fname))
        if s:known_files.get(a:fname).mtime != getftime(a:fname)
            call s:ProcessFile(a:fname, ftype)
        endif
    elseif !s:known_files.has(a:fname)
        call s:ProcessFile(a:fname, ftype)
    endif

    let fileinfo = s:known_files.get(a:fname)

    " If we don't have an entry for the file by now something must have gone
    " wrong, so don't change the tagbar content
    if empty(fileinfo)
        return
    endif

    " Display the tagbar content
    call s:RenderContent(fileinfo)

    " Call setCurrent after rendering so RenderContent can check whether the
    " same file is redisplayed
    if !empty(fileinfo)
        call s:known_files.setCurrent(fileinfo)
    endif

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

" s:EscapeCtagsCmd() {{{2
" Assemble the ctags command line in a way that all problematic characters are
" properly escaped and converted to the system's encoding
" Optional third parameter is a file name to run ctags on
function! s:EscapeCtagsCmd(ctags_bin, args, ...)
    if exists('+shellslash')
        let shellslash_save = &shellslash
        set noshellslash
    endif

    if a:0 == 1
        let fname = shellescape(a:1)
    else
        let fname = ''
    endif

    let ctags_cmd = shellescape(a:ctags_bin) . ' ' . a:args . ' ' . fname

    if exists('+shellslash')
        let &shellslash = shellslash_save
    endif

    " Needed for cases where 'encoding' is different from the system's
    " encoding
    if g:tagbar_systemenc != &encoding
        let ctags_cmd = iconv(ctags_cmd, &encoding, g:tagbar_systemenc)
    elseif $LANG != ''
        let ctags_cmd = iconv(ctags_cmd, &encoding, $LANG)
    endif

    if ctags_cmd == ''
        echoerr 'Tagbar: Encoding conversion failed!'
              \ 'Please make sure your system is set up correctly'
              \ 'and that Vim is compiled with the "iconv" feature.'
    endif

    return ctags_cmd
endfunction

" s:ExecuteCtags() {{{2
" Execute ctags with necessary shell settings
" Partially based on the discussion at
" http://vim.1045645.n5.nabble.com/bad-default-shellxquote-in-Widows-td1208284.html
function! s:ExecuteCtags(ctags_cmd)
    if exists('+shellslash')
        let shellslash_save = &shellslash
        set noshellslash
    endif

    if &shell =~ 'cmd\.exe'
        let shellxquote_save = &shellxquote
        set shellxquote=\"
        let shellcmdflag_save = &shellcmdflag
        set shellcmdflag=/s\ /c
    endif

    let ctags_output = system(a:ctags_cmd)

    if &shell =~ 'cmd\.exe'
        let &shellxquote  = shellxquote_save
        let &shellcmdflag = shellcmdflag_save
    endif

    if exists('+shellslash')
        let &shellslash = shellslash_save
    endif

    return ctags_output
endfunction

" s:GetTagInfo() {{{2
" Return the info dictionary of the tag on the specified line. If the line
" does not contain a valid tag (for example because it is empty or only
" contains a pseudo-tag) return an empty dictionary.
function! s:GetTagInfo(linenr, ignorepseudo)
    let fileinfo = s:known_files.getCurrent()

    if empty(fileinfo)
        return {}
    endif

    " Don't do anything in empty and comment lines
    let curline = getline(a:linenr)
    if curline =~ '^\s*$' || curline[0] == '"'
        return {}
    endif

    " Check if there is a tag on the current line
    if !has_key(fileinfo.tline, a:linenr)
        return {}
    endif

    let taginfo = fileinfo.tline[a:linenr]

    " Check if the current tag is not a pseudo-tag
    if a:ignorepseudo && taginfo.isPseudoTag()
        return {}
    endif

    return taginfo
endfunction

" s:GetNearbyTag() {{{2
" Get the tag info for a file near the cursor in the current file
function! s:GetNearbyTag()
    let fileinfo = s:known_files.getCurrent()

    let curline = line('.')
    let tag = {}

    " If a tag appears in a file more than once (for example namespaces in
    " C++) only one of them has a 'tline' entry and can thus be highlighted.
    " The only way to solve this would be to go over the whole tag list again,
    " making everything slower. Since this should be a rare occurence and
    " highlighting isn't /that/ important ignore it for now.
    for line in range(curline, 1, -1)
        if has_key(fileinfo.fline, line)
            let tag = fileinfo.fline[line]
            break
        endif
    endfor

    return tag
endfunction

" s:CheckMouseClick() {{{2
function! s:CheckMouseClick()
    let line   = getline('.')
    let curcol = col('.')

    if (match(line, s:icon_open . '[-+ ]') + 1) == curcol
        call s:CloseFold()
    elseif (match(line, s:icon_closed . '[-+ ]') + 1) == curcol
        call s:OpenFold()
    elseif g:tagbar_singleclick
        call s:JumpToTag(0)
    endif
endfunction

" TagbarBalloonExpr() {{{2
function! TagbarBalloonExpr()
    let taginfo = s:GetTagInfo(v:beval_lnum, 1)

    if empty(taginfo)
        return
    endif

    return taginfo.getPrototype()
endfunction

" TagbarGenerateStatusline() {{{2
function! TagbarGenerateStatusline()
    if g:tagbar_sort
        let text = '[Name]'
    else
        let text = '[Order]'
    endif

    if !empty(s:known_files.getCurrent())
        let filename = fnamemodify(s:known_files.getCurrent().fpath, ':t')
        let text .= ' ' . filename
    endif

    return text
endfunction

" Autoload functions {{{1
function! tagbar#ToggleWindow()
    call s:ToggleWindow()
endfunction

function! tagbar#OpenWindow(...)
    let flags = a:0 > 0 ? a:1 : ''
    call s:OpenWindow(flags)
endfunction

function! tagbar#CloseWindow()
    call s:CloseWindow()
endfunction

function! tagbar#SetFoldLevel(...)
    call s:SetFoldLevel(a:1)
endfunction

function! tagbar#OpenParents()
    call s:OpenParents()
endfunction

function! tagbar#RestoreSession()
    call s:RestoreSession()
endfunction

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
