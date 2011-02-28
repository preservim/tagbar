" ============================================================================
" File:        tagbar.vim
" Description: List the current file's tags in a sidebar, ordered by class etc
" Author:      Jan Larres <jan@majutsushi.net>
" Licence:     Vim licence
" Website:     http://majutsushi.github.com/tagbar/
" Version:     1.2
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

if !exists('g:tagbar_sort')
    let g:tagbar_sort = 1
endif

if !exists('g:tagbar_compact')
    let g:tagbar_compact = 0
endif

if !exists('g:tagbar_expand')
    let g:tagbar_expand = 0
endif

let s:type_init_done    = 0
let s:key_mapping_done  = 0
let s:autocommands_done = 0
let s:window_expanded   = 0

" s:InitTypes() {{{2
function! s:InitTypes()
    " Dictionary of the already processed files, indexed by file name with
    " complete path.
    " The entries are again dictionaries with the following fields:
    " - mtime: File modification time
    " - ftype: The vim file type
    " - tags:  List of the tags that are present in the file, sorted
    "          according to the value of 'g:tagbar_sort'
    " - fline: Dictionary of the tags, indexed by line number in the file
    " - tline: Dictionary of the tags, indexed by line number in the tagbar
    let s:known_files = {}

    let s:known_types = {}

    " Ant {{{3
    let type_ant = {}
    let type_ant.ctagstype = 'ant'
    let type_ant.kinds     = [
        \ 'p:projects',
        \ 't:targets'
    \ ]
    let s:known_types.ant = type_ant
    " Asm {{{3
    let type_asm = {}
    let type_asm.ctagstype = 'asm'
    let type_asm.kinds     = [
        \ 'm:macros',
        \ 't:types',
        \ 'd:defines',
        \ 'l:labels'
    \ ]
    let s:known_types.asm = type_asm
    " ASP {{{3
    let type_aspvbs = {}
    let type_aspvbs.ctagstype = 'asp'
    let type_aspvbs.kinds     = [
        \ 'd:constants',
        \ 'c:classes',
        \ 'f:functions',
        \ 's:subroutines',
        \ 'v:variables'
    \ ]
    let s:known_types.aspvbs = type_aspvbs
    " Awk {{{3
    let type_awk = {}
    let type_awk.ctagstype = 'awk'
    let type_awk.kinds     = [
        \ 'f:functions'
    \ ]
    let s:known_types.awk = type_awk
    " Basic {{{3
    let type_basic = {}
    let type_basic.ctagstype = 'basic'
    let type_basic.kinds     = [
        \ 'c:constants',
        \ 'g:enumerations',
        \ 'f:functions',
        \ 'l:labels',
        \ 't:types',
        \ 'v:variables'
    \ ]
    let s:known_types.basic = type_basic
    " BETA {{{3
    let type_beta = {}
    let type_beta.ctagstype = 'beta'
    let type_beta.kinds     = [
        \ 'f:fragments',
        \ 's:slots',
        \ 'v:patterns'
    \ ]
    let s:known_types.beta = type_beta
    " C {{{3
    let type_c = {}
    let type_c.ctagstype = 'c'
    let type_c.scopes    = ['enum', 'struct', 'union']
    let type_c.sro       = '::'
    let type_c.kinds     = [
        \ 'd:macros',
        \ 'p:prototypes',
        \ 'g:enums',
        \ 'e:enumerators',
        \ 't:typedefs',
        \ 's:structs',
        \ 'u:unions',
        \ 'm:members',
        \ 'v:variables',
        \ 'f:functions'
    \ ]
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
    let type_cpp.scopes    = [
        \ 'namespace',
        \ 'class',
        \ 'struct',
        \ 'enum',
        \ 'union'
    \ ]
    let type_cpp.sro       = '::'
    let type_cpp.kinds     = [
        \ 'd:macros',
        \ 'p:prototypes',
        \ 'g:enums',
        \ 'e:enumerators',
        \ 't:typedefs',
        \ 'n:namespaces',
        \ 'c:classes',
        \ 's:structs',
        \ 'u:unions',
        \ 'f:functions',
        \ 'm:members',
        \ 'v:variables'
    \ ]
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
    let type_cs.scopes    = [
        \ 'namespace',
        \ 'interface',
        \ 'class',
        \ 'struct',
        \ 'enum'
    \ ]
    let type_cs.sro       = '.'
    let type_cs.kinds     = [
        \ 'd:macros',
        \ 'f:fields',
        \ 'g:enums',
        \ 'e:enumerators',
        \ 't:typedefs',
        \ 'n:namespaces',
        \ 'i:interfaces',
        \ 'c:classes',
        \ 's:structs',
        \ 'E:events',
        \ 'm:methods',
        \ 'p:properties'
    \ ]
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
        \ 'd:data items',
        \ 'f:file descriptions',
        \ 'g:group items',
        \ 'p:paragraphs',
        \ 'P:program ids',
        \ 's:sections'
    \ ]
    let s:known_types.cobol = type_cobol
    " DOS Batch {{{3
    let type_dosbatch = {}
    let type_dosbatch.ctagstype = 'dosbatch'
    let type_dosbatch.kinds     = [
        \ 'l:labels',
        \ 'v:variables'
    \ ]
    let s:known_types.dosbatch = type_dosbatch
    " Eiffel {{{3
    let type_eiffel = {}
    let type_eiffel.ctagstype = 'eiffel'
    let type_eiffel.scopes    = ['class', 'feature']
    let type_eiffel.sro       = '.' " Not sure, is nesting even possible?
    let type_eiffel.kinds     = [
        \ 'c:classes',
        \ 'f:features'
    \ ]
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
    let type_erlang.scopes    = ['module']
    let type_erlang.sro       = '.' " Not sure, is nesting even possible?
    let type_erlang.kinds     = [
        \ 'm:modules',
        \ 'd:macro definitions',
        \ 'f:functions',
        \ 'r:record definitions'
    \ ]
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
    let type_mxml.scopes    = ['class']
    let type_mxml.sro       = '.'
    let type_mxml.kinds     = [
        \ 'v:global variables',
        \ 'c:classes',
        \ 'm:methods',
        \ 'p:properties',
        \ 'f:functions',
        \ 'x:mxtags'
    \ ]
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
    let type_fortran.scopes    = ['module', 'program', 'function', 'subroutine']
    let type_fortran.sro       = '.' " Not sure, is nesting even possible?
    let type_fortran.kinds     = [
        \ 'm:modules',
        \ 'p:programs',
        \ 'k:components',
        \ 't:derived types and structures',
        \ 'c:common blocks',
        \ 'b:block data',
        \ 'e:entry points',
        \ 'f:functions',
        \ 's:subroutines',
        \ 'l:labels',
        \ 'n:namelists',
        \ 'v:variables'
    \ ]
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
        \ 'f:JavaScript funtions',
        \ 'a:named anchors'
    \ ]
    let s:known_types.html = type_html
    " Java {{{3
    let type_java = {}
    let type_java.ctagstype = 'java'
    let type_java.scopes    = ['enum', 'interface', 'class']
    let type_java.sro       = '.'
    let type_java.kinds     = [
        \ 'p:packages',
        \ 'f:fields',
        \ 'g:enum types',
        \ 'e:enum constants',
        \ 'i:interfaces',
        \ 'c:classes',
        \ 'm:methods'
    \ ]
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
        \ 'v:global variables',
        \ 'c:classes',
        \ 'p:properties',
        \ 'm:methods',
        \ 'f:functions'
    \ ]
    let s:known_types.javascript = type_javascript
    " Lisp {{{3
    let type_lisp = {}
    let type_lisp.ctagstype = 'lisp'
    let type_lisp.kinds     = [
        \ 'f:functions'
    \ ]
    let s:known_types.lisp = type_lisp
    " Lua {{{3
    let type_lua = {}
    let type_lua.ctagstype = 'lua'
    let type_lua.kinds     = [
        \ 'f:functions'
    \ ]
    let s:known_types.lua = type_lua
    " Make {{{3
    let type_make = {}
    let type_make.ctagstype = 'make'
    let type_make.kinds     = [
        \ 'm:macros'
    \ ]
    let s:known_types.make = type_make
    " Matlab {{{3
    let type_matlab = {}
    let type_matlab.ctagstype = 'matlab'
    let type_matlab.kinds     = [
        \ 'f:functions'
    \ ]
    let s:known_types.matlab = type_matlab
    " Ocaml {{{3
    let type_ocaml = {}
    let type_ocaml.ctagstype = 'ocaml'
    let type_ocaml.scopes    = ['Module', 'class', 'type']
    let type_ocaml.sro       = '.' " Not sure, is nesting even possible?
    let type_ocaml.kinds     = [
        \ 'M:modules or functors',
        \ 'v:global variables',
        \ 'c:classes',
        \ 'C:constructors',
        \ 'm:methods',
        \ 'e:exceptions',
        \ 't:type names',
        \ 'f:functions',
        \ 'r:structure fields'
    \ ]
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
        \ 'f:functions',
        \ 'p:procedures'
    \ ]
    let s:known_types.pascal = type_pascal
    " Perl {{{3
    let type_perl = {}
    let type_perl.ctagstype = 'perl'
    let type_perl.kinds     = [
        \ 'p:packages',
        \ 'c:constants',
        \ 'f:formats',
        \ 'l:labels',
        \ 's:subroutines'
    \ ]
    let s:known_types.perl = type_perl
    " PHP {{{3
    let type_php = {}
    let type_php.ctagstype = 'php'
    let type_php.kinds     = [
        \ 'i:interfaces',
        \ 'c:classes',
        \ 'd:constant definitions',
        \ 'f:functions',
        \ 'v:variables',
        \ 'j:javascript functions'
    \ ]
    let s:known_types.php = type_php
    " Python {{{3
    let type_python = {}
    let type_python.ctagstype = 'python'
    let type_python.scopes    = ['class', 'function']
    let type_python.sro       = '.'
    let type_python.kinds     = [
        \ 'i:imports',
        \ 'c:classes',
        \ 'f:functions',
        \ 'm:members',
        \ 'v:variables'
    \ ]
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
        \ 's:subroutines'
    \ ]
    let s:known_types.rexx = type_rexx
    " Ruby {{{3
    let type_ruby = {}
    let type_ruby.ctagstype = 'ruby'
    let type_ruby.scopes    = ['class']
    let type_ruby.sro       = '.'
    let type_ruby.kinds     = [
        \ 'm:modules',
        \ 'c:classes',
        \ 'f:methods',
        \ 'F:singleton methods'
    \ ]
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
        \ 'f:functions',
        \ 's:sets'
    \ ]
    let s:known_types.scheme = type_scheme
    " Shell script {{{3
    let type_sh = {}
    let type_sh.ctagstype = 'sh'
    let type_sh.kinds     = [
        \ 'f:functions'
    \ ]
    let s:known_types.sh = type_sh
    let s:known_types.csh = type_sh
    let s:known_types.zsh = type_sh
    " SLang {{{3
    let type_slang = {}
    let type_slang.ctagstype = 'slang'
    let type_slang.kinds     = [
        \ 'n:namespaces',
        \ 'f:functions'
    \ ]
    let s:known_types.slang = type_slang
    " SML {{{3
    let type_sml = {}
    let type_sml.ctagstype = 'sml'
    let type_sml.kinds     = [
        \ 'e:exception declarations',
        \ 'f:function definitions',
        \ 'c:functor definitions',
        \ 's:signature declarations',
        \ 'r:structure declarations',
        \ 't:type definitions',
        \ 'v:value bindings'
    \ ]
    let s:known_types.sml = type_sml
    " SQL {{{3
    " The SQL ctags parser seems to be buggy for me, so this just uses the
    " normal kinds even though scopes should be available. Improvements
    " welcome!
    let type_sql = {}
    let type_sql.ctagstype = 'sql'
    let type_sql.kinds     = [
        \ 'c:cursors',
        \ 'f:functions',
        \ 'F:record fields',
        \ 'L:block label',
        \ 'P:packages',
        \ 'p:procedures',
        \ 's:subtypes',
        \ 't:tables',
        \ 'T:triggers',
        \ 'v:variables',
        \ 'i:indexes',
        \ 'e:events',
        \ 'U:publications',
        \ 'R:services',
        \ 'D:domains',
        \ 'V:views',
        \ 'n:synonyms',
        \ 'x:MobiLink Table Scripts',
        \ 'y:MobiLink Conn Scripts'
    \ ]
    let s:known_types.sql = type_sql
    " Tcl {{{3
    let type_tcl = {}
    let type_tcl.ctagstype = 'tcl'
    let type_tcl.kinds     = [
        \ 'c:classes',
        \ 'm:methods',
        \ 'p:procedures'
    \ ]
    let s:known_types.tcl = type_tcl
    " LaTeX {{{3
    let type_tex = {}
    let type_tex.ctagstype = 'tex'
    let type_tex.kinds     = [
        \ 'p:parts',
        \ 'c:chapters',
        \ 's:sections',
        \ 'u:subsections',
        \ 'b:subsubsections',
        \ 'P:paragraphs',
        \ 'G:subparagraphs',
    \ ]
    let s:known_types.tex = type_tex
    " Vera {{{3
    " Why are variables 'virtual'?
    let type_vera = {}
    let type_vera.ctagstype = 'vera'
    let type_vera.scopes    = ['enum', 'class', 'virtual']
    let type_vera.sro       = '.' " Nesting doesn't seem to be possible
    let type_vera.kinds     = [
        \ 'd:macros',
        \ 'g:enums',
        \ 'T:typedefs',
        \ 'c:classes',
        \ 'e:enumerators',
        \ 'm:members',
        \ 'f:functions',
        \ 't:tasks',
        \ 'v:variables',
        \ 'p:programs'
    \ ]
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
        \ 'c:constants',
        \ 'e:events',
        \ 'f:functions',
        \ 'm:modules',
        \ 'n:net data types',
        \ 'p:ports',
        \ 'r:register data types',
        \ 't:tasks'
    \ ]
    let s:known_types.verilog = type_verilog
    " VHDL {{{3
    " The VHDL ctags parser unfortunately doesn't generate proper scopes
    let type_vhdl = {}
    let type_vhdl.ctagstype = 'vhdl'
    let type_vhdl.kinds     = [
        \ 'c:constants',
        \ 't:types',
        \ 'T:subtypes',
        \ 'r:records',
        \ 'e:entities',
        \ 'f:functions',
        \ 'p:procedures',
        \ 'P:packages'
    \ ]
    let s:known_types.vhdl = type_vhdl
    " Vim {{{3
    let type_vim = {}
    let type_vim.ctagstype = 'vim'
    let type_vim.kinds     = [
        \ 'v:variables',
        \ 'f:functions',
        \ 'a:autocommand groups',
        \ 'c:commands',
        \ 'm:maps'
    \ ]
    let s:known_types.vim = type_vim
    " YACC {{{3
    let type_yacc = {}
    let type_yacc.ctagstype = 'yacc'
    let type_yacc.kinds     = [
        \ 'l:labels'
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
            let type.kinddict[kind[0]] = i
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
    for def in values(defdict)
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
    nnoremap <script> <silent> <buffer> <Space> :call <SID>ShowPrototype()<CR>

    nnoremap <script> <silent> <buffer> +           :silent! foldopen<CR>
    nnoremap <script> <silent> <buffer> <kPlus>     :silent! foldopen<CR>
    nnoremap <script> <silent> <buffer> -           :silent! foldclose<CR>
    nnoremap <script> <silent> <buffer> <kMinus>    :silent! foldclose<CR>
    nnoremap <script> <silent> <buffer> *           :silent! %foldopen!<CR>
    nnoremap <script> <silent> <buffer> <kMultiply> :silent! %foldopen!<CR>
    nnoremap <script> <silent> <buffer> =           :silent! %foldclose!<CR>

    nnoremap <script> <silent> <buffer> s    :call <SID>ToggleSort()<CR>
    nnoremap <script> <silent> <buffer> x    :call <SID>ZoomWindow()<CR>
    nnoremap <script> <silent> <buffer> q    :close<CR>
    nnoremap <script> <silent> <buffer> <F1> :call <SID>ToggleHelp()<CR>

    let s:key_mapping_done = 1
endfunction

" s:CreateAutocommands() {{{2
function! s:CreateAutocommands()
    augroup TagbarAutoCmds
        autocmd!
        autocmd BufEnter   __Tagbar__ nested call s:QuitIfOnlyWindow()
        autocmd BufUnload  __Tagbar__ call s:CleanUp()
        autocmd CursorHold __Tagbar__ call s:ShowPrototype()

        autocmd BufEnter,CursorHold * silent call
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
    if tagbarwinnr != -1 && winnr() != tagbarwinnr
        execute tagbarwinnr . 'wincmd w'
        return
    endif

    " If we're in the tagbar window don't do anything
    if winnr() == tagbarwinnr
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

    setlocal foldenable
    setlocal foldminlines=0
    setlocal foldmethod=expr
    setlocal foldexpr=s:GetFoldLevel(v:lnum)
    setlocal foldlevel=9999
    setlocal foldcolumn=1
    setlocal foldtext=getline(v:foldstart)

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

    if !s:key_mapping_done
        call s:MapKeys()
    endif

    if !s:autocommands_done
        call s:CreateAutocommands()
    endif

    let &cpoptions = cpoptions_save

    execute 'wincmd p'

    " Jump back to the tagbar window if autoclose is set. Can't just stay in
    " it since it wouldn't trigger the update event
    if g:tagbar_autoclose || a:autoclose
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

    let ctags_type = typeinfo.ctagstype

    let ctags_kinds = ""
    for kind in typeinfo.kinds
        let [short, full] = split(kind, ':')
        let ctags_kinds .= short
    endfor

    let ctags_args .= ' --language-force=' . ctags_type .
                    \ ' --' . ctags_type . '-kinds=' . ctags_kinds . ' '

    let ctags_cmd = g:tagbar_ctags_bin . ctags_args . shellescape(a:fname)
    let ctags_output = system(ctags_cmd)

    if v:shell_error
        let msg = 'Tagbar: Could not generate tags for ' . a:fname
        echohl WarningMsg | echomsg msg | echohl None
        if !empty(ctags_output)
            echohl WarningMsg | echomsg ctags_output | echohl None
        endif
        return
    endif

    let fileinfo = {}
    let fileinfo.mtime = getftime(a:fname)

    let rawtaglist = split(ctags_output, '\n\+')

    let fileinfo.ftype = a:ftype
    let fileinfo.tags  = []
    let fileinfo.fline = {}
    let fileinfo.tline = {}

    for line in rawtaglist
        let parts = split(line, ';"')
        if len(parts) == 2 " Is a valid tag line
            let taginfo = s:ParseTagline(parts[0], parts[1], typeinfo)
            let fileinfo.fline[taginfo.fields.line] = taginfo
            call add(fileinfo.tags, taginfo)
        endif
    endfor

    if has_key(typeinfo, 'scopes') && !empty(typeinfo.scopes)
        let scopedtags = []
        let is_scoped = 'has_key(typeinfo.kind2scope, v:val.fields.kind) ||
                       \ has_key(v:val, "scope")'
        let scopedtags += filter(copy(fileinfo.tags), is_scoped)
        call filter(fileinfo.tags, '!(' . is_scoped . ')')

        let processedtags = []
        call s:AddScopedTags(scopedtags, processedtags, '', '', 0, typeinfo)

        if !empty(scopedtags)
            echoerr 'Tagbar: ''scopedtags'' not empty after processing,'
                  \ 'this should never happen!'
                  \ 'Please contact the script maintainer with an example.'
        endif
        call extend(fileinfo.tags, processedtags)
    endif

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
function! s:ParseTagline(part1, part2, typeinfo)
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
    let taginfo.path = ''
    let taginfo.fullpath = taginfo.name
    if has_key(a:typeinfo, 'scopes')
        for scope in a:typeinfo.scopes
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
function! s:AddScopedTags(tags, processedtags, curpath, pscope, depth, typeinfo)
    let is_cur_tag = 'v:val.depth == a:depth'

    if !empty(a:curpath)
        " Check whether the tag is either a direct child at the current depth
        " or at least a proper grandchild with pseudo-tags in between. If it
        " is a direct child also check for matching scope.
        let is_cur_tag .= ' &&
        \ (v:val.path == a:curpath ||
         \ match(v:val.path, ''\V\^\C'' . a:curpath . a:typeinfo.sro) == 0) &&
        \ (v:val.path == a:curpath ? (v:val.scope == a:pscope) : 1)'
    endif

    let curtags = filter(copy(a:tags), is_cur_tag)

    if !empty(curtags)
        call filter(a:tags, '!(' . is_cur_tag . ')')

        let realtags = []
        let pseudotags = []

        while !empty(curtags)
            let tag = remove(curtags, 0)

            if tag.path != a:curpath
                " tag is child of a pseudo-tag, so create a new pseudo-tag and
                " add all its children to it
                let pseudotag = s:ProcessPseudoTag(curtags, tag, a:curpath,
                                                 \ a:pscope, a:typeinfo)

                call add(pseudotags, pseudotag)
            else
                call add(realtags, tag)
            endif
        endwhile

        " Recursively add the children of the tags on the current level
        for tag in realtags
            if !has_key(a:typeinfo.kind2scope, tag.fields.kind)
                continue
            endif

            if !has_key(tag, 'children')
                let tag.children = []
            endif

            let parentscope = a:typeinfo.kind2scope[tag.fields.kind]
            call s:AddScopedTags(a:tags, tag.children, tag.fullpath,
                               \ parentscope, a:depth + 1, a:typeinfo)
        endfor
        call extend(a:processedtags, realtags)

        " Recursively add the children of the tags that are children of the
        " pseudo-tags on the current level
        for tag in pseudotags
            call s:ProcessPseudoChildren(a:tags, tag, a:depth, a:typeinfo)
        endfor
        call extend(a:processedtags, pseudotags)
    endif

    " Now we have to check if there are any pseudo-tags at the current level
    " so we have to check for real tags at a lower level, i.e. grandchildren
    let is_grandchild = 'v:val.depth > a:depth'

    if !empty(a:curpath)
        let is_grandchild .=
        \ ' && match(v:val.path, ''\V\^\C'' . a:curpath . a:typeinfo.sro) == 0'
    endif

    let grandchildren = filter(copy(a:tags), is_grandchild)

    if !empty(grandchildren)
        call s:AddScopedTags(a:tags, a:processedtags, a:curpath,
                           \ a:pscope, a:depth + 1, a:typeinfo)
    endif
endfunction

" s:ProcessPseudoTag() {{{2
function! s:ProcessPseudoTag(curtags, tag, curpath, pscope, typeinfo)
    let pseudoname = substitute(a:tag.path, a:curpath, '', '')
    let pseudoname = substitute(pseudoname, '\V\^' . a:typeinfo.sro, '', '')
    let pseudotag = s:CreatePseudoTag(pseudoname, a:curpath, a:pscope,
                                    \ a:tag.scope, a:typeinfo)
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
function! s:ProcessPseudoChildren(tags, tag, depth, typeinfo)
    for childtag in a:tag.children
        if !has_key(a:typeinfo.kind2scope, childtag.fields.kind)
            continue
        endif

        if !has_key(childtag, 'children')
            let childtag.children = []
        endif

        let parentscope = a:typeinfo.kind2scope[childtag.fields.kind]
        call s:AddScopedTags(a:tags, childtag.children, childtag.fullpath,
                           \ parentscope, a:depth + 1, a:typeinfo)
    endfor

    let is_grandchild = 'v:val.depth > a:depth &&
                       \ match(v:val.path, ''^\C'' . a:tag.fullpath) == 0'
    let grandchildren = filter(copy(a:tags), is_grandchild)
    if !empty(grandchildren)
        let parentscope = a:typeinfo.kind2scope[a:tag.fields.kind]
        call s:AddScopedTags(a:tags, a:tag.children, a:tag.fullpath,
                           \ parentscope, a:depth + 1, a:typeinfo)
    endif
endfunction

" s:CreatePseudoTag() {{{2
function! s:CreatePseudoTag(name, curpath, pscope, scope, typeinfo)
    let pseudotag             = {}
    let pseudotag.name        = a:name
    let pseudotag.fields      = {}
    let pseudotag.fields.kind = a:typeinfo.scope2kind[a:scope]
    let pseudotag.fields.line = 0

    let parentscope = substitute(a:curpath, a:name . '$', '', '')
    let parentscope = substitute(parentscope,
                               \ '\V\^' . a:typeinfo.sro . '\$', '', '')

    let pseudotag.path     = ''
    let pseudotag.fullpath = pseudotag.name
    if a:pscope != ''
        let pseudotag.fields[a:pscope] = parentscope
        let pseudotag.scope    = a:pscope
        let pseudotag.path     = parentscope
        let pseudotag.fullpath =
                    \ pseudotag.path . a:typeinfo.sro . pseudotag.name
    endif
    let pseudotag.depth = len(split(pseudotag.path, '\V' . a:typeinfo.sro))

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

    call s:RenderContent(s:current_file, fileinfo.ftype)

    execute curline
endfunction

" Display {{{1
" s:RenderContent() {{{2
function! s:RenderContent(fname, ftype)
    let tagbarwinnr = bufwinnr('__Tagbar__')

    if &filetype == 'tagbar'
        let in_tagbar = 1
    else
        let in_tagbar = 0
        let prevwinnr = winnr()
        execute tagbarwinnr . 'wincmd w'
    endif

    let lazyredraw_save = &lazyredraw
    set lazyredraw

    setlocal modifiable

    silent! %delete _

    call s:PrintHelp()

    " If we don't have an entry for the file by now something must have gone
    " wrong
    if !has_key(s:known_files, a:fname)
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
    let fileinfo = s:known_files[a:fname]

    let typeinfo = s:known_types[a:ftype]

    " Print tags
    for kind in typeinfo.kinds
        let curtags = filter(copy(fileinfo.tags),
                           \ 'v:val.fields.kind ==# kind[0]')

        if empty(curtags)
            continue
        endif

        if has_key(typeinfo, 'kind2scope') &&
         \ has_key(typeinfo.kind2scope, kind[0])
            " Scoped tags
            for tag in curtags
                let taginfo = ''

                if tag.fields.line == 0 " Tag is a pseudo-tag
                    let taginfo .= '*'
                endif
                if has_key(tag.fields, 'signature')
                    let taginfo .= tag.fields.signature
                endif
                let taginfo .= ' : ' . typeinfo.kind2scope[kind[0]]

                let prefix = s:GetPrefix(tag)

                if g:tagbar_compact && line('.') == 1
                    silent! 0put =prefix . tag.name . taginfo
                else
                    silent! put =prefix . tag.name . taginfo
                endif

                " Save the current tagbar line in the tag for easy
                " highlighting access
                let curline                 = line('.')
                let tag.tline               = curline
                let fileinfo.tline[curline] = tag

                if has_key(tag, 'children')
                    for childtag in tag.children
                        call s:PrintTag(childtag, 1, fileinfo, typeinfo)
                    endfor
                endif

                if !g:tagbar_compact
                    silent! put _
                endif
            endfor
        else
            " Non-scoped tags
            if g:tagbar_compact && line('.') == 1
                silent! 0put =' ' . strpart(kind, 2)
            else
                silent! put =' ' . strpart(kind, 2)
            endif

            for tag in curtags
                let taginfo = ''

                if has_key(tag.fields, 'signature')
                    let taginfo .= tag.fields.signature
                endif

                let prefix = s:GetPrefix(tag)

                silent! put ='  ' . prefix . tag.name . taginfo

                " Save the current tagbar line in the tag for easy
                " highlighting access
                let curline                 = line('.')
                let tag.tline               = curline
                let fileinfo.tline[curline] = tag
            endfor

            if !g:tagbar_compact
                silent! put _
            endif
        endif
    endfor

    setlocal nomodifiable

    let &lazyredraw = lazyredraw_save

    if !in_tagbar
        execute prevwinnr . 'wincmd w'
    endif
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

    let prefix = s:GetPrefix(a:tag)

    " Print tag indented according to depth
    silent! put =repeat(' ', a:depth * 2) . prefix . a:tag.name . taginfo

    " Save the current tagbar line in the tag for easy
    " highlighting access
    let curline                   = line('.')
    let a:tag.tline               = curline
    let a:fileinfo.tline[curline] = a:tag

    " Recursively print children
    if has_key(a:tag, 'children')
        for childtag in a:tag.children
            call s:PrintTag(childtag, a:depth + 1, a:fileinfo, a:typeinfo)
        endfor
    endif
endfunction

" s:GetPrefix() {{{2
function! s:GetPrefix(tag)
    if has_key(a:tag.fields, 'access') &&
     \ has_key(s:access_symbols, a:tag.fields.access)
        let prefix = s:access_symbols[a:tag.fields.access]
    else
        let prefix = ' '
    endif

    return prefix
endfunction

" s:PrintHelp() {{{2
function! s:PrintHelp()
    if !g:tagbar_compact && s:short_help
        call append(0, '" Press <F1> for help')
    elseif !s:short_help
        call append(0, '" <Enter> : Jump to tag definition')
        call append(1, '" <Space> : Display tag prototype')
        call append(2, '" +       : Open fold')
        call append(3, '" -       : Close fold')
        call append(4, '" *       : Open all folds')
        call append(5, '" =       : Close all folds')
        call append(6, '" s       : Toggle sort')
        call append(7, '" x       : Zoom window in/out')
        call append(8, '" q       : Close window')
        call append(9, '" <F1>    : Remove help')
    endif
endfunction

" s:ToggleHelp() {{{2
function! s:ToggleHelp()
    let s:short_help = !s:short_help

    " Prevent highlighting from being off after adding/removing the help text
    match none

    if s:current_file == ''
        call s:RenderContent(s:current_file, '')
    else
        let fileinfo = s:known_files[s:current_file]
        call s:RenderContent(s:current_file, fileinfo.ftype)
    endif

    execute 1
endfunction

" User actions {{{1
" s:HighlightTag() {{{2
function! s:HighlightTag(fname)
    let fileinfo = s:known_files[a:fname]

    let curline = line('.')

    let tagline = 0

    " If a tag appears in a file more than once (for example namespaces in
    " C++) only one of them has a 'tline' entry and can thus be highlighted.
    " The only way to solve this would be to go over the whole tag list again,
    " making everything slower. Since this should be a rare occurence and
    " highlighting isn't /that/ important ignore it for now.
    for line in range(curline, 1, -1)
        if has_key(fileinfo.fline, line) &&
         \ has_key(fileinfo.fline[line], 'tline')
            let tagline = fileinfo.fline[line].tline
            break
        endif
    endfor

    let eventignore_save = &eventignore
    set eventignore=all

    let tagbarwinnr = bufwinnr('__Tagbar__')
    let prevwinnr   = winnr()
    execute tagbarwinnr . 'wincmd w'

    match none

    if tagline == 0
        execute 1
        call winline()
        execute prevwinnr . 'wincmd w'
        let &eventignore = eventignore_save
        return
    endif

    " Go to the line containing the tag
    execute tagline

    if foldclosed('.') != -1
        .foldopen!
    endif

    " Make sure the tag is visible in the window
    call winline()

    let pattern = '/^\%' . tagline . 'l\s*[-+#]\?\zs[^( ]\+\ze/'
    execute 'match Search ' . pattern

    execute prevwinnr . 'wincmd w'

    let &eventignore = eventignore_save
endfunction

" s:JumpToTag() {{{2
function! s:JumpToTag()
    let taginfo = s:GetTagInfo(line('.'))

    let autoclose = w:autoclose

    if empty(taginfo)
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

    if g:tagbar_autoclose || autoclose
        call s:CloseWindow()
    else
        call s:HighlightTag(s:current_file)
    endif
endfunction

" s:ShowPrototype() {{{2
function! s:ShowPrototype()
    let taginfo = s:GetTagInfo(line('.'))

    if empty(taginfo)
        return
    endif

    echo taginfo.prototype
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
    call s:RenderContent(a:fname, &filetype)

    " If we don't have an entry for the file by now something must have gone
    " wrong
    if !has_key(s:known_files, a:fname)
        return
    endif

    let s:current_file = a:fname

    call s:HighlightTag(a:fname)
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

" s:GetFoldLevel() {{{2
function! s:GetFoldLevel(lnum)
    let curline = getline(a:lnum)

    " Don't fold comments
    if curline[0] == '"'
        return 0
    endif

    let nextline = getline(a:lnum + 1)

    let curindent  = len(matchstr(curline[1:],  '^[ ]*[-+#]\?')) / 2
    let nextindent = len(matchstr(nextline[1:], '^[ ]*[-+#]\?')) / 2

    if curindent < nextindent
        return '>' . (curindent + 1)
    else
        return curindent
    endif
endfunction

" s:GetTagInfo() {{{2
" Return the info dictionary of the tag on the specified line. If the line
" does not contain a valid tag (for example because it is empty or only
" contains a pseudo-tag) return an empty dictionary.
function! s:GetTagInfo(linenr)
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
    if taginfo.fields.line == 0
        return {}
    endif

    return taginfo
endfunction

" TagbarBalloonExpr() {{{2
function! TagbarBalloonExpr()
    let taginfo = s:GetTagInfo(v:beval_lnum)

    if empty(taginfo)
        return
    endif

    return taginfo.prototype
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

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
