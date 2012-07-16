" File:        tagbar.vim
" Description: Tagbar syntax settings
" Author:      Jan Larres <jan@majutsushi.net>
" Licence:     Vim licence
" Website:     http://majutsushi.github.com/tagbar/
" Version:     2.4.1

scriptencoding utf-8

if exists("b:current_syntax")
  finish
endif

let s:ic = g:tagbar_iconchars[0]
if s:ic =~ '[]^\\-]'
    let s:ic = '\' . s:ic
endif
let s:io = g:tagbar_iconchars[1]
if s:io =~ '[]^\\-]'
    let s:io = '\' . s:io
endif

let s:pattern = '\([' . s:ic . s:io . '] \)\@<=[^-+: ]\+[^:]\+$'
execute "syntax match TagbarKind '" . s:pattern . "'"

let s:pattern = '\([' . s:ic . s:io . '][-+# ]\)\@<=[^*]\+\(\*\?\(([^)]\+)\)\? :\)\@='
execute "syntax match TagbarScope '" . s:pattern . "'"

let s:pattern = '[' . s:ic . s:io . ']\([-+# ]\)\@='
execute "syntax match TagbarFoldIcon '" . s:pattern . "'"

let s:pattern = '\([' . s:ic . s:io . ' ]\)\@<=+\([^-+# ]\)\@='
execute "syntax match TagbarAccessPublic '" . s:pattern . "'"
let s:pattern = '\([' . s:ic . s:io . ' ]\)\@<=#\([^-+# ]\)\@='
execute "syntax match TagbarAccessProtected '" . s:pattern . "'"
let s:pattern = '\([' . s:ic . s:io . ' ]\)\@<=-\([^-+# ]\)\@='
execute "syntax match TagbarAccessPrivate '" . s:pattern . "'"

unlet s:pattern

syntax match TagbarNestedKind '^\s\+\[[^]]\+\]$'
syntax match TagbarComment    '^".*'
syntax match TagbarType       ' : \zs.*'
syntax match TagbarSignature  '(.*)'
syntax match TagbarPseudoID   '\*\ze :'

highlight default link TagbarComment    Comment
highlight default link TagbarKind       Identifier
highlight default link TagbarNestedKind TagbarKind
highlight default link TagbarScope      Title
highlight default link TagbarType       Type
highlight default link TagbarSignature  SpecialKey
highlight default link TagbarPseudoID   NonText
highlight default link TagbarFoldIcon   Statement
highlight default link TagbarHighlight  Search

highlight default TagbarAccessPublic    guifg=Green ctermfg=Green
highlight default TagbarAccessProtected guifg=Blue  ctermfg=Blue
highlight default TagbarAccessPrivate   guifg=Red   ctermfg=Red

let b:current_syntax = "tagbar"

" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
