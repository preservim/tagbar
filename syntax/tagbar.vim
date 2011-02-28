" File:        tagbar.vim
" Description: Tagbar syntax settings
" Author:      Jan Larres <jan@majutsushi.net>
" Licence:     Vim licence
" Website:     http://majutsushi.github.com/tagbar/
" Version:     1.2

if exists("b:current_syntax")
  finish
endif

syntax match Comment    '^" .*'             " Comments
syntax match Identifier '^ [^: ]\+[^:]\+$'  " Non-scoped kinds
syntax match Title      '[^(* ]\+\ze\*\? :' " Scope names
syntax match Type       ' : \zs.*'          " Scope types
syntax match SpecialKey '(.*)'              " Signatures
syntax match NonText    '\*\ze :'           " Pseudo-tag identifiers

highlight default TagbarAccessPublic    guifg=Green ctermfg=Green
highlight default TagbarAccessProtected guifg=Blue  ctermfg=Blue
highlight default TagbarAccessPrivate   guifg=Red   ctermfg=Red

syntax match TagbarAccessPublic    '^\s*+\ze[^ ]'
syntax match TagbarAccessProtected '^\s*#\ze[^ ]'
syntax match TagbarAccessPrivate   '^\s*-\ze[^ ]'

let b:current_syntax = "tagbar"
