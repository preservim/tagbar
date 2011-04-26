" File:        tagbar.vim
" Description: Tagbar syntax settings
" Author:      Jan Larres <jan@majutsushi.net>
" Licence:     Vim licence
" Website:     http://majutsushi.github.com/tagbar/
" Version:     2.0.1

if exists("b:current_syntax")
  finish
endif

if has('multi_byte') && has('unix') && &encoding == 'utf-8' &&
 \ (empty(&termencoding) || &termencoding == 'utf-8')
    syntax match TagbarKind  '\([▶▼] \)\@<=[^-+: ]\+[^:]\+$'
    syntax match TagbarScope '\([▶▼][-+# ]\)\@<=[^*]\+\(\*\?\(([^)]\+)\)\? :\)\@='

    syntax match TagbarFoldIcon '[▶▼]\([-+# ]\)\@='

    syntax match TagbarAccessPublic    '\([▶▼ ]\)\@<=+\([^-+# ]\)\@='
    syntax match TagbarAccessProtected '\([▶▼ ]\)\@<=#\([^-+# ]\)\@='
    syntax match TagbarAccessPrivate   '\([▶▼ ]\)\@<=-\([^-+# ]\)\@='
elseif has('multi_byte') && (has('win32') || has('win64')) && g:tagbar_usearrows
    syntax match TagbarKind  '\([▷◢] \)\@<=[^-+: ]\+[^:]\+$'
    syntax match TagbarScope '\([▷◢][-+# ]\)\@<=[^*]\+\(\*\?\(([^)]\+)\)\? :\)\@='

    syntax match TagbarFoldIcon '[▷◢]\([-+# ]\)\@='

    syntax match TagbarAccessPublic    '\([▷◢ ]\)\@<=+\([^-+# ]\)\@='
    syntax match TagbarAccessProtected '\([▷◢ ]\)\@<=#\([^-+# ]\)\@='
    syntax match TagbarAccessPrivate   '\([▷◢ ]\)\@<=-\([^-+# ]\)\@='
else
    syntax match TagbarKind  '\([-+] \)\@<=[^-+: ]\+[^:]\+$'
    syntax match TagbarScope '\([-+][-+# ]\)\@<=[^*]\+\(\*\?\(([^)]\+)\)\? :\)\@='

    syntax match TagbarFoldIcon '[-+]\([-+# ]\)\@='

    syntax match TagbarAccessPublic    '\([-+ ]\)\@<=+\([^-+# ]\)\@='
    syntax match TagbarAccessProtected '\([-+ ]\)\@<=#\([^-+# ]\)\@='
    syntax match TagbarAccessPrivate   '\([-+ ]\)\@<=-\([^-+# ]\)\@='
endif

syntax match TagbarComment   '^".*'
syntax match TagbarType      ' : \zs.*'
syntax match TagbarSignature '(.*)'
syntax match TagbarPseudoID  '\*\ze :'

highlight default link TagbarComment   Comment
highlight default link TagbarKind      Identifier
highlight default link TagbarScope     Title
highlight default link TagbarType      Type
highlight default link TagbarSignature SpecialKey
highlight default link TagbarPseudoID  NonText
highlight default link TagbarFoldIcon  Statement
highlight default link TagbarHighlight Search

highlight default TagbarAccessPublic    guifg=Green ctermfg=Green
highlight default TagbarAccessProtected guifg=Blue  ctermfg=Blue
highlight default TagbarAccessPrivate   guifg=Red   ctermfg=Red

let b:current_syntax = "tagbar"
