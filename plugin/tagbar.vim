" ============================================================================
" File:        tagbar.vim
" Description: List the current file's tags in a sidebar, ordered by class etc
" Maintainer:  Jan Larres <jan@majutsushi.net>
" Licence:     Vim licence
" Website:     http://github.com/majutsushi/tagbar
" Note:        This plugin was heavily inspired by the 'Taglist' plugin by
"              Yegappan Lakshamanan and uses some small portions of code from
"              it.
" ============================================================================

if &cp || exists('g:loaded_tagbar')
    finish
endif

if !exists('*system')
    echomsg 'Tagbar: No system() function available, skipping plugin'
    finish
endif

if !exists('g:tagbar_ctags_exe')
    if executable('exuberant-ctags')
        let g:tagbar_ctags_exe = 'exuberant-ctags'
    elseif executable('exctags')
        let g:tagbar_ctags_exe = 'exctags'
    elseif executable('ctags')
        let g:tagbar_ctags_exe = 'ctags'
    elseif executable('ctags.exe')
        let g:tagbar_ctags_exe = 'ctags.exe'
    elseif executable('tags')
        let g:tagbar_ctags_exe = 'tags'
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
    let g:tagbar_width = 30
endif

let s:known_files = {}
let s:known_types = {}

function! s:ToggleWindow()
    let tagbarwinnr = bufwinnr("__Tagbar__")
    if tagbarwinnr != -1
        call s:CloseWindow()
        return
    endif

    call s:OpenWindow()
endfunction

function! s:OpenWindow()
    " If the tagbar window is already open jump to it
    let tagbarwinnr = bufwinnr('__Tagbar__')
    if tagbarwinnr != -1 && winnr() != tagbarwinnr
"        execute tagbarwinnr . 'wincmd w'
        return
    endif

    let openpos = g:tagbar_left ? 'topleft vertical ' : 'botright vertical '
    execute 'silent! ' . openpos . g:tagbar_width . 'split ' . '__Tagbar__'

    setlocal noreadonly " in case the "view" mode is used
    setlocal buftype=nofile
    setlocal bufhidden=delete
    setlocal noswapfile
    setlocal nobuflisted
    setlocal nomodifiable
    setlocal filetype=tagbar
    setlocal nolist
    setlocal nonumber
    setlocal norelativenumber
    setlocal nowrap
    setlocal winfixwidth

    setlocal foldenable
    setlocal foldminlines=0
    setlocal foldmethod=manual
    setlocal foldlevel=9999
    setlocal foldcolumn=1
    setlocal foldtext=v:folddashes.getline(v:foldstart)

    let cpoptions_save = &cpoptions
    set cpoptions&vim

"    nnoremap <script> <silent> <buffer> k :call <sid>GundoMove(-1)<CR>

    augroup TagbarAutoCmds
        autocmd!
        autocmd BufEnter  __Tagbar__ nested call s:QuitIfOnlyWindow()
        autocmd BufUnload __Tagbar__ call s:CleanUp()
"        autocmd CursorHold __Tag_List__ call s:Tlist_Window_Show_Info()

        autocmd BufEnter * call s:RefreshContent()
"        autocmd TabEnter * silent call s:Tlist_Refresh_Folds()
"        autocmd CursorHold * silent call s:Tlist_Window_Highlight_Tag(
"                            \ fnamemodify(bufname('%'), ':p'), line('.'), 1, 0)
    augroup END

    let &cpoptions = cpoptions_save

    execute 'wincmd p'
endfunction

function! s:CloseWindow()
    let tagbarwinnr = bufwinnr('__Tagbar__')
    if tagbarwinnr == -1
        return
    endif

    if winnr() == tagbarwinnr
        if winbufnr(2) != -1
            " Other windows are open, only close the tagbar one
            close
        endif
    else
        " Go to the tagbar window, close it and then come back to the
        " original window
        let curbufnr = bufnr('%')
        exe tagbarwinnr . 'wincmd w'
        close
        " Need to jump back to the original window only if we are not
        " already in that window
        let winnum = bufwinnr(curbufnr)
        if winnr() != winnum
            exe winnum . 'wincmd w'
        endif
    endif
endfunction

function! s:CleanUp()
    silent! autocmd! TagbarAutoCmds
endfunction

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

function! s:RefreshContent()
    let fname = fnamemodify(bufname('%'), ':p')

    if !s:IsValidFile(fname, &filetype)
        return
    endif

    let tagbarwinnr = bufwinnr('__Tagbar__')

    if tagbarwinnr == -1
        return
    endif

    if has_key(s:known_files, fname)
        if s:known_files[fname].mtime != getftime(fname)
            call s:ProcessFile(fname, &filetype)
        endif
    else
        call s:ProcessFile(fname, &filetype)
    endif

    call s:RenderContent(fname, &filetype)
endfunction

function! s:IsValidFile(fname, ftype)
    if a:fname == '' || a:ftype == ''
        return 0
    endif

    if !filereadable(a:fname)
        return 0
    endif

"    let var = 's:tlist_def_' . a:ftype . '_settings'
"    if !exists(var)
"        let var = 'g:tlist_' . a:ftype . '_settings'
"        if !exists(var)
"            return 0
"        endif
"    endif

    if a:ftype != 'cpp'
        return 0
    endif

    return 1
endfunction

function! s:ProcessFile(fname, ftype)
    let ctags_args = ' -f - --format=2 --excmd=pattern --fields=nksaz --extra= --sort=yes '

"    let s:tlist_{a:ftype}_ctags_args = '--language-force=' . ctags_ftype .
"                            \ ' --' . ctags_ftype . '-types=' . ctags_flags

    let ctags_cmd = g:tagbar_ctags_exe . ctags_args . shellescape(a:fname)
    let ctags_output = system(ctags_cmd)

    if v:shell_error
        let msg = 'Tagbar: Could not generate tags for ' . a:fname
        call s:PrintWarningMsg(msg)
        if !empty(ctags_output)
            call s:PrintWarningMsg(ctags_output)
        endif
        return
    endif

    let fileinfo = {}
    let fileinfo.mtime = getftime(a:fname)

    let taglist = split(ctags_output, '\n\+')

    let fileinfo.tags = []

    for line in taglist
        let taginfo = s:ParseTagline(line)
        call add(fileinfo.tags, taginfo)
    endfor

    let s:known_files[a:fname] = fileinfo
endfunction

function! s:ParseTagline(line)
    let parts = split(a:line, ';"')

    let taginfo = {}

    let basic_info = split(parts[0], '\t')
    let tagname    = basic_info[0]
    let tagfile    = basic_info[1]
    let tagproto   = basic_info[2]

    let taginfo.name      = tagname
    let taginfo.prototype = tagproto

    let fields = split(parts[1], '\t')
    for field in fields
        let delimit = stridx(field, ':')
        let key = strpart(field, 0, delimit)
        let val = strpart(field, delimit + 1)
        let taginfo[key] = val
    endfor

    return taginfo
endfunction

function! s:RenderContent(fname, ftype)
    let tagbarwinnr = bufwinnr('__Tagbar__')

    execute tagbarwinnr . 'wincmd w'

    setlocal modifiable

    silent! %delete _

    let tags = s:known_files[a:fname].tags
    for tag in tags
        silent! put =tag.name
    endfor

    setlocal nomodifiable

    execute 'wincmd p'
endfunction

function! s:PrintWarningMsg(msg)
    echohl WarningMsg
    echomsg a:msg
    echohl None
endfunction

command! -nargs=0 TagbarToggle call s:ToggleWindow()
command! -nargs=0 TagbarOpen   call s:OpenWindow()
command! -nargs=0 TagbarClose  call s:CloseWindow()

" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=syntax foldcolumn=1
