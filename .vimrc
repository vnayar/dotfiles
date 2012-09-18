" Do no emulate the limitations of regular 'vi'.
set nocompatible
" Use current indent level for each new line, and guess indent for language.
set autoindent
set smartindent
" Use spaces instead of tabs and make tabs 4 spaces wide.
set expandtab
set tabstop=4
set shiftwidth=4
" Show matching parenthesis as they are typed.
set showmatch
" Get rid of annoying beeps and replace them with a visual bell.
set vb
" Search through text as search is being typed, and highlight matches.
set incsearch
set hls
" Enable us to use the mouse.
set mouse=a
" Display line numbers.
set number

" Looks good on a dark terminal.
colorscheme torte

"
" CTAGS Configuration
"
set path=**
set tags=./tags;${HOME},./.ctags;${HOME}

"
" NERDTree Plugin Configuration
"
map <F2> :NERDTreeToggle<CR>

"
" My Mappings
"
map <F3> :bp<CR>
map <F4> :bn<CR>
map <F5> :tabp<CR>
map <F6> :tabn<CR>
map <F7> :tabnew<CR>

"""
" Java Omnicomplete Settings
"""
if has("autocmd")
    autocmd Filetype java setlocal omnifunc=javacomplete#Complete
    autocmd Filetype java setlocal completefunc=javacomplete#CompleteParamsInfo
endif
