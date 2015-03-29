set nocompatible              " be iMproved
filetype off                  " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" leader key
let mapleader = ','
 
" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" My bundles here:
"
" original repos on GitHub
Bundle 'tpope/vim-fugitive'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
Bundle 'tpope/vim-rails.git'
"Bundle 'guns/vim-clojure-static'
Bundle 'tpope/vim-fireplace'
Bundle 'derekwyatt/vim-scala'
Bundle 'tpope/vim-sensible'
"Bundle 'ervandew/supertab'
"Bundle 'Townk/vim-autoclose'
Bundle 'altercation/vim-colors-solarized'
Bundle 'wincent/Command-T'
Bundle 'scrooloose/syntastic'

" This does what it says on the tin. It will check your file on open too, not just on save.
" You might not want this, so just leave it out if you don't.
" let g:syntastic_check_on_open=1

Bundle 'jelera/vim-javascript-syntax'
Bundle 'pangloss/vim-javascript'
Bundle 'nathanaelkane/vim-indent-guides'
Bundle 'Raimondi/delimitMate'

let delimitMate_expand_cr = 1

Bundle 'Valloric/YouCompleteMe'

" These are the tweaks I apply to YCM's config, you don't need them but they
" might help.
" " YCM gives you popups and splits by default that some people might not
" like, so these should tidy it up a bit for you.
let g:ycm_add_preview_to_completeopt=0
let g:ycm_confirm_extra_conf=0
set completeopt-=preview"

"set omnifunc=syntaxcomplete#Complete

Bundle 'marijnh/tern_for_vim'
Bundle 'majutsushi/tagbar'

nmap <F8> :TagbarToggle<CR>

Bundle 'troydm/easybuffer.vim'
nmap <F7> :EasyBuffer<CR>

Bundle 'bling/vim-airline'
Bundle 'Yggdroot/indentLine'
nmap <S-F7> :IndentLinesToggle<CR>
let g:indentLine_char = '┊'
let g:indentLine_enabled = 0

Bundle 'groenewege/vim-less'

Bundle 'guns/vim-clojure-static'
Bundle 'kien/rainbow_parentheses.vim'

" vim-scripts repos
Bundle 'L9'
" Bundle 'FuzzyFinder'
Bundle 'UltiSnips'
Bundle 'lout.vim'

Bundle 'mileszs/ack.vim'
nmap <D-F> :Ack<space>

" Bundle 'scrooloose/nerdtree'
" Bundle 'jistr/vim-nerdtree-tabs'
" map <leader>n <plug>NERDTreeTabsToggle<CR>
" let NERDTreeQuitOnOpen = 1

" non-GitHub repos
" Bundle 'git://git.wincent.com/command-t.git'
" Git repos on your local machine (i.e. when working on your own plugin)
" Bundle 'file:///Users/gmarik/path/to/plugin'
" ...

filetype plugin indent on     " required!
syntax on

set backspace=indent,eol,start
" allow backspacing over everything in insert mode

set hidden        " do not close buffers, but hide them
set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
set number        " always show line numbers
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set showmatch     " set show matching parenthesis

" line numbers
set relativenumber
set number

:highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE

" http://stackoverflow.com/questions/1551231/highlight-variable-under-cursor-in-vim-like-in-netbeans
" :autocmd CursorMoved * exe printf('match IncSearch /\V\<%s\>/', escape(expand('<cword>'), '/\'))

 
" searching
set ignorecase smartcase incsearch hlsearch

" Clear search result with ESC
map <Space> :noh<cr>:<backspace>

" Sets how many lines of history VIM has to remember
set history=700

" Set to auto read when a file is changed from the outside
set autoread

" Always show current position
set ruler

" Turn backup off
set nobackup
set nowb
set noswapfile
  
" Use spaces instead of tabs
set expandtab
   
" Be smart when using tabs ;)
set smarttab
    
" 1 tab == 2 spaces
set shiftwidth=2
set tabstop=2

set mouse=a
set cursorline

set pastetoggle=<F2>

" Remap VIM 0 to first non-blank character
map 0 ^

" Open Tag under Cursor
nnoremap ü <C-]>
nnoremap Ü <C-O>

" Quickly edit/reload the vimrc file
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

map <silent> <PageUp> 1000<C-U>
map <silent> <PageDown> 1000<C-D>
imap <silent> <PageUp> <C-O>1000<C-U>
imap <silent> <PageDown> <C-O>1000<C-D>
set nostartofline

hi NonText guifg=bg

" ctags
set tags=tags;/"

if has('gui_running')
  set background=light
  :colorscheme solarized
  if has("macunix")
    set guifont=Menlo\ Regular:h15
  elseif has("unix")
    set guifont=DejaVu\ Sans\ Mono\ 13
  endif
else
  set background=light
endif

