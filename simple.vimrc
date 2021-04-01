" Basic vim config for using https://github.com/Gauteab/talon-tree-sitter-service

" Setup Plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Plugins
call plug#begin('~/.vim/plugged')
	Plug 'sheerun/vim-polyglot' " Syntax Highlighting for many languages
    Plug 'vim-airline/vim-airline'
	Plug 'dkasak/gruvbox' " theme
call plug#end()

syntax on
filetype on
filetype plugin indent on
set ignorecase
set smartcase
set wildmenu
set hidden
set incsearch
set backspace=indent,eol,start
set noswapfile " Disbale auto backup
set scrolloff=8 " Scroll offset
set mouse=a " Make the mouse work
set clipboard=unnamed " vim and os use same clipboard
colo gruvbox
set background=dark
set tabstop=4 
set softtabstop=4
set shiftwidth=4
set expandtab
set nowrap
set number relativenumber
set autoread

" Make searching nicer
augroup vimrc-incsearch-highlight
  autocmd!
  autocmd CmdlineEnter /,\? :set hlsearch
  autocmd CmdlineLeave /,\? :set nohlsearch
augroup END

" === TALON ===
source ~/.talon/user/talon-tree-sitter-service/talon.vimrc

