set tabstop=8
set shiftwidth=8
set softtabstop=8
set noexpandtab
set textwidth=80
set cindent
set cinoptions=:0,l1,t0,g0
syntax on

set number


filetype plugin indent on


call plug#begin()

" List your plugins here
Plug 'tpope/vim-sensible'
Plug 'vbe0201/vimdiscord'
Plug 'rust-lang/rust.vim'


call plug#end()
