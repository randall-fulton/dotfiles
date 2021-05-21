set nocompatible              			" be iMproved, required
filetype off                  			" required
" set the runtime path to include Vundle and initialize
set rtp+=~/.config/nvim/bundle/Vundle.vim
call vundle#begin('~/.config/nvim/bundle')	" required
Plugin 'VundleVim/Vundle.vim'			" required

if has('nvim')
  Plugin 'neoclide/coc.nvim', {'branch': 'release'}
endif

" Global usability
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'joshdick/onedark.vim'
Plugin '2leks/ayu-vim'
Plugin 'junegunn/fzf'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'vim-airline/vim-airline'

" Javascript/Typescript
Plugin 'leafgarland/typescript-vim' " syntax
Plugin 'maxmellon/vim-jsx-pretty'   " syntax
Plugin 'pangloss/vim-javascript'    " syntax
Plugin 'prettier/vim-prettier'      " formatting

call vundle#end()				      " required
filetype plugin indent on			" required
syntax on

" Editor
let ayucolor='dark'   " light, mirage, dark
colorscheme ayu
let mapleader=','
set cul
set relativenumber
set clipboard=unnamed
set colorcolumn=120
set background=dark
set mouse=a
set noswapfile
set expandtab
set shiftwidth=2
set tabstop=2

set termguicolors
set t_Co=256 " force 256 colors

if (has("termguicolors"))
	let &t_8f = "\<Esc>[38:2:%lu:%lu:%lum"
	let &t_8b = "\<Esc>[48:2:%lu:%lu:%lum"
endif

" Keybinds
nnoremap <C-p> :FZF<CR>
nnoremap <leader>h :bprevious<CR>
nnoremap <leader>l :bnext<CR>
nnoremap <leader>q :exec "bp\|bd #"<CR>
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <silent><expr> <c-space> coc#refresh()

nmap <silent><leader>ac <Plug>(coc-codeaction)
nmap <silent>gd <Plug>(coc-definition)
nmap <silent>gr <Plug>(coc-references)
nmap <silent><leader>r <Plug>(coc-rename)

" Commands
command! Nt NERDTree
command! Config :e ~/.config/nvim/init.vim
command! Source :source ~/.config/nvim/init.vim

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_error_symbol = "✗"
let g:syntastic_quiet_messages = {}
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 1
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_html_checkers = []

" Airline
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#formatter='unique_tail'

" reset $PAGER env var when using :man
" see https://vim.fandom.com/wiki/Using_vim_as_a_man-page_viewer_under_Unix for more info
" let $PAGER=''
