set nocompatible              			" be iMproved, required
filetype off                  			" required
" set the runtime path to include Vundle and initialize
set rtp+=~/.config/nvim/bundle/Vundle.vim
call vundle#begin('~/.config/nvim/bundle')	" required
Plugin 'VundleVim/Vundle.vim'			" required

if has('nvim')
  Plugin 'neoclide/coc.nvim', {'branch': 'release'}
endif

Plugin 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows' : 'tools\\update-dll-mingw',
\     'cygwin' : 'make -f make_cygwin.mak',
\     'mac' : 'make -f make_mac.mak',
\     'linux' : 'make',
\     'unix' : 'gmake',
\    },
\ }

Plugin 'christoomey/vim-tmux-navigator'
Plugin 'eslint/eslint'
Plugin 'joshdick/onedark.vim'
Plugin 'junegunn/fzf'
Plugin 'mileszs/ack.vim'
Plugin 'Quramy/tsuquyomi'
Plugin 'prettier/vim-prettier'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'sheerun/vim-polyglot'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-airline/vim-airline'

call vundle#end()				" required
filetype plugin indent on			" required
syntax on

" Editor
colorscheme onedark
let mapleader=','
set cul
set relativenumber
set clipboard=unnamed
set colorcolumn=120
set background=dark
set mouse=a
set noswapfile
" set backspace=2 " allows holding backspace
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
"nnoremap <leader>r *Nciw
" nmap <silent>K <Plug>(lcn-hover)
nmap <silent>gd <Plug>(coc-definition)
nmap <silent>gr <Plug>(coc-references)
nmap <silent><leader>r <Plug>(coc-rename)
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <silent><expr> <c-space> coc#refresh()

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

" Ack
if executable('ag')
	let g:ackprg = 'ag --vimgrep'
endif

" Airline
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#formatter='unique_tail'

let g:coc_global_extensions = [ 'coc-tsserver' ]

" reset $PAGER env var when using :man
" see https://vim.fandom.com/wiki/Using_vim_as_a_man-page_viewer_under_Unix for more info
" let $PAGER=''

" Go Test
function! GoTestCurrentFunction()
  let l:cursor_line = line('.')
  let l:cursor_col = col('.')

  let l:line = getline(search('func', 'b'))
  let match = matchlist(l:line, 'Test[^(]*')
  if len(match) > 0
    execute "!go test -v -run ".match[0]." ".expand('%:p:h')
  end

  exec "normal ".l:cursor_line."G"
  exec "normal ".l:cursor_col."|"
endfunction

autocmd FileType go nnoremap <leader>t :call GoTestCurrentFunction()<CR>

