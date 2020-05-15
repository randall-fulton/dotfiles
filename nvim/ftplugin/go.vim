" code generation
let g:go_addtags_transform = "snakecase"
let g:go_fmt_command = "goimports"
let g:go_snippet_engine = "neosnippet"

" indent
au FileType go set noexpandtab
au FileType go set shiftwidth=4
au FileType go set softtabstop=4
au FileType go set tabstop=4

" navigation
au FileType go nmap <F12> <Plug>(go-def)

" testing
au Filetype go nmap <leader>ga <Plug>(go-alternate-edit)
au Filetype go nmap <leader>gah <Plug>(go-alternate-split)
au Filetype go nmap <leader>gav <Plug>(go-alternate-vertical)
au FileType go nmap <F10> :GoTest -short<CR>
au FileType go nmap <F9> :GoCoverageToggle -short<CR>

" highlighting
" let g:go_auto_sameids = 1
let g:go_auto_type_info = 1
let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1
