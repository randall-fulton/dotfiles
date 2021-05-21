" ftplugin/typescript.vim

if exists('g:coc_global_extensions')
  let g:coc_global_extensions += [ 'coc-tsserver' ]
else
  let g:coc_global_extensions = [ 'coc-tsserver' ]
endif

augroup js
  autocmd!
  autocmd BufWritePre *.js,*.jsx,*.ts,*.tsx Prettier
augroup END
