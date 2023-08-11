(local lsp-key-maps
  {"<leader>"
   {:d {:name "Diagnostics"
  		:d [ vim.diagnostic.open_float "Open float" ]
  		:l [ vim.diagnostic.setloclist "List" ]
  		:n [ vim.diagnostic.goto_next "Next" ]
  		:p [ vim.diagnostic.goto_prev "Previous" ]}
  	:l {:name "LSP"
  		"<C-k>" [ vim.lsp.buf.signature_help "Signature help" ]
  		:a [ vim.lsp.buf.code_action "Code action" ]
  		:f [ vim.lsp.buf.format "Format buffer" ]
  		:g {:name "Go to"
  			:d [ vim.lsp.buf.definition "Definition" ]
  			:D [ vim.lsp.buf.declaration "Declaration" ]
  			:i [ vim.lsp.buf.implementation "Implementation" ]
  			:r [ vim.lsp.buf.references "References" ]
  			:t [ vim.lsp.buf.type_definition "Type definition" ]}
  		:k [ vim.lsp.buf.hover "Documentation" ]
  		:r [ vim.lsp.buf.rename "Rename" ]
  		:w {:name "Workspace"
  			:a [ vim.lsp.buf.add_workspace_folder "Add folder" ]
  			:r [ vim.lsp.buf.remove_workspace_folder "Remove folder" ]}}}
  :K [ vim.lsp.buf.hover "Documentation" ]})

(fn lsp-on-attach [_ bufnr]
  (let [wk (require :which-key)
        augroup (vim.api.nvim_create_augroup "LspFormatting" {})]
    (vim.api.nvim_buf_set_option bufnr "omnifunc" "v:lua.vim.lsp.omnifunc")
    (vim.api.nvim_clear_autocmds {:group augroup :buffer bufnr })
    (vim.api.nvim_create_autocmd "BufWritePre" 
                                 {:group augroup
                                 :buffer  bufnr
                                 :callback  vim.lsp.buf.format})
    (wk.register lsp-key-maps { :noremap true :silent true :buffer bufnr })))

(let [lsp (require :lspconfig)
      cmp (require :cmp_nvim_lsp)
      flags { :debounce_text_changes 150 }
      capabilities ((. cmp :default_capabilities)
                    (vim.lsp.protocol.make_client_capabilities))]
  (lsp.fennel_language_server.setup
    {:on_attach lsp-on-attach
     :flags flags
     :capabilities capabilities
     :root_dir (lsp.util.root_pattern "fnl")
     :settings {:fennel
                {:workspace
                 ;{:library (vim.api.nvim_get_runtime_file "" true)}
                 {:library (vim.api.nvim_list_runtime_paths)}
                 :diagnostics
                 {:globals [ "vim" ]}}}})
  (lsp.gopls.setup
    {:on_attach lsp-on-attach
     :flags flags
     :capabilities capabilities}))
