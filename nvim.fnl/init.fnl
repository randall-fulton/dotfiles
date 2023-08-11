(fn build-init []
  (let [{: build} (require :hotpot.api.make)
        ;; by default, Fennel wont perform strict global checking when
        ;; compiling but we can force it to check by providing a list
        ;; of allowed global names, this can catch some additional errors in
        ;; this file.
        allowed-globals (icollect [n _ (pairs _G)] n)
        opts {:verbosity 0 ;; set to 1 (or dont inclued the key) to see messages
              :compiler {:modules {:allowedGlobals allowed-globals}}}]
    ;; just pass back the whole path as is
    (build "~/.config/nvim/init.fnl" opts ".+" #(values $1))))

(let [hotpot (require :hotpot)
      setup hotpot.setup
      build hotpot.api.make.build
      uv vim.loop]
  ;; do some configuration stuff
  (setup {:provide_require_fennel true
          :compiler {:modules {:correlate true}
                     :macros {:env :_COMPILER
                              :compilerEnv _G
                              :allowedGlobals false}}})

  ;; watch this file for changes and auto-rebuild on save
  (let [handle (uv.new_fs_event)
        ;; uv wont accept condensed paths
        path (vim.fn.expand "~/.config/nvim/init.fnl")]
    ;; note the vim.schedule call
    (uv.fs_event_start handle path {} #(vim.schedule build-init))
    ;; close the uv handle when we quit nvim
    (vim.api.nvim_create_autocmd :VimLeavePre {:callback #(uv.close handle)})))

(require :config)
