(use-package eglot
  :hook (go-ts-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c C-r" . eglot-rename)))
