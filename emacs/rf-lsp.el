(use-package eglot
  :hook (go-ts-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c C-r" . eglot-rename)
              ("C-." . eglot-code-action-quickfix)))

(use-package company-mode
  :hook (eglot--managed-mode . company-mode))

(use-package flymake
  :hook (go-ts-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package dap-mode
  :init (setq dap-auto-configure-features t)
  :config (require 'dap-dlv-go))
