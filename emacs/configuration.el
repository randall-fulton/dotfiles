(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package gruvbox-theme)

(setq inhibit-startup-screen t)
(menu-bar-mode nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'gruvbox)
(set-frame-font (cond ((eq system-type 'darwin) "FiraMono Nerd Font Mono 14")
		      ((eq system-type 'gnu/linux) "FiraMono Nerd Font Mono 12")
		      ((eq system-type 'windows-nt) "FuraMono Nerd Font Mono 11"))
		nil t)
(ido-mode) ; find-file completion

(use-package exec-path-from-shell :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package magit :ensure t)

(use-package which-key :ensure t)
(which-key-mode)

(use-package company)
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)
;;  :hook (lsp-mode . lsp-enable-which-key-integration))
;; (use-package yasnippet)
;; (use-package lsp-ui)

(use-package flycheck)

(use-package go-mode
  :ensure t
  :hook (yas-minor-mode)
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports))

(use-package rustic
  :ensure t
  :hook (lsp-deferred yas-minor-mode)
  :config
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports))
