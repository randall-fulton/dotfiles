(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-protocol 'ssh)
(setq straight-vc-git-force-protocol t)

(use-package gruvbox-theme)

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'gruvbox)
(setq-default tab-width 4)
(set-frame-font (cond ((eq system-type 'darwin) "FiraMono Nerd Font Mono 16")
		      ((eq system-type 'gnu/linux) "Iosevka 12")
		      ((eq system-type 'windows-nt) "FuraMono Nerd Font Mono 11"))
		nil t)
;; replaced with ivy below
;; (ido-mode) ; find-file completion

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq-default explicit-shell-file-name "/usr/bin/zsh")

(use-package exec-path-from-shell :ensure t)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

(global-unset-key (kbd "C-x o"))
(global-set-key (kbd "C-,")
				(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-.")
				(lambda () (interactive) (other-window 1)))

(use-package magit :ensure t)

(use-package which-key :ensure t)
(which-key-mode)

(use-package ivy
      :ensure t)
(ivy-mode)

(use-package company)
(use-package lsp-mode
      :ensure t
      :init
      (setq lsp-keymap-prefix "C-c l")
      :commands lsp
      :custom
      (lsp-rust-analyzer-cargo-watch-command "clippy"))
;;  :hook (lsp-mode . lsp-enable-which-key-integration))
;; (use-package yasnippet)
;; (use-package lsp-ui)

(use-package projectile
      :ensure t
      :bind ("s-p" . projectile-command-map)
      :init
      (setq projectile-project-search-path '("~/dev")))
(projectile-mode +1)

      ;; Recommended keymap prefix on Windows/Linux
      ;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package flycheck)

(use-package dockerfile-mode
      :ensure t)

(use-package go-mode
  :ensure t
  :hook (yas-minor-mode)
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports))

(use-package haskell-mode
      :ensure t)

(use-package parinfer-rust-mode
      :ensure t
      :hook (emacs-lisp-mode lisp-mode)
      :init
      (setq parinfer-rust-auto-download t))
(use-package slime
      :ensure t
      :init
      (setq inferior-lisp-program "sbcl --dynamic-space-size 4096")
      (setq browse-url-handlers
		'(("hyperspec" . eww-browse-url)
		      ("." . browse-url-default-browser))))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package odin-mode
  :ensure t
  :straight (odin-mode :type git :host github :repo "randall-fulton/odin-mode"))

(use-package rustic
      :ensure t
      :hook (lsp-deferred yas-minor-mode) ; lsp-rust-analyzer-inlay-hints-mode
      :init
      ;; (setq lsp-rust-analyzer-server-display-inlay-hints t)
      :config
      (add-hook 'before-save-hook #'lsp-format-buffer)
      (add-hook 'before-save-hook #'lsp-organize-imports)
      (push 'rustic-clippy flycheck-checkers))

(use-package yaml-mode
      :ensure t)
