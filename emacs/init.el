(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e45f6f1a61b7eb23a400b5a184e1adc87b35ff3db6c668d953655828e30de8a5" "b1a691bb67bd8bd85b76998caf2386c9a7b2ac98a116534071364ed6489b695d" default))
 '(package-selected-packages))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package gruvbox-theme)

(setq inhibit-startup-screen t)
(menu-bar-mode nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'gruvbox)
(set-frame-font (cond ((eq system-type 'darwin) "FiraMono Nerd Font Mono 14")
		      ((eq system-type 'windows-nt) "FuraMono Nerd Font Mono 11"))
		nil t)

(ido-mode) ; find-file completion

;; required to access certain binaries, e.g. rust-analyzer
(use-package exec-path-from-shell :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; (use-package flycheck)

(use-package magit :ensure t)

(use-package which-key :ensure t)
(which-key-mode)

;; (use-package yasnippet)
(use-package company)
;; (use-package lsp-ui)
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)
;;   :hook (lsp-mode . lsp-enable-which-key-integration))
;; (use-package projectile)
;; (use-package slime)

;; Go
(use-package go-mode
  :ensure t
  :hook (yas-minor-mode)
  :config
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports))

;; Rust
(use-package rustic
  :ensure t
  :hook (lsp-deferred yas-minor-mode)
  :config
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports))

;; ;; Slime
;; (setq inferior-lisp-program "sbcl")
