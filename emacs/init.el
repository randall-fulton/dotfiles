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
 '(package-selected-packages
   '(flycheck rustic magit which-key yasnippet company lsp-ui lsp-mode go-mode gruvbox-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(straight-use-package 'flycheck)
(straight-use-package 'rustic)
(straight-use-package 'magit)
(straight-use-package 'which-key)
(straight-use-package 'yasnippet)
(straight-use-package 'company)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-mode)
(straight-use-package 'go-mode)
(straight-use-package 'gruvbox-theme)
(straight-use-package 'projectile)
(straight-use-package 'slime)

(setq inhibit-startup-screen t)
(menu-bar-mode nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'gruvbox)
(set-frame-font (cond ((eq system-type 'darwin) "FiraMono Nerd Font Mono 14")
		      ((eq system-type 'windows-nt) "FuraMono Nerd Font Mono 11"))
		nil t)

(ido-mode) ; find-file completion
(which-key-mode)

(require 'lsp-mode)

;; Go
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Rust
(defun lsp-rust-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'rustic-mode-hook #'lsp-deferred)
(add-hook 'rustic-mode-hook #'yas-minor-mode)
(add-hook 'rustic-mode-hook #'lsp-rust-install-save-hooks)

;; Slime
(setq inferior-lisp-program "sbcl")
