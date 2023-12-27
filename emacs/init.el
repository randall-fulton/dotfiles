(setq custom-file (expand-file-name "rf-custom.el" user-emacs-directory))
(load-file custom-file)

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

(setq package-install-upgrade-built-in t)
(setq straight-use-package-by-default t
      straight-vc-git-default-protocol 'https
      straight-vc-git-force-protocol nil)

(straight-use-package 'use-package)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq inhibit-startup-screen t
      ring-bell-function 'ignore)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq explicit-shell-file-name "/bin/zsh")
(setenv "SHELL" explicit-shell-file-name)

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
	    (exec-path-from-shell-initialize)))

(setq backup-directory-alist `(("." . ".saves"))
      auto-save-default nil)

(load-file (expand-file-name "rf-dired.el" user-emacs-directory))

(load-file (expand-file-name "rf-magit.el" user-emacs-directory))

(load-file (expand-file-name "rf-which-key.el" user-emacs-directory))

(load-file (expand-file-name "rf-theme.el" user-emacs-directory))

(load-file (expand-file-name "rf-langs.el" user-emacs-directory))

(load-file (expand-file-name "rf-lsp.el" user-emacs-directory))

(load-file (expand-file-name "rf-utils.el" user-emacs-directory))
