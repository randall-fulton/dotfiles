(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-protocol 'ssh)
(setq straight-vc-git-force-protocol t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defun rf/configure-org ()
  (variable-pitch-mode)
  (visual-line-mode)
  (setq org-pretty-entities t
		org-hide-leading-star t
		org-hide-emphasis-markers t
		org-startup-indented t)
  (let* ((variable-tuple
		  (cond ((x-list-fonts "ETBembo")      '(:font "ETBembo"))
				((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
				(nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
		 (base-font-color     (face-foreground 'default nil 'default))
		 (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
	(custom-theme-set-faces
	 'user
	 `(variable-pitch ((t (,@variable-tuple :height 180 :weight thin))))
	 '(fixed-pitch ((t (:font "Fira Code" :height 120))))
	 `(org-level-8 ((t (,@headline ,@variable-tuple))))
	 `(org-level-7 ((t (,@headline ,@variable-tuple))))
	 `(org-level-6 ((t (,@headline ,@variable-tuple))))
	 `(org-level-5 ((t (,@headline ,@variable-tuple))))
	 `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
	 `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
	 `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
	 `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
	 `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))
	 '(org-code ((t (:inherit (shadow fixed-pitch)))))
	 '(org-block ((t (:inherit fixed-pitch))))
	 '(org-code ((t (:inherit (shadow fixed-pitch)))))
	 '(org-document-info ((t (:foreground "dark orange"))))
	 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
	 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
	 '(org-link ((t (:foreground "royal blue" :underline t))))
	 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
	 '(org-property-value ((t (:inherit fixed-pitch))) t)
	 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
	 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
	 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
	 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))))
(add-hook #'org-mode-hook #'rf/configure-org)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq default-directory
	  (cond ((equal system-type 'windows-nt) "G:\\Developer")
			(t "~/dev")))

(setq inhibit-startup-screen t)
(setq initial-buffer-choice default-directory)

(setq-default tab-width 4)

(set-frame-font (cond ((eq system-type 'darwin) "FiraMono Nerd Font Mono 16")
                      ((eq system-type 'gnu/linux) "Fira Code 12")
                      ((eq system-type 'windows-nt) "Fira Code 12"))
                nil t)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-light-medium))

(use-package ligature
  :load-path "path-to-ligature-repo"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures '(prog-mode org-mode)
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(setq display-line-numbers-type 'relative)
(add-hook #'prog-mode-hook #'display-line-numbers-mode)

(setq-default explicit-shell-file-name "/usr/bin/zsh")

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))

(global-unset-key (kbd "C-x o"))
(global-set-key (kbd "C-,")
				(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-.")
				(lambda () (interactive) (other-window 1)))

(defun rf/config-edit ()
  "Open configuration file"
  (interactive)
  (find-file (expand-file-name "configuration.org" user-emacs-directory)))
(global-set-key (kbd "C-c c") '("config-edit" . rf/config-edit))

(use-package magit
  :ensure t)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package ivy
  :ensure t
  :config
  (ivy-mode))

(use-package company)

(use-package lsp-ui
  :bind (("C-c o" . lsp-ui-imenu))
  :custom
  (lsp-ui-imenu-window-fix-width t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-delay 1)
  (lsp-ui-doc-position 'top))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy"))
;; (use-package yasnippet)

(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path (list default-directory))
  (projectile-mode +1))

(use-package flycheck
  :bind
  (("M-p" . flycheck-previous-error)
   ("M-n" . flycheck-next-error)))

(use-package dockerfile-mode)

(use-package go-mode
  :hook (yas-minor-mode)
  :bind (("C-c C-c C-c" . tester-run-current-test))
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports))
(use-package ob-go
  :straight (ob-go
             :type git
             :host github
             :repo "pope/ob-go"))

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook #'lsp-deferred)
  (add-hook 'haskell-mode-hook #'flycheck-mode)
  :init
  (use-package lsp-haskell)
  (use-package hindent))

(use-package parinfer-rust-mode
  :hook (elisp-mode lisp-mode)
  :init
  (setq parinfer-rust-auto-download t))
(use-package slime
  :init
  (setq inferior-lisp-program "sbcl --dynamic-space-size 4096")
  (setq browse-url-handlers
		'(("hyperspec" . eww-browse-url)
		  ("." . browse-url-default-browser))))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

(use-package python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package odin-mode
  :straight (odin-mode :type git :host github :repo "randall-fulton/odin-mode"))

(use-package rustic
  :hook (lsp-deferred yas-minor-mode) ; lsp-rust-analyzer-inlay-hints-mode
  :init
  ;; (setq lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports)
  (push 'rustic-clippy flycheck-checkers))

(use-package yaml-mode)
