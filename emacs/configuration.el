(setq custom-file (expand-file-name "configuration.el" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(backup-directory-alist '(("." . ".saves")))
 '(custom-safe-themes
   '("dee623165e2a4760320d7b33ec3715d77a447317e93ba3688325c1fc1b06c2e4" "d395c1793e0d64797d711c870571a0033174ca321ed48444efbe640bf692bf4f" "11873c4fbf465b956889adfa9182495db3bf214d9a70c0f858f07f6cc91cbd47" default))
 '(dired-listing-switches "-al --group-directories-first")
 '(display-buffer-alist
   '(("\\\\*compilation\\\\*" display-buffer-in-side-window
      (side . bottom))
     ("\\\\*eshell\\\\*" display-buffer-in-side-window
      (side . bottom))
     ("\\\\*vterm\\\\*" display-buffer-in-side-window
      (side . bottom))))
 '(display-line-numbers-type 'relative)
 '(explicit-shell-file-name "zsh")
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(prog-mode-hook '(company-mode display-line-numbers-mode))
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(straight-use-package-by-default t)
 '(straight-use-package-mode t)
 '(straight-vc-git-default-protocol 'https)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:font "Fira Code 16" :height 180))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; default values
;;
;; can be overridden in custom.el, which is machine-specific
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rf/source-code-font-family "Fira Code")
(defvar rf/source-code-font-size 16)
(defun rf/source-code-font ()
  (format "%s %d"
	  rf/source-code-font-family
	  rf/source-code-font-size))

(setq-default explicit-shell-file-name "zsh")

;; machine-specific config should be stored in custom.el
(when (file-exists-p "custom.el")
  (load-file "custom.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-frame-font (rf/source-code-font) nil t)

(straight-use-package 'use-package)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

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
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun rf/multi-occur (regexp)
  "Run MULTI-OCCUR in the active buffer."
  (interactive "sSearch regexp: ")
  (multi-occur `(,(current-buffer)) regexp))

(global-set-key (kbd "C-c C-b") 'rf/buffer-map)

(defalias 'rf/buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'rf/multi-occur)
    map)
  "Bindings for current buffer.")

;; (rf/load-org-config "org.org")
;; (rf/load-org-config "org-roam.org")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; packages
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons)

(use-package modus-themes
  :demand t
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-org-blocks 'gray-background)
  :config (load-theme 'modus-operandi)
  :bind (("C-c M-a t t" . modus-themes-toggle)))

(use-package ligature
  :load-path "path-to-ligature-repo"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures '(prog-mode) ; org-mode)
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

(use-package doom-modeline
  :init
  (require 'all-the-icons)
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 50))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize)))

(use-package notmuch-indicator
  :custom
  (notmuch-indicator-refresh-count 60)
  (notmuch-indicator-hide-empty-counters t)
  (notmuch-indicator-args '((:terms "--output threads tag:gh-pr-todo" :label "pr:"))))

(use-package notmuch
  :commands notmuch-hello
  :bind (("C-c m" . notmuch-hello))
  :custom
  (notmuch-search-line-faces '(("gh-pr-todo" . ((t :foreground "#f77")))))
  (notmuch-saved-searches
   '((:name "Inbox" :query "tag:inbox" :sort-order newest-first :key "i")
     (:name "Github[reviews]" :query "tag:gh-pr-todo" :sort-order newest-first :key "p")
     (:name "Newsletters" :query "tag:news-unread" :sort-order newest-first :key "n")
     (:name "Unread" :query "tag:unread" :sort-order newest-first :key "u")))
  :config
  (notmuch-indicator-mode 1))

(use-package magit :ensure t)

(use-package which-key
	:ensure t
	:config
	(which-key-mode))

(use-package multiple-cursors
  :bind (("C-M-n" . 'mc/mark-next-like-this)
         ("C-M-k" . 'mc/skip-to-next-like-this)
         ("C-M-p" . 'mc/unmark-previous-like-this))
  :custom ((mc/always-run-for-all t)))

(defmacro rf/define-key-with-fallback (keymap key def condition)
  "Binds KEY to definition DEF in KEYMAP. Binding is only active
   when CONDITION is true."
  `(define-key ,keymap ,key
     (lambda () (interactive)
	 (if ,condition (progn ,def (print "overridden"))
	   (call-interactively (key-binding ,key))))))

(defun rf/buffer-highlight-todo (&optional buf)
  "Enable highlighting of TODO/NOTE comments in BUF. Will target
   active buffer when BUF is not provided."
  (interactive)
  (or buf (setq buf (current-buffer)))
  (with-current-buffer buf
    (hi-lock-face-phrase-buffer "TODO" 'modus-themes-intense-magenta)
    (hi-lock-face-phrase-buffer "NOTE" 'bold)))

(defun rf/todo-open-list (&optional nlines)
  "Do a `projectile-multi-occur' for all TODOs in project."
  (interactive "P")
  (let ((project (projectile-acquire-root)))
    (multi-occur (projectile-project-buffers project)
		   "TODO"
		   nlines)))

(unless (eq system-type 'windows-nt)
  (use-package vterm
    :custom ((vterm-shell "zsh"))))

(use-package ivy
	:ensure t
	:config
	(ivy-mode))

(use-package company
  :hook (prog-mode . company-mode))

(use-package external-completion
  :straight (:type git
	       :host github
	       :repo "emacs-straight/external-completion"))

(use-package eglot
  :bind (("C-c l r" . eglot-rename)
	   ("C-c l d" . eglot-find-declaration)
	   ("C-c l i" . eglot-find-implementation)
	   ("C-c l t" . eglot-find-typeDefinition)
	   ("C-c l a" . eglot-code-actions)
	   ("C-c l f" . eglot-format-buffer)
	   ("M-n" . flymake-goto-next-error)
	   ("M-p" . flymake-goto-prev-error))
  :after (company external-completion))

(use-package rg)

(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :init
  (projectile-mode +1)
  (projectile-register-project-type 'go '("go.mod")
				      :project-file "go.mod"
				      :compile "go build"
				      :test "go test"
				      :run "go run ./..."
				      :test-suffix "_test.go"))

(use-package yasnippet-snippets)

(use-package yasnippet
  :bind (:map yas-minor-mode-map
		("M-/" . yas-expand)
		("TAB" . nil))
  :config (yas-reload-all))

(use-package company-ctags)

(defun rf/c-align-macro (begin end)
  (interactive "r")
  (align-regexp
   begin
   end
   "\\(\\s-*\\)\\\\[[:space:]]*$"
   1 1 nil))

(defun rf/setup-c-mode ()
  "Setup c-mode"
  (add-hook 'c-mode-hook #'yas-minor-mode)
  (add-hook 'c-mode-hook #'company-mode)
  (add-hook 'c-mode-hook #'company-ctags-auto-setup)
  (add-hook 'c-mode-hook
	      (lambda ()
		(define-key c-mode-map
		  (kbd "C-c f m")
		  'rf/c-align-macro))))

(rf/setup-c-mode)

(let (executable (executable-find "d2"))
  (use-package d2-mode
    :if executable)
  (use-package ob-d2
    :if executable
    :custom
    (ob-d2-command executable)))

(let ((executable (executable-find "mmdc")))
  (use-package mermaid-mode
    :if executable)
  (use-package ob-mermaid
    :if executable
    :custom
    (ob-mermaid-cli-path executable)))

(use-package dockerfile-mode)

(use-package glsl-mode)

(use-package graphql-mode)

(use-package go-mode
  :bind (("C-c C-c C-c" . tester-run-current-test)
         ("C-c C-c f" . gofmt))
  :config
  ;; (add-hook 'go-mode-hook #'lsp-deferred)
  ;; (add-hook 'before-save-hook #'lsp-format-buffer)
  ;; (add-hook 'before-save-hook #'lsp-organize-imports)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'go-mode-hook #'eglot-ensure)
  :custom
  (gofmt-command "goimports"))

(use-package ob-go
  :straight (ob-go
             :type git
             :host github
             :repo "pope/ob-go"))

(use-package go-dlv)

(use-package haskell-mode
	:config
	;; (add-hook 'haskell-mode-hook #'lsp-deferred)
	(add-hook 'haskell-mode-hook #'flycheck-mode)
	:init
	;; (use-package lsp-haskell)
	(use-package hindent))

(use-package parinfer-rust-mode
  :hook (emacs-lisp-mode lisp-mode geiser-mode))

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl --dynamic-space-size 4096")
  (setq browse-url-handlers
        '(("hyperspec" . eww-browse-url)
          ("." . browse-url-default-browser))))

(use-package geiser)

(use-package geiser-guile)

(defun rf/disable-indent-tabs-for-lisp (mode-hooks)
  "Disable indent-tabs-mode for all MODE-HOOKS."
  (dolist (hook mode-hooks)
    (add-hook hook #'(lambda() (indent-tabs-mode -1)))))
(rf/disable-indent-tabs-for-lisp '(emacs-lisp-mode-hook lisp-mode-hook))

(use-package nix-mode)

(use-package protobuf-mode)

;; (use-package lsp-pyright
;;   :hook (python-mode . (lambda ()
;; 			 (require 'lsp-pyright)
;; 			 (lsp))))

(use-package python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(add-hook 'ruby-mode-hook 'eglot-ensure)

(require 'ansi-color)
(defun rf/display-ansi-colors-in-buffer (&optional buf)
  "Enable ANSI colors in BUF"
  (interactive)
  (or buf (setq buf (current-buffer)))
  (let ((inhibit-read-only t))
    (with-current-buffer buf
	(ansi-color-apply-on-region (point-min) (point-max)))))

(use-package rustic
  :custom
  (rustic-lsp-client 'eglot)
  :config
  (add-hook 'rust-mode-hook #'eglot-ensure)
  (add-hook 'rust-mode-hook #'yas-minor-mode)
  (add-hook 'compilation-finish-functions
            #'(lambda (buf &rest ignored)
                (rf/display-ansi-colors-in-buffer buf))))

(use-package typescript-mode)

(use-package yaml-mode)
