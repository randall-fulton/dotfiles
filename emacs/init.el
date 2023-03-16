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
   '("3e374bb5eb46eb59dbd92578cae54b16de138bc2e8a31a2451bf6fdb0f3fd81b" "e45f6f1a61b7eb23a400b5a184e1adc87b35ff3db6c668d953655828e30de8a5" "b1a691bb67bd8bd85b76998caf2386c9a7b2ac98a116534071364ed6489b695d" default))
 '(package-selected-packages nil)
 '(safe-local-variable-values
   '((lsp-go-env quote
				 ((GOFLAGS . "-tags=unit,integration")))
	 (projectile-project-compilation-cmd . "go vet -tags=unit,integration ./...")
	 (lsp-go-env quote
				 ((GOFLAGS . "-tags=unit")))
	 (lsp-pyright-venv-path . "./venv")
	 (flycheck-golangci-lint-config . "~/dev/controller/.golangci.yml"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:font "Fira Code 12" :height 120))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#3c3836" :height 2.0 :underline nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#3c3836" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#3c3836" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#3c3836" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#3c3836" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#3c3836"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#3c3836"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#3c3836"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#3c3836"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:height 180 :weight thin)))))
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

;; refresh config from git
(defun rf/config-update-from-git ()
  "Pull latest config"
  (interactive)
  (when (require 'magit nil 'noerror)
    (let ((origin (pwd)))
      (cd (cond ((equal window-system 'w32) "g:/Developer/dotfiles")
                (t "~/dev/dotfiles")))
      (magit-pull-from-upstream nil)
      (cd origin))))

;; (rf/config-update-from-git)

(straight-use-package 'org)
(load-file (expand-file-name "configuration.el" user-emacs-directory))


