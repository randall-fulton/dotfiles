(use-package modus-themes
    :init
    (setq modus-themes-bold-constructs t
          modus-themes-org-blocks 'gray-background)
    :config (load-theme 'modus-operandi))
;;    :bind (("C-c M-a t t" . modus-themes-toggle)))

(setq org-pretty-entities t
      org-hide-leading-star t
      org-hide-emphasis-markers t
      org-startup-indented t)

(defun rf/customize-org-faces ()
  (variable-pitch-mode)
  (let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")      '(:font "ETBembo"))
                ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
    (custom-theme-set-faces
     'user
     `(variable-pitch ((t (,@variable-tuple :height 160 :weight thin))))
     `(fixed-pitch ((t (:font "Fira Code" :height 140))))
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple))))
     `(org-level-3 ((t (,@headline ,@variable-tuple))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-document-title ((t (,@headline ,@variable-tuple :underline nil))))
     '(org-code ((t (:inherit (shadow fixed-pitch)))))
     '(org-block ((t (:inherit fixed-pitch))))
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

(add-hook 'org-mode-hook 'rf/customize-org-faces)
