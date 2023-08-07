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
   '("d395c1793e0d64797d711c870571a0033174ca321ed48444efbe640bf692bf4f" "11873c4fbf465b956889adfa9182495db3bf214d9a70c0f858f07f6cc91cbd47" default))
 '(display-buffer-alist
   '(("\\\\*compilation\\\\*" display-buffer-in-side-window
      (side . bottom))
     ("\\\\*eshell\\\\*" display-buffer-in-side-window
      (side . bottom))
     ("\\\\*vterm\\\\*" display-buffer-in-side-window
      (side . bottom)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:font "Fira Code 16" :height 180))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch))))))

(straight-use-package 'org)

(defun rf/load-org-config (file)
  "Load configuration in Org file FILE. Tangle first, if needed."
  (let ((config-src  (expand-file-name file user-emacs-directory))
	(config-dest (expand-file-name (file-name-with-extension file "el")
				       user-emacs-directory)))
    (when (not (file-exists-p config-dest))
      (require 'ob-tangle)
      (org-babel-tangle-file config-src config-dest))
    (load-file config-dest)))

;; All machine-specific config should be stored in custom.el
(when (file-exists-p "custom.el")
  (load-file "custom.el"))

(rf/load-org-config "configuration.org")
