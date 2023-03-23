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

(rf/load-org-config "configuration.org")
