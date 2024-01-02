(setq treesit-language-source-alist
 '((go "https://github.com/tree-sitter/tree-sitter-go")
   (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
   (json "https://github.com/tree-sitter/tree-sitter-json")
   (make "https://github.com/alemuller/tree-sitter-make")
   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
   (python "https://github.com/tree-sitter/tree-sitter-python")
   (toml "https://github.com/tree-sitter/tree-sitter-toml")
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun rf/treesitter-update-langs ()
  "Install all treesitter grammers in `treesit-language-source-alist'.

This should not be run on startup, as it doesn't prevent
reinstall/recompile of grammars that already exist."
  (interactive)
  (mapc #'treesit-install-language-grammar
	(mapcar #'car treesit-language-source-alist)))

(use-package go-ts-mode)

(use-package markdown-mode)
