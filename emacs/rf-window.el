(unbind-key (kbd "C-x o"))

(global-set-key [(control ?,)] (lambda () (interactive) (other-window -1)))
(global-set-key [(control ?.)] (lambda () (interactive) (other-window 1)))
