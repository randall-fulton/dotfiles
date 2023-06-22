;; Uses Bernt Hansen's guide as a foundation.
;; http://doc.norang.ca/org-mode.html

(setq org-agenda-files '("~/dev/org"))

;; keybinds
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c c") 'org-capture)

;; task states
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("NEXT" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)
        ("MEETING" :foreground "forest green" :weight bold)))
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t))
        ("HOLD" ("WAITING" . t) ("HOLD" . t))
        ("TODO" ("WAITING") ("HOLD") ("CANCELLED"))
        ("NEXT" ("WAITING") ("HOLD") ("CANCELLED"))
        ("DONE" ("WAITING") ("HOLD") ("CANCELLED"))))

;; capture
(setq org-default-notes-file "~/dev/org/refile.org")
(setq org-capture-templates
      '(("t" "todo" entry (file org-default-notes-file)
         "* TODO %?\n%U\n%a\n"
         :clock-in t :clock-resume t)
        ("n" "note" entry (file org-default-notes-file)
         "* %? :NOTE:\n%U\n%a\n"
         :clock-in t :clock-resume t)
        ("m" "meeting" entry (file org-default-notes-file)
         "* MEETING %? :MEETING:\n%U"
         :clock-in t :clock-resume t)
        ("r" "respond" entry (file org-default-notes-file)
         "* NEXT Respond to %? on \nSCHEDULED: %t\n%U\n%a\n"
         :clock-in t :clock-resume t)
        ("j" "journal" entry (file+datetree "~/dev/org/diary.org")
         "* %?\n%U\n"
         :clock-in t :clock-resume t)))

(setq org-refile-targets
      '((nil :maxlevel . 9)
        (org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-completion-use-ido t)
(setq org-indirect-buffer-display 'current-window)

(defun rf/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'rf/verify-refile-target)

(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode 'both)
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

;; agenda
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("N" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels t)))
        (" " "Agenda"
         ((agenda "" nil)
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'rf/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-HOLD-CANCELLED/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'rf/skip-non-projects)
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANCELLED/!NEXT"
                     ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                            (if rf/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'rf/skip-projects-and-habits-and-single-tasks)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled rf/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines rf/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date rf/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                     ((org-agenda-overriding-header (concat "Project Subtasks"
                                                            (if rf/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'rf/skip-non-project-tasks)
                      (org-agenda-todo-ignore-scheduled rf/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines rf/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date rf/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                     ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                            (if rf/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'rf/skip-project-tasks)
                      (org-agenda-todo-ignore-scheduled rf/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines rf/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date rf/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANCELLED+WAITING|HOLD/!"
                     ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                            (if rf/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'rf/skip-non-tasks)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled rf/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines rf/hide-scheduled-and-waiting-next-tasks))))
          ;; (tags "-REFILE/"
          ;;       ((org-agenda-overriding-header "Tasks to Archive")
          ;;        (org-agenda-skip-function 'rf/skip-non-archivable-tasks)
          ;;        (org-tags-match-list-sublevels nil))))
         nil)))

;; helpers
(defun rf/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun rf/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun rf/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (rf/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun rf/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun rf/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun rf/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun rf/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar rf/hide-scheduled-and-waiting-next-tasks t)

(defun rf/toggle-next-task-display ()
  (interactive)
  (setq rf/hide-scheduled-and-waiting-next-tasks (not rf/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if rf/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun rf/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (rf/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun rf/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (rf/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (rf/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun rf/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (rf/list-sublevels-for-projects-indented)
  (if (save-excursion (rf/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((rf/is-project-p)
            nil)
           ((and (rf/is-project-subtree-p) (not (rf/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun rf/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((rf/is-task-p)
        nil)
       (t
        next-headline)))))

(defun rf/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((rf/is-project-p)
        subtree-end)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       (t
        nil)))))

(defun rf/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ;; ((org-is-habit-p)
       ;;  next-headline)
       ((and rf/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((rf/is-project-p)
        next-headline)
       ((and (rf/is-task-p) (not (rf/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun rf/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((rf/is-project-p)
        next-headline)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       ((and (not limit-to-project)
             (rf/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (rf/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun rf/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((rf/is-project-p)
        subtree-end)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       ((rf/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun rf/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((rf/is-project-p)
        next-headline)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       ((and (rf/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (rf/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun rf/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((rf/is-project-p)
        subtree-end)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       (t
        nil)))))

(defun rf/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (rf/is-subproject-p)
        nil
      next-headline)))     
