;;; fabb.el --- A babashka tasks porcelain -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Russell Matney
;;
;; Author: Russell Matney <russell.matney@gmail.com>
;; Maintainer: Russell Matney <russell.matney@gmail.com>

;; Keywords: clojure babashka tasks tools
;; Homepage: https://github.com/teknql/fabb

;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A babashka tasks porcelain inside Emacs.
;;
;;  Fabb's initial focus is running babashka tasks in Emacs.
;;
;;  To support this, we find and parse a bb.edn to get a list of task defs.
;;  `fabb-invoke-ivy' can be used to select and run a bb task.

;;; Code:
(require 's)
(require 'parseedn)
(require 'compile)
(require 'ivy)
(require 'evil)
(require 'cl-lib)
;; maybe this is preferred?
;; (eval-when-compile (require 'cl-lib))

(defmacro fabb--comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;;; fabb groups and vars ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup fabb nil
  "Invoke and manage babahska tasks."
  :group 'tools)

;;; misc helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fabb--disable-line-numbers ()
  "Disable line numbers in current buffer.

Pulled from (magit-section-mode) definition."
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1))
  (when (and (fboundp 'display-line-numbers-mode)
             (bound-and-true-p global-display-line-numbers-mode))
    (display-line-numbers-mode -1)))

(defun fabb--directory-for-file (path)
  "Return the directory for PATH."
  (when path
    (directory-file-name (file-name-directory path))))

(defun fabb--find-file-in-project (filename &optional dir)
  "Find and return a full path for the passed FILENAME.

DIR can be passed to determine the directory to search from.
DIR defaults to `default-directory', which depends on the current buffer."
  (let ((directory (cond
                    ((and dir (file-directory-p dir)) dir)
                    (dir (fabb--directory-for-file dir))
                    (t default-directory))))
    (when-let ((project-dir (locate-dominating-file directory filename)))
      (s-concat (file-name-as-directory project-dir) filename))))

(fabb--comment
 (fabb--find-file-in-project "bb.edn")
 (fabb--find-file-in-project "bb.edn" "~/russmatney/clawe")
 (fabb--find-file-in-project "bb.edn" "~/russmatney/clawe/src/user.clj")
 (fabb--find-file-in-project "nope.edn"))

(defun fabb--parse-edn (path)
  "Parse the passed PATH as edn via `parseedn-read'."
  (with-temp-buffer
    (insert-file-contents path)
    (car (parseedn-read))))

;;; parsing task-defs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fabb--raw-task->task-def (task-name raw-task bb-edn-path)
  "Convert a passed TASK-NAME and RAW-TASK into a fabb-task-def.

BB-EDN-PATH is used to set some properties on the task-def.

task-defs are property lists with these keys:
- `:task-name': the key from the bb.edn tasks map. Used to invoke the task via
  `bb <task-name>'.
- `:task-doc': an optional documentation string from the task's `:doc' key.
- `:task-command': the clojure s-expression that will eventually be executed.
- `:task-dir': the directory the bb.edn file was in.
- `:task-dir-name': the name of the parent directory of the bb.edn file.
- `:task-bb-edn-path': the path to this task's bb.edn."
  (let* ((bb-edn-dir (directory-file-name (file-name-directory bb-edn-path)))
         (dir-name (file-name-base bb-edn-dir))
         (is-table (hash-table-p raw-task))
         (doc (when is-table (gethash :doc raw-task)))
         (task-def
          (list :task-name task-name
                :task-bb-edn bb-edn-path
                :task-dir bb-edn-dir
                :task-dir-name dir-name
                ;; :raw-task raw-task
                )))
    (append task-def
            (when doc (list :task-doc doc))
            ;; TODO symbols from the bb.edn appear to be dropped (run 'doctor-tail)
            (list :task-command (if is-table
                                    (gethash :task raw-task)
                                  raw-task)))))

(defun fabb--parse-bb-edn-tasks (path)
  "Parse and return task-defs for tasks in the bb.edn at PATH."
  (let* ((bb-edn-content (fabb--parse-edn path))
         (raw-tasks (gethash :tasks bb-edn-content)))
    ;; TODO skip `:' prefixed task-names :requires, :enter, :init
    (cl-loop for raw-task being each hash-value of raw-tasks
             using (hash-key task-name)
             collect (fabb--raw-task->task-def task-name raw-task path))))

(defun fabb-task-defs (&optional path)
  "Return a list of task-defs for the current context.

PATH can optionally be used to specify a different starting point for
determining the bb.edn."
  (let ((bb-edn-path (fabb--find-file-in-project "bb.edn" path)))
    (fabb--parse-bb-edn-tasks bb-edn-path)))

(fabb--comment
 (fabb-task-defs)
 (fabb-task-defs "~/russmatney/clawe/src/user.clj"))

;;; invoking tasks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fabb-task--buffer-name (task-def)
  "Build the buffer name for TASK-DEF."
  (format "*fabb-%s* [%s]"
          (plist-get task-def :task-name)
          (plist-get task-def :task-dir-name)))

(defun fabb-task--command (task-def)
  "Build the command to invoke for TASK-DEF."
  ;; TODO consider more bb options here (via transient)
  (format "bb %s" (plist-get task-def :task-name)))

(defvar-local fabb--context-task-def nil
  "A local var for fabb-invoke buffers, containing the relevant task-def.")

(defun fabb-invoke-task (task-def &optional window-opt)
  "Invoke the passed TASK-DEF via 'bb <task-name>'.

Accepts an optional WINDOW-OPT that can be 'same-window or 'in-background.

'same-window invokes the task and opens the task buffer in the same window.
'in-background invokes the task in the background, suppressing any popup.

Invoking a task sets a local var: `fabb--context-task-def'."
  (let ((default-directory (plist-get task-def :task-dir))

        ;; overwrite this func for compilation mode
        ;; this is what results in the same buffer being re-used.
        ;; TODO confirm that re-invokations kill exiting processes in this buffer
        (compilation-buffer-name-function
         (lambda (_name-of-mode) (fabb-task--buffer-name task-def)))

        ;; here we set compilation mode's display-buffer behavior
        (display-buffer-alist
         (pcase window-opt
           ('same-window
            '(((lambda (buf al) t) . (display-buffer-same-window))))
           ('in-background
            '(((lambda (buf al) t) . (display-buffer-no-window))))
           (_ ;; default comp display window behavior
            '()))))

    (when-let ((buffer (compile (fabb-task--command task-def))))
      (with-current-buffer buffer
        (fabb-task-mode)
        (setq-local fabb--context-task-def task-def)
        (message (format "%s" fabb--context-task-def))
        (setq buffer-read-only nil)
        (insert (format "%s" fabb--context-task-def))
        (setq buffer-read-only t)))
    (fabb-status-refresh)))

(fabb--comment
 (let ((task
        (cl-first
         (fabb-task-defs))))
   (fabb-invoke-task task 'in-background))
 (let ((task
        (cl-first
         (fabb-task-defs))))
   (fabb-invoke-task task 'same-window)))

;;;###autoload
(defun fabb-task-reinvoke-task-prompt ()
  "Prompt to reinvoke the task for this buffer."
  (interactive)
  (print "fabb-task-reinvoke-prompt called")
  (print fabb--context-task-def)
  (if fabb--context-task-def
      (if (yes-or-no-p "Rerun this task?")
          (fabb-invoke-task fabb--context-task-def 'same-window)
        (message "Hmm, thought better of it, didja?"))
    (message "no context task :(")))

;;;###autoload
(defun fabb-task-reinvoke-task-no-prompt ()
  "Reinvoke the task for this buffer right away."
  (interactive)
  (print "fabb-task-reinvoke no-prompt called")
  (print fabb--context-task-def)
  (if fabb--context-task-def
      (fabb-invoke-task fabb--context-task-def 'same-window)
    (message "no context task :(")))

;;;###autoload
(defun fabb-task-edit-and-reinvoke-task ()
"Reinvoke the task for this buffer right away."
(interactive)
(message "To impl"))

;;;###autoload
(defun fabb-edit-and-invoke-context-task ()
  "Invoke the in-context task."
  (interactive)
  (print "edit and invoking")
  (print fabb--context-task-def)
  (when fabb--context-task-def
    (let ((default-directory (plist-get fabb--context-task-def :task-dir))
          (compilation-buffer-name-function
           (lambda (_name-of-mode) (fabb-task--buffer-name fabb--context-task-def)))
          (display-buffer-alist '((display-buffer-same-window . t))))
      (when-let ((buffer (recompile t)))
        (with-current-buffer buffer
          (fabb-task-minor-mode)
          ;; (setq-local fabb--context-task-def fabb--context-task-def)
          )
        ;; (fabb-display-buffer buffer)
        ))))

;;; fabb-status ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fabb-get-mode-buffer (mode name)
  "Return a buffer with MODE and NAME.

Otherwise return nil.

See `magit-get-mode-buffer' for a more mature version of this."
  (seq-some
   (lambda (b)
     (with-current-buffer b
       (and (eq major-mode mode)
            (string= (buffer-name) name)
            b)))
   (buffer-list)))

(defun fabb-status--buffer-name (&optional path)
  "Return the fabb-status buffer name for the current context, or PATH."
  (when-let ((directory (fabb--directory-for-file (fabb--find-file-in-project "bb.edn" path)) ))
    (format "*fabb-status* [%s]" directory)))

(defun fabb-display-buffer (buffer)
  "Display the passed BUFFER via `display-buffer'.

Should pretty much always open in the same window.

Attempt to re-use fabb-mode derived windows (like Magit does)."
  (let* ((action
          (if (memq (with-current-buffer buffer major-mode)
                    '(fabb-status-mode compilation-mode))
              '(display-buffer-same-window)
            nil))
         (window (display-buffer buffer action)))
    (select-window window)))

(defvar-local fabb-status--context (list)
  "A context for the fabb-status buffer.")

(defun fabb-status--build-context ()
  "Return a property-list of context for the current buffer."
  (let* ((tasks (fabb-task-defs))
         (first-task (car tasks)))
    (list :tasks tasks
          :task-bb-edn (plist-get first-task :task-bb-edn)
          :task-dir (plist-get first-task :task-dir)
          :task-dir-name (plist-get first-task :task-dir-name))))

;;;###autoload
(defun fabb-status (&optional path)
  "Show the status of fabb tasks.

PATH can be any file from which the bb.edn should be found.
It defaults to the current buffer's file."
  (interactive)
  (let* ((status-buffer-name (fabb-status--buffer-name path))
         (existing-buffer (fabb-get-mode-buffer 'fabb-status-mode status-buffer-name))
         (buffer (if existing-buffer existing-buffer
                   (generate-new-buffer status-buffer-name))))
    (unless existing-buffer
      (with-current-buffer buffer
        (fabb-status-mode)
        (setq-local fabb-status--context
                    (fabb-status--build-context))))
    (fabb-display-buffer buffer)
    (fabb-status-refresh buffer)
    buffer))

(defun fabb-task--existing-buffer-for-task (task &optional task-buffer-name)
  "Return a buffer for the passed TASK, if one exists.

Support passing the name directly as TASK-BUFFER-NAME, for cases where we don't
have a contextual task."
  (let ((task-buffer-name (or task-buffer-name (fabb-task--buffer-name task))))
    (fabb-get-mode-buffer 'compilation-mode task-buffer-name)))

;;; fabb-status helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun fabb-status-invoke-task-in-background ()
  "Select the task or buffer at point."
  (interactive)
  (let* ((props (fabb-status--get-text-props))
         (task (plist-get props :task)))
    ;; invoke the task
    (fabb-invoke-task task 'in-background)))

;;;###autoload
(defun fabb-status-invoke-task-and-show-buffer ()
  "Invoke (or re-invoke) the task, and jump to it's buffer."
  (interactive)
  (let* ((props (fabb-status--get-text-props))
         (task (plist-get props :task)))
    ;; invoke the task in this window
    (fabb-invoke-task task 'same-window)))

;;;###autoload
(defun fabb-status-show-task-buffer ()
  "Invoke (or re-invoke) the task, and jump to it's buffer.

If there is no buffer, a prompt is used to determine if the task should be run."
  (interactive)
  (let* ((props (fabb-status--get-text-props))
         (task (plist-get props :task))
         (task-buffer (fabb-task--existing-buffer-for-task task)))
    ;; if task buffer, display it!
    (if task-buffer
        (fabb-display-buffer task-buffer)
      ;; no buffer, prompt to see if we should run
      (if (yes-or-no-p "Run this task right now?")
          ;; invoke the task
          (fabb-invoke-task task 'same-window)
        (message "No existing buffer.")))))

;;; populating fabb-status ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fabb-status--get-text-props ()
  "Get the props for the line at point."
  (let* ((line (thing-at-point 'line)))
    (text-properties-at 0 line)))

(defun fabb-status--set-text-props (line task)
  "Set the TASK as a text prop on the LINE."
  (let ((props (list :task task)))
    (set-text-properties 0 1 props line)))

(defun fabb-status--header-lines ()
  "Return a list of lines to insert as the status buffer header."
  (let* ((ctx fabb-status--context)
         (tasks (plist-get ctx :tasks))
         (task-count (length tasks)))
    (list (format "%d Tasks" task-count)
          "\n")))

(defun fabb-status--task-lines (task)
  "Return a nice representation of the TASK for listing on the status buffer."
  (let* ((doc (plist-get task :task-doc))
         (task-buffer (fabb-task--existing-buffer-for-task task))
         (task-line (if doc
                        (format "\tbb %s: %s" (plist-get task :task-name) doc)
                      (format "\tbb %s" (plist-get task :task-name))))
         (buffer-line
          (when task-buffer
            (format "\t\t%s" (buffer-name task-buffer)))))

    (fabb-status--set-text-props task-line task)
    (when buffer-line
      (fabb-status--set-text-props buffer-line task))
    (if buffer-line
        (list task-line buffer-line)
      (list task-line))))

;;;###autoload
(defun fabb-status-refresh (&optional buffer path)
  "Redraw the fabb-status buffer.

Attempts to find a *fabb-status* buffer to refresh, preferring BUFFER, then
PATH, then check if the current buffer is a *fabb-status* one."
  (when-let ((buffer
              (or buffer
                  (fabb-get-mode-buffer 'fabb-status-mode
                                        (fabb-status--buffer-name path))
                  (when (s-starts-with? "*fabb-status*" (buffer-name (current-buffer)))
                    (current-buffer)))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (thread-last (fabb-status--header-lines)
                   (mapc (lambda (line) (insert line))))
      (insert ?\n)
      (thread-last (plist-get fabb-status--context :tasks)
                   (mapc
                    (lambda (task)
                      (mapc (lambda (l)
                              (insert l)
                              (insert ?\n))
                            (fabb-status--task-lines task)))))
      (setq buffer-read-only t))))

;;; ivy-frontend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fabb-invoke--ivy-history nil
  "History for `fabb-invoke-ivy'.")

(defun fabb-invoke--ivy-targets ()
  "Parse task-defs and map them into ivy targets."
  (thread-last
    (fabb-task-defs)
    (mapcar (lambda (task-def)
              (cons
               (if (plist-get task-def :task-doc)
                   (format "%s: %s"
                           (plist-get task-def :task-name)
                           (plist-get task-def :task-doc))
                 (plist-get task-def :task-name) )
               task-def)))))

(defun fabb-invoke--ivy-action (selection)
  "Invokes the task-def on SELECTION."
  (when-let ((selected-task-def (cdr selection)))
    (fabb-invoke-task selected-task-def 'same-window)))

;;;###autoload (autoload 'fabb-invoke-ivy "fabb" nil t)
(defun fabb-invoke-ivy ()
  "Select and invoke a bb-task via ivy."
  (interactive)
  (ivy-read "Invoke bb task:"
            (fabb-invoke--ivy-targets)
            :require-match t
            :caller 'fabb-invoke-ivy
            :history 'fabb-invoke--ivy-history
            :action 'fabb-invoke--ivy-action))

;;;; cleanup funcs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun fabb-kill-fabb-buffers ()
  "Kill all *fabb* buffers."
  (interactive)
  (thread-last
    (buffer-list)
    (mapcar
     (lambda (b)
       (with-current-buffer b
         (when
             (s-starts-with-p "*fabb" (buffer-name))
           (kill-buffer)))))))

;;; fabb-dispatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO require transient properly?
;;;###autoload (autoload 'fabb-dispatch "fabb" nil t)
(transient-define-prefix fabb-dispatch ()
  "A transient dispatcher for fabb."
  ["Status Tasks"
   [("i" "Select Task with ivy" fabb-invoke-ivy)
    ("r" "Invoke this task" fabb-status-invoke-task-and-show-buffer)
    ("R" "Invoke this task (in background)" fabb-status-invoke-task-in-background)
    ("RET" "Show task buffer" fabb-status-show-task-buffer)]]
  ["Cleanup/Debugging"
   [("x" "Kill all *fabb* buffers" fabb-kill-fabb-buffers)
    ("q" "Close Fabb window" quit-window)]])

;;; major modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fabb-major-mode

(defvar fabb-mode-map
  (let ((map (make-sparse-keymap))) map)
  "Keymap for `fabb-mode'.")

(define-derived-mode fabb-mode special-mode "Fabb"
  :group 'fabb
  ;; mostly pulled from magit-section.el (magit-section-mode)
  (setq truncate-lines t)
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (fabb--disable-line-numbers))

;;; fabb-status major mode

(defvar fabb-status-mode-map
  (let ((map (make-sparse-keymap))) map)
  "Keymap for `fabb-status-mode'.")

(define-derived-mode fabb-status-mode fabb-mode "Fabb Status"
  "Mode for interacting with fabb tasks."
  :group 'fabb)

;;; fabb-task major mode

(defvar fabb-task-mode-map
  (let ((map (make-sparse-keymap))) map)
  "Keymap for `fabb-task-mode'.")

(define-derived-mode fabb-task-mode compilation-mode "Fabb Task"
  "Mode for interacting with fabb tasks."
  :group 'fabb
  (setq truncate-lines nil)
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (fabb--disable-line-numbers))

(provide 'fabb)
;;; fabb.el ends here
