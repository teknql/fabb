;;; fabb.el --- A babashka tasks porcelain -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Russell Matney
;;
;; Author: Russell Matney <russell.matney@gmail.com>
;; Maintainer: Russell Matney <russell.matney@gmail.com>
;; Version: 0.0.1
;; Keywords: clojure babashka tasks
;; Homepage: https://github.com/teknql/fabb
;; Package-Requires: ((emacs "27.1"))
;;
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
;;
;;; Code:
(require 's)
(require 'parseedn)

(require 'cl-lib)
;; maybe this is preferred?
;; (eval-when-compile (require 'cl-lib))

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(defun not-impled ()
  (interactive)
  (print "not impled"))

;;; fabb groups and vars ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup fabb nil
  "Invoke and manage babahska tasks."
  :group 'tools)

;;; misc helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun directory-for-file (path)
  "Return the directory for PATH."
  (when path
    (directory-file-name (file-name-directory path))))

(defun find-file-in-project (filename &optional dir)
  "Find and return a full path for the passed FILENAME.

DIR can be passed to determine the directory to search from.
DIR defaults to `default-directory', which depends on the current buffer."
  (let ((directory (cond
                    ((and dir (file-directory-p dir)) dir)
                    (dir (directory-for-file dir))
                    (t default-directory))))
    (when-let ((project-dir (locate-dominating-file directory filename)))
      (s-concat (file-name-as-directory project-dir) filename))))

(comment
 (find-file-in-project "bb.edn")
 (find-file-in-project "bb.edn" "~/russmatney/clawe")
 (find-file-in-project "bb.edn" "~/russmatney/clawe/src/user.clj")
 (find-file-in-project "nope.edn"))

(defun parse-edn (path)
  "Parse the passed PATH as edn via `parseedn-read'."
  (with-temp-buffer
    (insert-file-contents path)
    (car (parseedn-read))))

;;; parsing task-defs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun raw-task->task-def (task-name raw-task bb-edn-path)
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

(defun parse-bb-edn-tasks (path)
  "Parse and return task-defs for tasks in the bb.edn at PATH."
  (let* ((bb-edn-content (parse-edn path))
         (raw-tasks (gethash :tasks bb-edn-content)))
    (cl-loop for raw-task being each hash-value of raw-tasks
             using (hash-key task-name)
             collect (raw-task->task-def task-name raw-task path))))

(defun fabb-task-defs (&optional path)
  "Return a list of task-defs for the current context.

PATH can optionally be used to specify a different starting point for
determining the bb.edn."
  (let ((bb-edn-path (find-file-in-project "bb.edn" path)))
    (parse-bb-edn-tasks bb-edn-path)))

(comment
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

(defun fabb-invoke-task (task-def)
  "Invoke the passed TASK-DEF via 'bb <task-name>'."
  (let ((default-directory (plist-get task-def :task-dir))
        (compilation-buffer-name-function
         (lambda (_name-of-mode) (fabb-task--buffer-name task-def))))
    (compile (fabb-task--command task-def))))

;;; fabb-dispatch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO require transient
;;;###autoload (autoload 'fabb-dispatch "fabb" nil t)
(transient-define-prefix fabb-dispatch ()
  ["Invoke Tasks"
   [("/" "Select Task with ivy" fabb-invoke-ivy)
    ("l" "List Task buffers" not-impled)]]
  ["Some other Category"
   [("e" "print something" not-impled)]])

;;; fabb-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO it's likely we want a _minor_ mode to share bindings
(defvar fabb-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (suppress-keymap map t)
    (define-key map "/" #'fabb-invoke-ivy)
    (define-key map "?" #'fabb-dispatch)
    map)
  "Keymap for `fabb-mode'.")

(defun disable-line-numbers ()
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

(define-derived-mode fabb-mode special-mode "Fabb"
  :group 'fabb
  ;; mostly pulled from magit-section.el (magit-section-mode)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (disable-line-numbers))

;;; fabb-status ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fabb-get-mode-buffer (mode name)
  "Return a buffer with MODE and NAME.

Otherwise return nil.

See `magit-get-mode-buffer' for a more mature version of this."
  (seq-some (lambda (b)
              (with-current-buffer b
                (and (eq major-mode mode)
                     (string= (buffer-name) name)
                     b)))
            (buffer-list)))

(defun fabb-status--buffer-name (&optional path)
  "Return the fabb-status buffer name for the current context, or PATH."
  (when-let ((directory (directory-for-file (find-file-in-project "bb.edn" path)) ))
    (format "*fabb-status* [%s]" directory)))

(defun fabb-display-buffer (buffer)
  "Display the passed BUFFER via `display-buffer'.

Attempt to re-use fabb-mode derived windows (like Magit does)."
  (let ((window
         (display-buffer buffer
                         (if (and (derived-mode-p 'fabb-mode)
                                  (not (memq (with-current-buffer buffer major-mode)
                                             '(fabb-status-mode))))
                             '(display-buffer-same-window)
                           nil))))
    (select-window window)))

(defun fabb-status (&optional path)
  "Show the status of fabb tasks.

PATH can be any file from which the bb.edn should be found.
It defaults to the current buffer's file."
  (interactive)
  (let* ((status-buffer-name (fabb-status--buffer-name path))
         (existing-buffer
          (fabb-get-mode-buffer 'fabb-status-mode status-buffer-name))
         (buffer (if existing-buffer existing-buffer
                   (generate-new-buffer status-buffer-name))))
    (with-current-buffer buffer
      (fabb-status-mode))
    (fabb-display-buffer buffer)
    buffer))

;;;###autoload
(defalias 'fabb #'fabb-status)

(defvar fabb-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fabb-mode-map)
    map)
  "Keymap for `fabb-status-mode'.")

(define-derived-mode fabb-status-mode fabb-mode "Fabb Status"
  "Mode for interacting with fabb tasks.

\\<fabb-mode-map>\

\\<fabb-status-mode-map>\

\\{fabb-status-mode-map}"
  :group 'fabb)

;;; ivy-frontend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fabb-invoke--ivy-history nil
  "History for `fabb-invoke-ivy'.")

(defun fabb-invoke--ivy-targets ()
  "Parse task-defs and map them into ivy targets."
  (thread-last
    (fabb-task-defs)
    (mapcar (lambda (task-def)
              ;; assumes task-names are unique (which they are, per-project)
              ;; TODO include :doc in ivy string
              ;; TODO include last-run-at/running-status if known
              (cons (plist-get task-def :task-name) task-def)))))

(defun fabb-invoke--ivy-action (selection)
  "Invokes the task-def on SELECTION."
  (when-let ((selected-task-def (cdr selection)))
    (fabb-invoke-task selected-task-def)))

;;;###autoload (autoload 'fabb-invoke-ivy "fabb" nil t)
(defun fabb-invoke-ivy ()
  "Select and invoke a bb-task via ivy."
  (interactive)
  ;; TODO handle conditionally requiring/configuring ivy?
  (ivy-read "Invoke bb task:"
            (fabb-invoke--ivy-targets)
            :require-match t
            :caller 'fabb-invoke-ivy
            :history 'fabb-invoke--ivy-history
            :action 'fabb-invoke--ivy-action))

(provide 'fabb)
;;; fabb.el ends here
