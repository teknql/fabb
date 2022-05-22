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
;;  `fabb-invoke--ivy' can be used to select and run a bb task.
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

;;; misc helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-file-in-current-project (filename)
  "Find and return a full path for the passed FILENAME."
  (when-let ((project-dir (locate-dominating-file default-directory filename)))
    (s-concat (file-name-as-directory project-dir) filename)))

(defun parse-edn (path)
  "Parse the passed PATH as edn via `parseedn-read'."
  (with-temp-buffer
    (insert-file-contents path)
    (car (parseedn-read))))

;;; parsing task-defs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun raw-task->task-def (task-name raw-task bb-edn-path)
  "Convert a passed TASK-NAME and RAW-TASK into a fabb-task-def.

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
            (list :task-command (if is-table
                                    (gethash :task raw-task)
                                  raw-task)))))

(defun parse-bb-edn-tasks (path)
  "Parse and return task-defs for tasks in the bb.edn at PATH."
  (let* ((bb-edn-content (parse-edn path))
         (raw-tasks (gethash :tasks bb-edn-content))
         (task-defs (cl-loop for raw-task being each hash-value of raw-tasks
                             using (hash-key task-name)
                             collect (raw-task->task-def task-name raw-task path))))
    task-defs))

(defun fabb-task-defs ()
  "Return a list of task-defs for the current context."
  (let ((bb-edn-path (find-file-in-current-project "bb.edn")))
    (parse-bb-edn-tasks bb-edn-path)))

;;; invoking tasks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fabb-invoke--buffer-name (task-def)
  "Build the buffer name for TASK-DEF."
  (format "*fabb-%s [%s]*"
          (plist-get task-def :task-name)
          (plist-get task-def :task-dir-name)))

(defun fabb-invoke--command (task-def)
  "Build the command to invoke for TASK-DEF."
  ;; TODO consider more bb options here (via transient)
  (format "bb %s" (plist-get task-def :task-name)))

(defun fabb-invoke-task (task-def)
  "Invoke the passed TASK-DEF via 'bb <task-name>'."
  (let ((compilation-buffer-name-function
         (lambda (_name-of-mode) (fabb-invoke--buffer-name task-def))))
    (compile (fabb-invoke--command task-def))))

;;; ivy-frontend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fabb-invoke--ivy-history nil
  "History for `fabb-invoke--ivy'.")

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

(defun fabb-invoke--ivy ()
  "Select and invoke a bb-task via ivy."
  (interactive)
  ;; TODO handle conditionally requiring/configuring ivy?
  (ivy-read "Invoke bb task:"
            (fabb-invoke--ivy-targets)
            :require-match t
            :caller 'fabb-invoke--ivy
            :history 'fabb-invoke--ivy-history
            :action 'fabb-invoke--ivy-action))


(provide 'fabb)
;;; fabb.el ends here
