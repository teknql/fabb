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
;;  Fabb's initial focus is running and managing output from babashka tasks in
;;  Emacs.
;;
;;  To support this, we find and parse a bb.edn to get a list of task defs.
;;  Those are presented in a special *fabb-tasks* buffer, from whence they can
;;  be invoked.
;;  Current and previously running tasks can also be found, jumped to, or re-run
;;  from *fabb-tasks*.
;;
;;; Code:
(require 's)
(require 'parseedn)
(require 'cl-lib)

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)


(defun find-file-in-current-project (filename)
  "Find and return a full path for the passed FILENAME."
  (when-let ((project-dir (locate-dominating-file default-directory filename)))
    (s-concat (file-name-as-directory project-dir) filename)))

(comment
 (find-file-in-current-project "bb.edn")
 (find-file-in-current-project "idontexist.edn"))

(defun parse-edn (path)
  "Parse the passed PATH as edn via `parseedn-read'."
  (with-temp-buffer
    (insert-file-contents path)
    (car (parseedn-read))))

(comment
 (let ((content
        (thread-first "bb.edn" find-file-in-current-project parse-edn)))
   (gethash :tasks content)))

(defun raw-task->task-def (task-name raw-task)
  "Convert a passed TASK-NAME and RAW-TASK into a fabb-task-def.

task-defs are property lists with these keys:
- `:task-name': the key from the bb.edn tasks map. Used to invoke the task via
  `bb <task-name>'.
- `:task-doc': an optional documentation string from the task's `:doc' key.
- `:task-command': the clojure s-expression that will eventually be executed."
  (let* ((is-table (hash-table-p raw-task))
         (doc (when is-table (gethash :doc raw-task)))
         (task-def
          (list :task-name task-name
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
                             collect (raw-task->task-def task-name raw-task))))
    task-defs))

(comment
 (thread-first "bb.edn" find-file-in-current-project parse-bb-edn-tasks))


(provide 'fabb)
;;; fabb.el ends here
