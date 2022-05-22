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

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(comment
 (print "Howdy"))


(provide 'fabb)
;;; fabb.el ends here
