;;; fabb-colors.el --- Colors for fabb faces -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defface fabb-colors-task-name
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background  dark)) :foreground "DarkSeaGreen2"))
  "A task name font face."
  :group 'fabb)

(defface fabb-colors-task-name-running
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background  dark)) :foreground "DarkSeaGreen2"))
  "A task name font face."
  :group 'fabb)

(defface fabb-colors-task-name-completed
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background  dark)) :foreground "DarkSeaGreen2"))
  "A task name font face."
  :group 'fabb)

(defface fabb-colors-task-name-error
  '((((class color) (background light)) :foreground "coral3")
    (((class color) (background  dark)) :foreground "coral3"))
  "A task name font face."
  :group 'fabb)

(defface fabb-colors-task-doc
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey50"))
  "A doc font face."
  :group 'fabb)

(defface fabb-colors-detail-key
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey50"))
  "A detail key font face."
  :group 'fabb)

(defface fabb-colors-buffer-name
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "A buffer name font face."
  :group 'fabb)

(defface fabb-colors-debug-key
  '((((class color) (background light)) :foreground "coral3")
    (((class color) (background  dark)) :foreground "coral3"))
  "Font face for keys in debug contexts."
  :group 'fabb)

;; (defface fabb-colors-debug-key
;;   '((((class color) (background light)) :foreground "#10151C")
;;     ;; DistantForeground: #9CAABB
;;     ;; Background: #28323B
;;     (((class color) (background  dark)) :foreground "#10151C"))
;;   "A task name font face."
;;   :group 'fabb)

(defface fabb-colors-debug-value
  '((((class color) (background light)) :foreground "salmon1")
    (((class color) (background  dark)) :foreground "salmon1"))
  "Font face for values in debug contexts."
  :group 'fabb)

(provide 'fabb-colors)
;;; fabb-colors.el ends here
