# Fabb

A babashka-tasks porcelain inside emacs.
## Status

Fabb is quite useful in my daily work already! There may be some rough edges,
but it's quicker to run bb commands than popping open a shell in emacs or
otherwise.

Currently I configure the bindings in [my doom config](https://github.com/russmatney/dotfiles/blob/bbc4151ad6e71b7a14f8a846d6394d7630cd5548/emacs/.config/doom/%2Bfabb.el) like so:

``` elisp
;;; ~/.config/doom/+fabb.el -*- lexical-binding: t; -*-

(map!
 (:leader :desc "Fabb Status" :nv "f" #'fabb-status))

(map!
 (:after
  fabb
  (:map fabb-mode-map
   :n "i" #'fabb-invoke-ivy
   :n "?" #'fabb-dispatch
   :n "f" #'fabb-dispatch
   :n "q" #'quit-window)

  (:map fabb-status-mode-map
   :n "r" #'fabb-status-invoke-task-and-show-buffer
   :n "R" #'fabb-status-invoke-task-in-background
   :n "e" #'fabb-status-edit-and-invoke-task
   :n "RET" #'fabb-status-show-task-buffer
   :n "x" #'fabb-kill-fabb-buffers

   :n "j" #'fabb-status-goto-next-task
   :n "k" #'fabb-status-goto-previous-task)

  (:map fabb-task-mode-map
   :n "i" #'fabb-invoke-ivy
   :n "?" #'fabb-dispatch

   :nv "e" #'fabb-task-edit-and-reinvoke-task
   :nv "f" #'fabb-task-edit-and-reinvoke-task-with-path
   :nv "r" #'fabb-task-reinvoke-task-prompt
   :nv "R" #'fabb-task-reinvoke-task-no-prompt

   "q" nil
   :n "q" nil))

 (:map compilation-mode-map
  :n "C-k" nil
  :n "C-j" nil)
 (:map compilation-minor-mode-map
  :n "C-k" nil
  :n "C-j" nil))

(use-package! fabb)
```


## Development

I'm currently consuming Fabb in my emacs config as a doom module.

Doom looks for local packages in two dirs, including `~/.config/doom/{local-repo}`.

A symlink to there, plus the following in package.el addition, then a doom sync
worked for me.

``` sh
# something like
ln -s ~/teknql/fabb ~/.config/doom/fabb
```

Then in doom's packages.el:

``` elisp
;; doom packages.el
(package! fabb :recipe (:local-repo "fabb" :build (:not compile)))
```

Then you can configure fabb like:

``` elisp
;; doom config.el
(map!
 (:leader :desc "Fabb Status" :nv "f" #'fabb-status))

(use-package! fabb
  :config
  (map!
   (:map fabb-mode-map
    :n "/" #'fabb-invoke-ivy
    :n "?" #'fabb-dispatch)))
```

Configuring from the doom config fixes some common bindings issues, given that
many of the bindings get overwritten by doom/evil/etc - ideally there's some
simple solution to this.

## Todos

See [the todo.org](./todo.org)
