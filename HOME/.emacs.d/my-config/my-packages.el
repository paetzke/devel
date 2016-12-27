(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar prelude-packages
  '(
    atom-dark-theme
    fish-mode
    format-sql
    graphviz-dot-mode
    json-reformat
    markdown-mode
    py-autopep8
    py-isort
;    py-yapf
    yaml-mode
    ))

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'my-packages)
