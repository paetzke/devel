
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(setq server-use-tcp t)


(add-to-list 'load-path (expand-file-name "~/.emacs.d/my-config"))
(require 'my-packages)
(require 'my-generic)
(require 'my-python)
(require 'my-shell)
(require 'my-org-mode)
