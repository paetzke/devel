
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(setq server-use-tcp t)



(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil))

(toggle-scroll-bar -1)
;; no backup files
(setq make-backup-files nil)
;; remove whites
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; don't autosave
(setq auto-save-default nil)
;; no tabs
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(tool-bar-mode -1)

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)

; Some initial langauges we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
                                        ;(sh . t)
   (python . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (calc . t)))


(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar prelude-packages
  '(
    ;atom-dark-theme
    fish-mode
    ;format-sql
                                        ;graphviz-dot-mode
    json-reformat
    markdown-mode
    py-autopep8
    py-isort
;    py-yapf
    yaml-mode
    ))

(defun prelude-packages-installed-p ()
  (cl-loop for p in prelude-packages
        when (not (package-installed-p p)) do (cl-return nil)
        finally (cl-return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;; autopep8
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=80"))


;; isort
(add-hook 'before-save-hook 'py-isort-before-save)
(setq py-isort-options '("--lines=80"))


(load-theme 'dichromacy t)
