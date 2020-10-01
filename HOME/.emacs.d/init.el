;; MacOs specific settings
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil))


;; Don't create backup files
(setq make-backup-files nil)


;; Remove trailing whitespaces
(add-hook 'write-file-hooks 'delete-trailing-whitespace)


(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(defvar prelude-packages
  '(
    fish-mode
    py-autopep8
    py-isort
    ))
(defun prelude-packages-installed-p ()
  (cl-loop for p in prelude-packages
        when (not (package-installed-p p)) do (cl-return nil)
        finally (cl-return t)))
(unless (prelude-packages-installed-p)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
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
