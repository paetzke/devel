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

(load-theme 'misterioso t)



(custom-set-faces
 '(default ((t (:family "Source Code Pro" :height 100)))))


(setq inhibit-startup-message t)

(setq column-number-mode t)

;; Prevent the annoying beep on errors

(setq ring-bell-function 'ignore)


(provide 'my-generic)
