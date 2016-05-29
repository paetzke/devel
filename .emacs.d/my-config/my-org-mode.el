(require 'org-install)

(global-set-key (kbd "M-S <left>") 'org-table-delete-column)
(global-set-key (kbd "M-S <right>") 'org-table-insert-column)

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)

; Some initial langauges we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (calc . t)))

(provide 'my-org-mode)
