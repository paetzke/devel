;; Kill buffer if shell terminates
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Set fish as default shell
(defvar my-term-shell "fish")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; Set encoding to UTF-8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)


(provide 'my-shell)
