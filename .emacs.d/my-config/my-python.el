
;; autopep8
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=80"))


;; isort
(add-hook 'before-save-hook 'py-isort-before-save)
(setq py-isort-options '("--lines=80"))



(provide 'my-python)
