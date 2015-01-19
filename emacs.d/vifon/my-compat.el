;;; for Emacs 24.3, probably issue with using package.el via Emacs 24.4
(unless (fboundp 'function-put)
  (autoload 'function-put "helm-config"))

(provide 'my-compat)
