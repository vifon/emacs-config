(fset 'my-iconify (symbol-function 'iconify-frame))
(defalias 'iconify-frame 'ignore)

(provide 'disable-suspend)
