(defun sanitize-bindings-closure (map bindings)
  `(lambda ()
     (when (boundp ',map)
       (dolist (key ,bindings)
         (define-key ,map (kbd key) nil)))))

(setq slime-bindings-to-sanitize '("C-c x"))
(add-hook 'slime-mode-hook
          (sanitize-bindings-closure 'slime-mode-map
                                     'slime-bindings-to-sanitize))

(setq slime-repl-bindings-to-sanitize '("DEL"))
(add-hook 'slime-repl-mode-hook
          (sanitize-bindings-closure 'slime-repl-mode-map
                                     'slime-repl-bindings-to-sanitize))

(provide 'fuck-you-slime)
