;;; -*- lexical-binding: t; -*-

(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key [remap upcase-word] #'upcase-dwim)
(bind-key [remap downcase-word] #'downcase-dwim)
(bind-key [remap capitalize-word] #'capitalize-dwim)
(bind-key [remap count-words-region] #'count-words)
(bind-key [remap eval-last-sexp] #'pp-eval-last-sexp)
(bind-key [remap eval-expression] #'pp-eval-expression)
(bind-key [remap zap-to-char] #'zap-up-to-char)
