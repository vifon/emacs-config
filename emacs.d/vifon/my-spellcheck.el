;;; Based on: https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html
(with-eval-after-load "ispell"
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (unless (getenv "LANG")
    (setenv "LANG" "en_US"))
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "pl_PL,en_US")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "pl_PL,en_US")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_pl_PL.
  (setq ispell-personal-dictionary "~/.hunspell_personal"))
;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
(unless (file-exists-p "~/.hunspell_personal")
  (shell-command "touch ~/.hunspell_personal"))

(provide 'my-spellcheck)
