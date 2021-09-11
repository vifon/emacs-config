(define-skeleton haskell-skeleton
  "" ""
  "module Main where\n\n"
  "main :: IO ()\n"
  "main = do\n"
  "  " _ "\n"
  "  return ()"
  )

(define-auto-insert "\\.hs\\'" 'haskell-skeleton)
