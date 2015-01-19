(define-skeleton asm-x86_64-skeleton
  "" ""
  "section .text\n"
  "global  " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n\n"

  (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ":\n"
  "        push    rbp\n"
  "        mov     rbp,    rsp\n        "
  _
  "\n\nend:\n"
  "        mov     rsp,    rbp\n"
  "        pop     rbp\n"
  "        ret\n"
  )

(define-skeleton asm-x86-skeleton
  "" ""
  "section .text\n"
  "global  " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n\n"

  (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ":\n"
  "        push    ebp\n"
  "        mov     ebp,    esp\n        "
  _
  "\n\nend:\n"
  "        mov     esp,    ebp\n"
  "        pop     ebp\n"
  "        ret\n"
  )

(define-skeleton asm-mips-skeleton
  "" ""
  "        .globl  main\n\n"

  ".data\n"
  "        buffer:         .space  1024\n"
  "        prompt:         .asciiz \"> \"\n\n"

  ".text\n"
  "main:\n"
  "        li      $v0,    4\n"
  "        la      $a0,    prompt\n"
  "        syscall\n\n"

  "        li      $v0,    8\n"
  "        la      $a0,    buffer\n"
  "        li      $a1,    1024\n"
  "        syscall\n        "
  _
  "\nend:\n"
  "        li      $v0,    10\n"
  "        syscall\n"
  )

(define-auto-insert "\\.\\([S]\\|s\\|asm\\)$" 'asm-x86_64-skeleton)
