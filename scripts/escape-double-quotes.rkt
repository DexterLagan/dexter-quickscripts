#lang racket/base
(require racket/string)
(require quickscript)
(script-help-string "escape double-quotes")

(define-script escape-double-quotes
  #:label "escape-double-quotes"
  (λ (selection)
    (string-replace selection "\"" "\\\"")))


; EOF
