#lang racket/base
(require racket/string)

(require quickscript)

(define-script escape-double-quotes
  #:label "escape-double-quotes"
  (λ (selection)
    (string-replace selection "\"" "\\\"")))


; EOF
