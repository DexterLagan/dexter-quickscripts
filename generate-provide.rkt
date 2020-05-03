#lang racket/gui
(require quickscript)
(module+ test
  (require rackunit))

;;; purpose
 
; To generate the provide form from a module file.
; A better way would be to edit the syntax and delete the function bodies, but I'm not that great with dealing with syntax yet.

;;; consts

(define *app-name* "Provide Generator")

;;; defs

;; returns a list of syntax objects from the provided string
(define (get-stx-lines s)
  (define lang-less-forms
    (string-append "("
                   (string-join (cdr (string-split s "\n")) "\n")
                   "\n)"))
  (void (message-box *app-name* lang-less-forms))
  (syntax->list
   (with-input-from-string lang-less-forms read-syntax)))

;; macro to compose functions passing an 'x' parameter
(define-syntax (composex stx)
  (syntax-case stx ()
    ((_ f1 ...)
     (with-syntax ([x-var (datum->syntax stx 'x)])
       #'(compose1 (λ (x-var) f1) ...)))))
; unit test
(module+ test
  (check-equal? ((composex (string-replace x " " "-")
                           (string-downcase x)
                           (string-trim x)) "Naice Day")
                "naice-day"))

;; returns the contents of the clipboard
(define (get-clipboard)
  (send the-clipboard get-clipboard-string (current-milliseconds)))

;; sets the clipboard with the provided string
(define (set-clipboard s)
  (send the-clipboard set-clipboard-string s (current-milliseconds)))

;; converts a syntax object to string with no extra info
(define (stx->str stx)
  (~a (syntax->datum stx)))

;; extract the possible function name of a line
(define (get-function-macro-name stx-line)
  (let ((syntax-list        (syntax->list stx-line)))
    (if (and syntax-list (>= (length syntax-list) 2))                                 ; make sure syntax list contains at least two objects
        (let* ((2nd         (second   syntax-list))
               (1st-string  (stx->str (first    syntax-list)))
               (2nd-string  (stx->str 2nd)))
          (cond ((and (string=? 1st-string "define") (string-prefix? 2nd-string "(")) ; if this is a function definition
                 (stx->str (first (syntax->list 2nd))))
                ((string=? 1st-string "define-syntax")
                 2nd-string)
                (else "")))
        "")))

;; returns the length of the longest string in list l
(define (get-length-of-longest-string l)
  (if (and (list? l) (string? (car l)))
      (apply max (map string-length l))
      0))

;; prints a non-empty line with carriage return
(define (println s)
  (if (non-empty-string? s) (string-append s "\n") ""))

;; returns a complete provide line as string from a single syntax line
(define (generate-provide-line stx-line indentation)
  (let ((syntax-list         (syntax->list stx-line)))
    (if (and syntax-list (>= (length syntax-list) 2))                                      ; make sure syntax list contains at least two objects
        (let* ((1st          (first    syntax-list))
               (2nd          (second   syntax-list))
               (1st-string   (stx->str 1st))
               (2nd-string   (stx->str 2nd))
               (definition?  (string=? 1st-string "define"))
               (macro?       (string=? 1st-string "define-syntax"))               
               (param-list?  (string-prefix? 2nd-string "("))
               (macro-tag-maybe (if macro? " [MACRO]" ""))
               (provide-indent "         "))
          (cond ((and (or definition? macro?)                                              ; if this is a function or a macro with parameter definition
                      param-list?)                                     
                 (let* ((function-name   (stx->str (first (syntax->list 2nd))))
                        (function-length (string-length function-name))
                        (space-length    (if (>= indentation function-length)
                                             (- indentation function-length) 0))
                        (spacer          (make-string space-length #\space)))              ; extract function name
                   (string-append provide-indent function-name spacer " ; " 2nd-string macro-tag-maybe))) ; return function name followed by function def.
                ((and macro? (not param-list?))                                            ; if this is a macro definition with no parameter definition
                 (let* ((macro-name 2nd-string)
                        (macro-length (string-length macro-name))
                        (space-length (if (>= indentation macro-length)
                                          (- indentation macro-length) 0))
                        (spacer          (make-string space-length #\space)))
                   (string-append provide-indent macro-name spacer " ; (" macro-name " ... )" macro-tag-maybe))) ; return macro name followed by comment
                (else "")))
        "")))

;; adds a parenthesis after the last item in a list of code lines, before its comment
(define (close-parenthesis pre-indentation l post-indentation)
  (let* ((last-line-clean (string-trim (last l)))
         (last-word       (first (string-split last-line-clean " ")))
         (last-comment    (if (string-contains? last-line-clean ";")
                              (string-trim (second (string-split last-line-clean ";")))
                              ""))
         (word-length     (string-length last-word))
         (space-length    (if (>= post-indentation word-length)
                              (- post-indentation word-length) 0))
         (pre-spacer      (make-string pre-indentation #\space))
         (spacer          (make-string space-length #\space))
         (new-word        (string-append pre-spacer last-word ")" spacer "; " last-comment))
         (most-words      (all-but-last l)))
    (append most-words (list new-word))))

;; returns all but the last elements of a list
(define (all-but-last l)
  (if (and (list? l) (>= (length l) 2))
      (reverse (cdr (reverse l)))
      l))

;; returns a complete provide form from the provided syntax lines
(define (generate-provide-form stx-lines indentation)
  (let ((generate-lines (λ (stx-line) (generate-provide-line stx-line indentation))))  ; prepare a function to generate lines,
    ((composex (string-replace x "(provide          " "(provide ")                     ; remove extra indentation on first line
               (string-append "(provide " x)                                           ; add provide form header
               (apply string-append x)                                                 ; concatenate them
               (map println x)                                                         ; add carriage returns
               (close-parenthesis 9 x indentation)                                     ; add closing parenthesis for provide form - 9 chars = "(provide "
               (sort x string<?)                                                       ; sort them
               (map generate-lines x))                                                 ; generate lines
     stx-lines)))

;;; main

(define-script generate-provide
  #:label "generate-provide"
  (λ (selection #:file f #:definitions defs-text)
    (let* ((stx-lines    (get-stx-lines                   (send defs-text get-text)))                                ; alternative: (send defs-text get-text)
           (indentation  (get-length-of-longest-string    (map get-function-macro-name stx-lines)))  ; calculate the lendth of the longest function name
           (provide-form (generate-provide-form stx-lines indentation)))                             ; process syntax lines and generate the provide form
      (void (message-box *app-name* provide-form)))))


; EOF
