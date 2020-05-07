#lang racket/gui
(require quickscript)
(module+ test
  (require rackunit))

;;; purpose

; to generate a program skeleton, optionally with a few practical definitions

;;; consts

(define *app-name* "Program Skeleton Generator")
(define *purpose-lines* "\n\n;;; purpose\n\n")
(define *version-lines* "\n\n;;; version history\n\n; v1.0 - initial release.")
(define *consts-lines* "\n\n;;; consts\n\n")
(define *defs-lines* "\n\n;;; defs\n\n")
(define *main-lines* "\n\n;;; main\n\n")
(define *optional-defs* #<<---
;; string-append shortcut
(define ++ string-append)

;; quick echo
(define echo
  (λ args
    (displayln (apply ~a args))))

;; quick message box
(define msgbox
  (λ args
    (void (message-box *app-name* (string-append (apply ~a args) "  ")))))

;; quick confirmation dialog
(define (confirm? question)
  (eq? 'yes (message-box *app-name* question #f (list 'yes-no))))

;; displays an error and quits
(define die
  (λ args
    (displayln (string-append (apply ~a args) "\n"))
    (exit 1)))

;; displays an error dialog and quits
(define die!
  (λ args
    (void (message-box *app-name* (string-append (apply ~a args) "  ")))
    (exit 1)))

;; macro combines rackunit's check-equal?, check-true and check-false
;; does not require rackunit to be required at the module level
;; usage:
;;(assert (string-append "a" "b") "ab")      ; passes
;;(assert (string-append "a" "b" "c") "cba") ; fails
(define-syntax (assert stx)
  (syntax-parse stx
    [(_assert ?a ?b)
     (quasisyntax/loc stx
       (module+ test
         (require rackunit)
         #,(syntax/loc stx (check-equal? ?a ?b #'?a))))]
    [(_assert ?a)
     (quasisyntax/loc stx
       (module+ test
         (require rackunit)
         #,(syntax/loc stx (check-true ?a #'?a))))]))

; Macro that defines whichever parameters are fed to it and fills them in from command line
(define-syntax define-command-line-params
  (syntax-rules ()
    ((define-command-line-params appname param1 ...)
     (define-values (param1 ...)
       (command-line #:program appname
                     #:args (param1 ...)
                     (values param1 ...))))))

; returns a function that composes parameters in order,
; using a placeholder _ for passing values between functions.
(define-syntax (compose_ stx)
  ; macro to compose functions passing an '_' parameter
  (syntax-case stx ()
    ((_ f1 ...)
     (with-syntax ([x-var (datum->syntax stx '_)])
       #'(apply compose1 (reverse (list (λ (x-var) f1) ...)))))))
; unit test
(module+ test
  (require rackunit)
  (check-equal? ((compose_ (string-trim _)
                           (string-downcase _)
                           (string-replace _ " " "-")) "Hello World")
                "hello-world"))
---
  )

;;; defs

;; displays a generic confirmation dialog. Returns true or false depending on user's selection
(define (confirm? question)
  (eq? 'yes (message-box *app-name* (string-append question "  ") #f (list 'yes-no))))

;;; main

(define new-program-content #f)

(define program-name
  (get-text-from-user *app-name* "What will be the program name?  "))

(define purpose
  (get-text-from-user *app-name* "What will be its purpose?  "))

(define rackunit?
  (confirm? "Will it use RackUnit?"))

(define optional-defs?
  (confirm? "Do you want to include a few useful procedures?"))

(when program-name
  (set! new-program-content
        (string-append (if rackunit? "(module+ test\n  (require rackunit))" "")
                       *purpose-lines*
                       (if purpose (string-append "; " purpose) "")
                       *version-lines*
                       *consts-lines*
                       "(define *app-name* \"" program-name "\")\n"
                       "(define *version* \"1.0\")"
                       *defs-lines*
                       (if optional-defs? *optional-defs* "")
                       *main-lines*)))

;; main script proc
(define-script generate-skeleton
  #:label "generate-skeleton"
  (λ (selection)
    new-program-content))


; EOF
