#lang racket/gui
(require quickscript)
(module+ test
  (require rackunit))

;;; purpose

; to compile the current program to standalone executable,
; optionally using the first icon found in /icon.
; Platform-independent.

;;; consts

(define *app-name* "Compile Standalone")
(define *raco-command-line* "raco exe ")
(define *raco-embed-dlls* "--embed-dlls ")
(define *raco-icon-switch* "--ico ")

(define *nix-icon-ext* ".png")
(define *win-icon-ext* ".ico")
(define *mac-icon-ext* ".icns")

;;; defs

;; get the current platform's icon extension
(define (get-current-platforms-icon-ext os-type)
  (case os-type
    (('unix)    *nix-icon-ext*)
    (('windows) *win-icon-ext*)
    (('macosx)  *mac-icon-ext*)))

;; find files of a given extension recursively from the current directory
(define (find-files/ext ext)
  (find-files (λ (f) (string-suffix? (path->string f) ext))))

;; if an icon folder exists, returns the enclosing icon file path
(define (get-icon-path icon-ext)
  (if (directory-exists? "icon")
      (let ((icon-files (find-files/ext icon-ext)))
        (if (null? icon-files) #f
            (first icon-files)))
      #f))

;; returns the path to the folder containing f
(define (get-parent-folder-str f)
  (define-values (base name folder?)
    (split-path f))
  (path->string base))

;; generates a raco command line, given the source file
(define (get-raco-command-line os-type icon-path source-file)
  (string-append *raco-command-line*
                 (if (eq? os-type 'windows) *raco-embed-dlls* "")
                 (if icon-path
                     (string-append *raco-icon-switch* icon-path " ") "")
                 (path->string source-file)))
; unit test
(module+ test
  (check-equal? (get-raco-command-line 'windows "icon/win-icon.ico" (string->path "source-file.rkt"))
                "raco exe --embed-dlls --ico icon/win-icon.ico source-file.rkt")
  (check-equal? (get-raco-command-line 'unix "icon/linux-icon.png" (string->path "source-file.rkt"))
                "raco exe --ico icon/linux-icon.png source-file.rkt")
  (check-equal? (get-raco-command-line 'macosx #f (string->path "source-file-mac.rkt"))
                "raco exe source-file-mac.rkt")
  (check-equal? (get-raco-command-line 'macosx "icon/mac-icon.icns" (string->path "source-file-mac.rkt"))
                "raco exe --ico icon/mac-icon.icns source-file-mac.rkt"))

;;; main

(define os-type (system-type))
(define icon-ext (get-current-platforms-icon-ext os-type))
(define icon-path (get-icon-path icon-ext))

(define-script compile
  #:label "compile"
  (λ (selection #:file f)
    (if (system (get-raco-command-line os-type icon-path f))
        (begin (shell-execute "explore" (get-parent-folder-str f) ""
                              (current-directory) 'sw_shownormal))
        (void (message-box *app-name* "Error during compilation.")))))


; EOF
