#lang racket/gui
(require quickscript)
(module+ test
  (require rackunit))

;;; purpose

; to compile the current program to standalone executable,
; optionally using the first icon found in /icon.
; Platform-independent.
; By Dexter Santucci and Laurent Orseau

;;; consts

(define *app-name* "Compile Standalone")
(define *raco-command-line* "raco exe ")
(define *raco-embed-dlls* "--embed-dlls ")
(define *raco-gui* "--gui ")
(define *raco-icon-switch* "--ico ")
(define *icon-dirname* "icon")

; icon file extensions for each OS
(define *nix-icon-ext* #".png")
(define *win-icon-ext* #".ico")
(define *mac-icon-ext* #".icns")

; shell file explorer commands for each OS
(define *nix-file-browser-cmd* "xdg-open") ; mimeopen -n
(define *win-file-browser-cmd* "explorer")
(define *mac-file-browser-cmd* "open")

;;; defs

;; get the current platform's icon extension
(define (get-current-platforms-icon-ext os-type)
  (case os-type
    (('unix)    *nix-icon-ext*)
    (('windows) *win-icon-ext*)
    (('macosx)  *mac-icon-ext*)))

;; find files of a given extension recursively from the current directory
;Causes an error from Windows 10:
;Error in script file "C:\\...\\compile-standalone.rkt":
; path-extension=?: contract violation
;  expected: (or/c bytes? string?)
;  given: #<path:C:\Code\Racket\...\icon\plane_icon.ico>
(define (find-files/ext path ext)
  (define (ext-file? f)
    (and (not (void? f))
         (or (path? f) (string? f))
         (file-exists? f)
         (path-has-extension? f ext)))
  (find-files ext-file? path))

;; same as previous, but using string-suffix?
;Causes an error from Windows 10:
;Error in script file "C:\\...\\compile-standalone.rkt":
; path-extension=?: contract violation
;  expected: (or/c bytes? string?)
;  given: #<void>
(define (find-files/ext# path ext)
  (define (ext-file? f)
    (and (not (void? f))
         (or (path? f) (string? f))
         (file-exists? f)
         (string-suffix? (path->string f) ext)))
  (find-files ext-file? path))

;; if an icon folder exists, returns the enclosing icon file path
(define (get-icon-path parent-path icon-ext)
  (define icon-dir
    (build-path parent-path *icon-dirname*))
  (if (directory-exists? icon-dir)
      (let ((icon-files (find-files/ext# icon-dir icon-ext)))
        (if (null? icon-files) #f
            (first icon-files)))
      #f))

;; generates a raco command line, given the source file
(define (get-raco-command-line os-type icon-path gui? source-file)
  (string-append *raco-command-line*
                 (if (eq? os-type 'windows) *raco-embed-dlls* "")
                 (if icon-path
                     (string-append *raco-icon-switch* icon-path " ") "")
                 (if gui? *raco-gui* "")
                 (path->string source-file)))
; unit test
(module+ test
  (check-equal? (get-raco-command-line 'windows "icon/win-icon.ico" #t (string->path "source-file.rkt"))
                "raco exe --embed-dlls --ico icon/win-icon.ico --gui source-file.rkt")
  (check-equal? (get-raco-command-line 'unix "icon/linux-icon.png" #f (string->path "source-file.rkt"))
                "raco exe --ico icon/linux-icon.png source-file.rkt")
  (check-equal? (get-raco-command-line 'macosx #f #t (string->path "source-file-mac.rkt"))
                "raco exe --gui source-file-mac.rkt")
  (check-equal? (get-raco-command-line 'macosx "icon/mac-icon.icns" #f (string->path "source-file-mac.rkt"))
                "raco exe --ico icon/mac-icon.icns source-file-mac.rkt"))

;; opens a file explorer window given a path and the system type
; thanks Laurent!
(define (explore-path path os-type)
  (define file-browse-command
    (case os-type
      (('unix)    *nix-file-browser-cmd*)
      (('windows) *win-file-browser-cmd*)
      (('macosx)  *mac-file-browser-cmd*)))
  (system (string-append file-browse-command " \"" (path->string (path-only path)) "\"")))

;;; main

(define-script compile
  #:label "compile-standalone"
  (λ (selection #:file f #:definitions defs-text)

    (define gui?
      (eq? 'yes (message-box *app-name* "Use GRacket? If you select 'No', Racket will be used." #f (list 'yes-no))))
    (define os-type (system-type))
    (define icon-ext (get-current-platforms-icon-ext os-type))
    (define parent-path (path-only f))
    (define icon-path (get-icon-path parent-path icon-ext))
    
    (define raco-command (get-raco-command-line os-type icon-path gui? f))
    (if (system raco-command)
        (begin (explore-path parent-path os-type))
        (void (message-box *app-name* "Error during compilation.")))))


; EOF
