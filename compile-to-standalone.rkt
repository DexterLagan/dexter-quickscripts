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
(define *raco-gui* "--gui ")
(define *raco-icon-switch* "--ico ")
(define *icon-dirname* "icon")

(define *nix-icon-ext* #".png")
(define *win-icon-ext* #".ico")
(define *mac-icon-ext* #".icns")

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
    (and (file-exists? f) ; regular files only
         (path-has-extension? f ext)))
  (find-files ext-file? path))

;; if an icon folder exists, returns the enclosing icon file path
(define (get-icon-path parent-path icon-ext)
  (define icon-dir
    (build-path parent-path *icon-dirname*))
  (if (directory-exists? icon-dir)
      (let ((icon-files (find-files/ext icon-dir icon-ext)))
        (if (null? icon-files) #f
            (first icon-files)))
      #f))

;; returns the path to the folder containing f
(define (get-parent-folder f)
  (define-values (base name folder?)
    (split-path f))
  base)

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

;;; main

(define-script compile
  #:label "compile-standalone"
  (λ (selection #:file f #:definitions defs-text)

    (define os-type (system-type))
    (define icon-ext (get-current-platforms-icon-ext os-type))
    (define parent-path (get-parent-folder f))
    (define parent-path-str (path->string parent-path))
    (define icon-path (get-icon-path parent-path icon-ext))
   
    (define gui?
      (or (send defs-text find-string "#lang racket/gui")
          (send defs-text find-string "(require racket/gui")))
    (define raco-command (get-raco-command-line os-type icon-path gui? f))
    (if (system raco-command)
        (begin (shell-execute "explore" parent-path-str ""
                              (current-directory) 'sw_shownormal))
        (void (message-box *app-name* "Error during compilation.")))))


; EOF
