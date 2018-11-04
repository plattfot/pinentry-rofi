#! /usr/bin/guile -s
!#

(use-modules
 (ice-9 textual-ports)
 (srfi srfi-9) ;; For records
 (ice-9 format)
 (ice-9 regex))

(define-record-type <pinentry>
  (make-pinentry ok prompt desc visibility)
  pinentry?
  (ok pinentry-ok set-pinentry-ok!)
  (prompt pinentry-prompt set-pinentry-prompt!)
  (desc pinentry-desc set-pinentry-desc!)
  (visibility pinentry-visibility set-pinentry-visibility!))

(define-record-type <pinregex>
  (make-pinregex option getinfo setkeyinfo setdesc setprompt getpin bye)
  pinregex?
  (option pinregex-option)
  (getinfo pinregex-getinfo)
  (setkeyinfo pinregex-setkeyinfo)
  (setdesc pinregex-setdesc)
  (setprompt pinregex-setprompt)
  (getpin pinregex-getpin)
  (bye pinregex-bye))

(define-syntax-rule (set-and-return! val expr)
  "Set val to expr and return val"
  (begin (set! val expr) val))

(define (pinentry-loop pinentry pinregex input-port)
  (let ((line (get-line input-port))
        (rofi "rofi -dmenu -input /dev/null ~a -disable-history -p ~s ~a ~s")
        (regex-match #f))
    (unless (eof-object? line)
      (cond 
       ((set-and-return! regex-match (regexp-exec (pinregex-option pinregex) line))
        (format #t "Option is ~s\n" (match:substring regex-match 1)))

       ((set-and-return! regex-match (regexp-exec (pinregex-setdesc pinregex) line))
        (set-pinentry-desc! pinentry (match:substring regex-match 1)))

       ((set-and-return! regex-match (regexp-exec (pinregex-setprompt pinregex) line))
        (set-pinentry-prompt! pinentry (match:substring regex-match 1)))

       ((set-and-return! regex-match (regexp-exec (pinregex-getpin pinregex) line))
        (format #t rofi
                (if (pinentry-visibility pinentry) "" "-password")
                (pinentry-prompt pinentry)
                (if (equal? (pinentry-desc pinentry) "") "" "-mesg")
                (pinentry-desc pinentry)))

       ((set-and-return! regex-match (regexp-exec (pinregex-bye pinregex) line))
        (exit #t))
       
       (#t
        (begin (format #t "BYE\n")
               (exit #f))))
      (pinentry-loop pinentry pinregex input-port))))

(format #t "OK Please go ahead\n")
(let ((pinentry (make-pinentry #t "Passphrase:" "" #f))
      (pinregex (make-pinregex
                 (make-regexp "^OPTION (.+)$")
                 (make-regexp "^GETINFO (.+)$")
                 (make-regexp "^SETKEYINFO (.+)$")
                 (make-regexp "^SETDESC (.+)$")
                 (make-regexp "^SETPROMPT (.+)$")
                 (make-regexp "^GETPIN$")
                 (make-regexp "^BYE"))))
  (pinentry-loop pinentry pinregex (current-input-port)))



