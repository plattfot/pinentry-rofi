#! /usr/bin/guile -s
!#

;; TODO check the license for this one.
;; Based on https://gist.github.com/sardemff7/759cbf956bea20d382a6128c641d2746

(use-modules
 (ice-9 popen)
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

(define-syntax-rule (set-and-return! val expr)
  "Set val to expr and return val"
  (begin (set! val expr) val))

(define (string-empty? str)
  "Evaluates to #t if string is empty."
  (string=? str ""))

(define (pinentry-option pinentry line)
  "Process line if it starts with OPTION.
Return false otherwise.

Known options are:
grab
ttyname=/dev/pts/1
ttytype=tmux-256color
lc-messages=C
allow-external-password-cache
default-ok=_OK
default-cancel=_Cancel
default-yes=_Yes
default-no=_No
default-prompt=PIN:
default-pwmngr=_Save in password manager
default-cf-visi=Do you really want to make your passphrase visible on the screen?
default-tt-visi=Make passphrase visible
default-tt-hide=Hide passphrase
touch-file=/run/user/1000/gnupg/S.gpg-agent"
  (let ((option-re (make-regexp "^OPTION (.+)$")))
    (regexp-exec option-re line)))

(define (pinentry-getinfo pinentry line)
  "Process line if it starts with GETINFO"
  (let ((getinfo-re (make-regexp "^GETINFO (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec getinfo-re line))
      (let ((info (match:substring regex-match 1)))
        (cond
         ((string=? info "pid")
          (format #t "D ~a\n" (getpid))))))
    regex-match))

(define (pinentry-setkeyinfo pinentry line)
  "SETKEYINFO s/FINGERPRINT"
  (let ((setkeyinfo-re (make-regexp "^SETKEYINFO (.+)$")))
    (regexp-exec setkeyinfo-re line)))

(define (pinentry-setdesc pinentry line)
  "SETDESC Please enter the passphrase for the ssh key%0A  ke:yf:in:ge:rp:ri:nt"
  (let ((setdesc-re (make-regexp "^SETDESC (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec setdesc-re line))
      (set-pinentry-desc! pinentry (match:substring regex-match 1)))
    regex-match))

(define (pinentry-setprompt pinentry line)
  "SETPROMPT Passphrase:"
  (let ((setprompt-re (make-regexp "^SETPROMPT (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec setprompt-re line))
      (set-pinentry-prompt! pinentry (match:substring regex-match 1)))
    regex-match))

(define (pinentry-getpin pinentry line)
  (let ((rofi "rofi -dmenu -input /dev/null -disable-history -lines 1 ~a -p ~s ~a ~s")
        (getpin-re (make-regexp "^GETPIN$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec getpin-re line))
      (let* ((rofi-cmd (format #f rofi
                              (if (pinentry-visibility pinentry) "" "-password")
                              (pinentry-prompt pinentry)
                              (if (string-empty? (pinentry-desc pinentry)) "" "-mesg")
                              (pinentry-desc pinentry)))
             (pipe (open-input-pipe rofi-cmd))
             (pass (get-string-all pipe))
             (status (close-pipe pipe)))
        (if status
            (unless (string-empty? pass)
              (format #t "D ~a" pass))
            (begin
              (format #t "ERR 83886179 Operation cancelled <rofi>\n")
              (set-pinentry-ok! pinentry #f)))))
    regex-match))

(define (pinentry-bye pinentry line)
  (let ((bye-re (make-regexp "^BYE"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec bye-re line))
      (exit #t))
    regex-match))

(define (pinentry-loop pinentry input-port)
  (let ((line (get-line input-port)))
    (unless (eof-object? line)
      (cond
       ((pinentry-option pinentry line))
       ((pinentry-getinfo pinentry line))
       ((pinentry-setkeyinfo pinentry line))
       ((pinentry-setdesc pinentry line))
       ((pinentry-setprompt pinentry line))
       ((pinentry-getpin pinentry line))
       ((pinentry-bye pinentry line))
       (#t (begin (format #t "BYE\n") (exit #f))))
      (pinentry-loop pinentry input-port))))

(let ((pinentry (make-pinentry #t "Passphrase:" "" #f)))
  (format #t "OK Please go ahead\n")
  (pinentry-loop pinentry (current-input-port))
  (when (pinentry-ok pinentry)
    (format #t "OK\n")))
