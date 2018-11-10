#! /usr/bin/guile -s
!#

;;  Copyright © 2016 Quentin "Sardem FF7" Glidic
;;  Copyright © 2018 Fredrik "PlaTFooT" Salomonsson
;;
;;  Permission is hereby granted, free of charge, to any person obtaining a copy
;;  of this software and associated documentation files (the "Software"), to deal
;;  in the Software without restriction, including without limitation the rights
;;  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;  copies of the Software, and to permit persons to whom the Software is
;;  furnished to do so, subject to the following conditions:
;;
;;  The above copyright notice and this permission notice shall be included in
;;  all copies or substantial portions of the Software.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;  THE SOFTWARE.

(use-modules
 (ice-9 popen)
 (ice-9 textual-ports)
 (srfi srfi-9) ;; For records
 (ice-9 format)
 (ice-9 regex)
 (ice-9 getopt-long))

(define pinentry-rofi-guile-version "0.3.0")

(when (equal? (system-file-name-convention) 'windows)
  (format #t "Only support posix systems!")
  (exit #f))

(define-record-type <pinentry>
  (make-pinentry ok prompt desc visibility display)
  pinentry?
  (ok pinentry-ok set-pinentry-ok!)
  (prompt pinentry-prompt set-pinentry-prompt!)
  (desc pinentry-desc set-pinentry-desc!)
  (visibility pinentry-visibility set-pinentry-visibility!)
  (display pinentry-display set-pinentry-display!))

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
          (format #t "D ~a\n" (getpid))
          (force-output)))))
    regex-match))

(define (pinentry-setkeyinfo pinentry line)
  "SETKEYINFO s/FINGERPRINT"
  (let ((setkeyinfo-re (make-regexp "^SETKEYINFO (.+)$")))
    (regexp-exec setkeyinfo-re line)))

(define (html-< str)
  "Replace < with &lt;"
  (regexp-substitute/global #f "<" str 'pre "&lt;" 'post))

(define (hex->char str)
  "Replace matching'%XX' where X ∈ {0-F} with their respective char."
  (regexp-substitute/global
   #f "%([[:xdigit:]]{2})" str 'pre
   (lambda (m) (integer->char
                (string->number
                 (match:substring m 1) 16))) 'post))

(define (pinentry-setdesc pinentry line)
  "SETDESC Please enter the passphrase for the ssh key%0A  ke:yf:in:ge:rp:ri:nt"
  (let ((setdesc-re (make-regexp "^SETDESC (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec setdesc-re line))
      (let ((mesg (hex->char (html-< (match:substring regex-match 1)))))
        (set-pinentry-desc! pinentry mesg)))
    regex-match))

(define (pinentry-setprompt pinentry line)
  "SETPROMPT Passphrase:"
  (let ((setprompt-re (make-regexp "^SETPROMPT (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec setprompt-re line))
      (set-pinentry-prompt! pinentry (match:substring regex-match 1)))
    regex-match))

(define (pinentry-getpin pinentry line)
  (let ((getpin-re (make-regexp "^GETPIN$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec getpin-re line))
      (let* ((pipe (open-pipe*
                    OPEN_READ
                    "env"
                    (format #f "DISPLAY=~a" (pinentry-display pinentry))
                    "rofi"
                    "-dmenu" 
                    "-input" "/dev/null" 
                    "-disable-history"
                    "-lines" "1" 
                    (if (pinentry-visibility pinentry) "" "-password")
                    "-p" (pinentry-prompt pinentry) 
                    "-mesg" (pinentry-desc pinentry)))
             (pass (get-string-all pipe))
             (status (close-pipe pipe)))
        (if (equal? (status:exit-val status) 0)
            (unless (string-empty? pass)
              (format #t "D ~a" pass)
              (force-output))
            (begin
              (format #t "ERR 83886179 Operation cancelled <rofi>\n")
              (force-output)
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
       (#t (begin
             (format #t "BYE\n")
             (force-output)
             (exit #f))))
      (when (pinentry-ok pinentry)
        (format #t "OK\n")
        (force-output))
      (pinentry-loop pinentry input-port))))

(let* ((option-spec
        '((display (single-char #\d) (value #t))
          (xauthority (single-char #\a) (value #t))
          (version (single-char #\v) (value #f))
          (help (single-char #\h) (value #f))))
       (default-display ":0")
       (options (getopt-long (command-line) option-spec))
       (pinentry (make-pinentry #t "Passphrase:" "" #f
                                (option-ref options 'display default-display))))
  (when (option-ref options 'help #f)
    (format #t "\
Usage: ~a [OPTIONS]
Options:
  -d, --display DISPLAY Set display, default is ~s.
  -v, --version         Display version.
  -h, --help            Display this help.
Author:
Fredrik \"PlaTFooT\" Salomonsson
"
(car (command-line))
default-display)
    (exit #t))
  (when (option-ref options 'version #f)
    (format #t "pinentry-rofi-guile version ~a\n" pinentry-rofi-guile-version)
    (exit #t))
  (format #t "OK Please go ahead\n")
  (force-output)
  (pinentry-loop pinentry (current-input-port)))
