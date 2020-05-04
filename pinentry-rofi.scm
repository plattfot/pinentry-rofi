#! /usr/bin/guile \
--no-auto-compile -e main -s
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

(define pinentry-rofi-guile-version "0.5.0")

(when (equal? (system-file-name-convention) 'windows)
  (format #t "Only support posix systems!")
  (exit #f))

(define-record-type <pinentry>
  (make-pinentry ok prompt ok-button cancel-button display logfile)
  pinentry?
  (ok pinentry-ok set-pinentry-ok!)
  (prompt pinentry-prompt set-pinentry-prompt!)
  (desc pinentry-desc set-pinentry-desc!)
  (visibility pinentry-visibility set-pinentry-visibility!)
  (display pinentry-display set-pinentry-display!)
  (error pinentry-error set-pinentry-error!)
  (logfile pinentry-logfile set-pinentry-logfile!)
  (ok-button pinentry-ok-button set-pinentry-ok-button!)
  (notok-button pinentry-notok-button set-pinentry-notok-button!)
  (cancel-button pinentry-cancel-button set-pinentry-cancel-button!))

(define-syntax-rule (set-and-return! val expr)
  "Set val to expr and return val."
  (begin (set! val expr) val))

(define (string-empty? str)
  "Evaluates to #t if string is empty."
  (string=? str ""))

(define (pinentry-remove-underline str)
  "Replace _ followed by a character with just the character."
  (regexp-substitute/global #f "(^|[[:blank:]])_([[:alpha:]])" str
                            'pre 1 2 'post))

(define (pinentry-escape-underscore str)
  "Replace __ followed by a character with _ and said character.
Always call this after `pinentry-remove-underline' or
`html-underline'."
  (regexp-substitute/global #f "(^|[[:blank:]])__([[:alpha:]])" str
                            'pre 1 "_" 2 'post))

(define (html-newline str)
  "Replace %0A with &#10;"
  (regexp-substitute/global #f "%0A" str 'pre "&#10;" 'post))

(define (html-underline str)
  "Underscore followed by a character, underlines that character."
  (regexp-substitute/global #f "(^|[[:blank:]])_([[:alpha:]])" str
                            'pre 1"<u>"2"</u>" 'post))
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

(define (pango-markup str)
  "Transform string to pango."
  (hex->char
   (pinentry-escape-underscore
    (html-underline
     (html-<
      (html-newline str))))))

(define (input-string str)
  "Transform string to input for rofi.
Input strings does not support pango markup"
  (pinentry-escape-underscore
   (pinentry-remove-underline str)))

(define* (pinentry-set set-func pinentry label)
  "Using SET-FUNC, set the entry in PINENTRY to LABEL."
  (set-func pinentry label)
  (set-pinentry-ok! pinentry #t))

(define (pinentry-set-button set-func pinentry label)
  "Using SET-BUTTON-FUNC, set the entry in PINENTRY to LABEL.
LABEL will be transformed using `input-string'"
  (pinentry-set set-func pinentry (input-string label)))

(define (pinentry-set-mesg set-func pinentry label)
  "Using SET-FUNC, set the entry in PINENTRY to LABEL.
LABEL will be transformed using `pango-markup'"
  (pinentry-set set-func pinentry (pango-markup label)))

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
  (let ((regex-match #f)
        (option-re (make-regexp "^OPTION (.+)$")))
    (cond
     ((set-and-return! regex-match
                       (regexp-exec
                        (make-regexp "^OPTION[[:blank:]]+default-ok=(.+)$") line))
      (pinentry-set-button
       set-pinentry-ok-button!
       pinentry
       (match:substring regex-match 1)))
     ((set-and-return! regex-match
                       (regexp-exec
                        (make-regexp "^OPTION[[:blank:]]+default-cancel=(.+)$") line))
      (pinentry-set-button
       set-pinentry-cancel-button!
       pinentry
       (match:substring regex-match 1)))
     ((set-and-return! regex-match
                       (regexp-exec
                        (make-regexp "^OPTION[[:blank:]]+default-prompt=(.+)$") line))
      (pinentry-set-mesg
       set-pinentry-prompt!
       pinentry
       (match:substring regex-match 1)))
     ((set-and-return! regex-match (regexp-exec option-re line))))
    regex-match))

(define (pinentry-getinfo pinentry line)
  "Process line if it starts with GETINFO"
  (let ((getinfo-re (make-regexp "^GETINFO (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec getinfo-re line))
      (let ((info (match:substring regex-match 1)))
        (cond
         ((string=? info "pid")
          (format #t "D ~a\n" (getpid))
          (force-output))))
      (set-pinentry-ok! pinentry #t))
    regex-match))

(define (pinentry-setkeyinfo pinentry line)
  "SETKEYINFO s/FINGERPRINT"
  (let ((setkeyinfo-re (make-regexp "^SETKEYINFO (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec setkeyinfo-re line))
      (set-pinentry-ok! pinentry #t))
    regex-match))

(define (pinentry-setok pinentry line)
  "Set ok button label."
  (let ((setok-button-re (make-regexp "^SETOK (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec setok-button-re line))
      (pinentry-set-button
       set-pinentry-ok-button!
       pinentry
       (match:substring regex-match 1)))
    regex-match))

(define (pinentry-setcancel pinentry line)
  "Set cancel button label."
  (let ((setcancel-button-re (make-regexp "^SETCANCEL (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec setcancel-button-re line))
      (pinentry-set-button
       set-pinentry-cancel-button!
       pinentry
       (match:substring regex-match 1)))
    regex-match))

(define (pinentry-setnotok pinentry line)
  "Set notok button label."
  (let ((setnotok-button-re (make-regexp "^SETNOTOK (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec setnotok-button-re line))
      (pinentry-set-button
       set-pinentry-notok-button!
       pinentry
       (match:substring regex-match 1)))
    regex-match))

(define (pinentry-setdesc pinentry line)
  "SETDESC description"
  (let ((setdesc-re (make-regexp "^SETDESC (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec setdesc-re line))
      (pinentry-set-mesg
       set-pinentry-desc!
       pinentry
       (match:substring regex-match 1)))
    regex-match))

(define (pinentry-seterror pinentry line)
  "SETERROR MESSAGE"
  (let ((seterror-re (make-regexp "^SETERROR (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec seterror-re line))
      (pinentry-set-mesg
       set-pinentry-error!
       pinentry
       (match:substring regex-match 1)))
    regex-match))

(define (pinentry-setprompt pinentry line)
  "SETPROMPT Passphrase:"
  (let ((setprompt-re (make-regexp "^SETPROMPT (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec setprompt-re line))
      (pinentry-set-mesg
       set-pinentry-prompt!
       pinentry
       (match:substring regex-match 1)))
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
                    "-l" "1"
                    (if (pinentry-visibility pinentry) "" "-password")
                    "-p" (pinentry-prompt pinentry)
                    "-mesg" (if (pinentry-error pinentry)
                                (format #f "~a\n~a"
                                        (pinentry-error pinentry)
                                        (pinentry-desc pinentry))
                                (pinentry-desc pinentry))))
             (pass (get-string-all pipe))
             (status (close-pipe pipe)))
        (if (equal? (status:exit-val status) 0)
            (begin
              (unless (string-empty? pass)
                (format #t "D ~a" pass)
                (force-output))
              (set-pinentry-ok! pinentry #t))
            (begin
              (format #t "ERR 83886179 Operation cancelled <rofi>\n")
              (force-output)
              (set-pinentry-ok! pinentry #f)))))
    regex-match))

(define (pinentry-confirm pinentry line)
  (let ((getpin-re (make-regexp "^CONFIRM$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec getpin-re line))
      ;; Can probably do this with a pipe in both direction, but
      ;; manual warns about deadlocks so sticking with this for now.
      (let* ((pipe (open-pipe
                    (string-join
                     `("echo -e "
                       ,(format #f "'~a\n~a'"
                                (pinentry-ok-button pinentry)
                                (or (pinentry-notok-button pinentry)
                                    (pinentry-cancel-button pinentry)))
                       "|"
                       ,(format #f "env DISPLAY=~a" (pinentry-display pinentry))
                       "rofi -dmenu -disable-history -only-match -l 2 -i"
                       ,(format #f "-p '>'")
                       ,(format #f "-mesg ~s" (if (pinentry-error pinentry)
                                                  (format #f "~a\n~a"
                                                          (pinentry-error pinentry)
                                                          (pinentry-desc pinentry))
                                                  (pinentry-desc pinentry)))))
                    OPEN_READ))
             (pass (get-string-all pipe))
             (status (close-pipe pipe)))
        (if (and (equal? (status:exit-val status) 0)
                 (string=? (string-trim-right pass) (pinentry-ok-button pinentry)))
            (set-pinentry-ok! pinentry #t)
            (begin
              (format #t "ERR 277 Operation cancelled\n")
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
       ((pinentry-setok pinentry line))
       ((pinentry-setnotok pinentry line))
       ((pinentry-setcancel pinentry line))
       ((pinentry-setprompt pinentry line))
       ((pinentry-getpin pinentry line))
       ((pinentry-confirm pinentry line))
       ((pinentry-seterror pinentry line))
       ((pinentry-bye pinentry line))
       (#t (begin
             (let ((log (pinentry-logfile pinentry)))
               (when (file-port? log)
                 (format log "Unknown command: ~s\n" line)
                 (force-output log)))
              ;; GPG_ERR_ASS_UNKNOWN_CMD = 275,
             (format #t "ERR 275 Unknown command ~s\n" line)
             (force-output)
             (set-pinentry-ok! pinentry #f))))
      (when (pinentry-ok pinentry)
        (format #t "OK\n")
        (force-output))
      (pinentry-loop pinentry input-port))))

(define (main args)
  (let* ((option-spec
          '((display (single-char #\d) (value #t))
            (xauthority (single-char #\a) (value #t))
            (version (single-char #\v) (value #f))
            (log (value #t))
            (help (single-char #\h) (value #f))))
         (default-display ":0")
         (options (getopt-long (command-line) option-spec))
         (pinentry (make-pinentry #t "Passphrase:" "Ok" "Cancel"
                                  (option-ref options 'display default-display)
                                  (let ((logfile (option-ref options 'log #f)))
                                    (when logfile
                                      (open-output-file
                                       (format #f "~a.~a" logfile (getpid))))))))
    (when (option-ref options 'help #f)
      (format #t "\
Usage: ~a [OPTIONS]
Options:
  -d, --display DISPLAY Set display, default is ~s.
      --log LOGFILE     Log unknown commands to LOGFILE
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
    (pinentry-loop pinentry (current-input-port))))
