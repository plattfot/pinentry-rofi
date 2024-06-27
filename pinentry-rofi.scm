;; SPDX-FileCopyrightText: 2016 Quentin "Sardem FF7" Glidic
;; SPDX-FileCopyrightText: 2018-2023 Fredrik Salomonsson <plattfot@posteo.net>
;; SPDX-FileCopyrightText: 2024 Jakob Simon Kukla <jakob.kukla@gmail.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (pinentry-rofi)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1) ;; concatenate
  #:use-module (srfi srfi-9) ;; For records
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:export (make-pinentry
            pinentry?
            pinentry-ok set-pinentry-ok!
            pinentry-prompt set-pinentry-prompt!
            pinentry-desc set-pinentry-desc!
            pinentry-visibility set-pinentry-visibility!
            pinentry-display set-pinentry-display!
            pinentry-error set-pinentry-error!
            pinentry-title set-pinentry-title!
            pinentry-logfile set-pinentry-logfile!
            pinentry-ok-button set-pinentry-ok-button!
            pinentry-notok-button set-pinentry-notok-button!
            pinentry-cancel-button set-pinentry-cancel-button!
            pinentry-lc-ctype set-pinentry-lc-ctype!
            pinentry-lc-messages set-pinentry-lc-messages!
            pinentry-rofi-options

            remove-underline
            escape-underscore

            html-newline
            html-underline
            html-<
            string-empty?

            hex->char
            input-string
            pango-markup

            pinentry-set

            rofi-popup

            pinentry-option
            pinentry-getinfo
            pinentry-setkeyinfo
            pinentry-setok
            pinentry-setcancel
            pinentry-setnotok
            pinentry-setdesc
            pinentry-seterror
            pinentry-settitle
            pinentry-setprompt
            pinentry-getpin
            pinentry-confirm
            pinentry-bye
            pinentry-loop))

(when (equal? (system-file-name-convention) 'windows)
  (format #t "Only support posix systems!")
  (exit #f))

(define-record-type <pinentry>
  (make-pinentry ok prompt ok-button cancel-button display logfile lc-ctype lc-messages rofi-options)
  pinentry?
  (ok pinentry-ok set-pinentry-ok!)
  (prompt pinentry-prompt set-pinentry-prompt!)
  (desc pinentry-desc set-pinentry-desc!)
  (visibility pinentry-visibility set-pinentry-visibility!)
  (display pinentry-display set-pinentry-display!)
  (error pinentry-error set-pinentry-error!)
  (title pinentry-title set-pinentry-title!)
  (logfile pinentry-logfile set-pinentry-logfile!)
  (ok-button pinentry-ok-button set-pinentry-ok-button!)
  (notok-button pinentry-notok-button set-pinentry-notok-button!)
  (cancel-button pinentry-cancel-button set-pinentry-cancel-button!)
  (lc-ctype pinentry-lc-ctype set-pinentry-lc-ctype!)
  (lc-messages pinentry-lc-messages set-pinentry-lc-messages!)
  (rofi-options pinentry-rofi-options set-pinentry-rofi-options!))

(define-syntax-rule (set-and-return! val expr)
  "Set val to expr and return val."
  (begin (set! val expr) val))

(define (string-empty? str)
  "Evaluates to #t if string is empty."
  (string=? str ""))

(define (remove-underline str)
  "Replace _ followed by a character with just the character."
  (regexp-substitute/global #f "(^|%0A|&#10;|\n|[[:blank:]])_([[:alpha:]])" str
                            'pre 1 2 'post))

(define (escape-underscore str)
  "Replace __ followed by a character with _ and said character.
Always call this after `remove-underline' or
`html-underline'."
  (regexp-substitute/global #f "(^|%0A|&#10;|\n|[[:blank:]])__([[:alpha:]])" str
                            'pre 1 "_" 2 'post))

(define (html-newline str)
  "Replace %0A with &#10;"
  (regexp-substitute/global #f "%0A" str 'pre "&#10;" 'post))

(define (html-underline str)
  "Underscore followed by a character, underlines that character."
  (regexp-substitute/global #f "(^|%0A|&#10;|\n|[[:blank:]])_([[:alpha:]])" str
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
  (escape-underscore
   (html-underline
    (hex->char
     (html-<
      (html-newline str))))))

(define (input-string str)
  "Transform string to input for rofi.
Input strings does not support pango markup"
  (escape-underscore
   (remove-underline str)))

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
lc-ctype=C
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
     ((set-and-return! regex-match
                       (regexp-exec
                        (make-regexp "^OPTION[[:blank:]]+lc-ctype=(.+)$") line))
      (pinentry-set
       set-pinentry-lc-ctype!
       pinentry
       (match:substring regex-match 1)))
     ((set-and-return! regex-match
                       (regexp-exec
                        (make-regexp "^OPTION[[:blank:]]+lc-messages=(.+)$") line))
      (pinentry-set
       set-pinentry-lc-messages!
       pinentry
       (match:substring regex-match 1)))
     ((set-and-return! regex-match (regexp-exec option-re line))))
    regex-match))

(define* (pinentry-getinfo pinentry line #:key (port #t))
  "Process line if it starts with GETINFO"
  (let ((getinfo-re (make-regexp "^GETINFO (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec getinfo-re line))
      (let ((info (match:substring regex-match 1)))
        (cond
         ((string=? info "pid")
          (format port "D ~a~%~!" (getpid)))))
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

(define (pinentry-settitle pinentry line)
  "SETTITLE window title"
  (let ((settitle-re (make-regexp "^SETTITLE (.+)$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec settitle-re line))
      (pinentry-set-mesg
       set-pinentry-title!
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

(define* (rofi-popup #:key (env '())
                     visibility
                     title
                     (prompt ">")
                     message
                     buttons
                     only-match
                     (extra-options '()))
  "Run external program rofi and fetch the input from the user.

Keyword arguments:
TITLE: Text for the window title
PROMPT: Text for the prompt, default '>'
ENV: List of environemnt variables in the form (ENVVAR . VALUE)
VISIBILITY: If #t show the input
MESSAGE: Message for the popup window
BUTTONS: List of strings that will be buttons.
ONLY-MATCH: Only allow to match what is listed in the buttons.
EXTRA-OPTIONS: A list of extra options to pass to rofi.

Return the input from the user if succeeded else #f."
  (let* ((inputs (if buttons `("echo -e"
                               ,(format #f "~s" (string-join buttons "\n"))
                               "|")
                     '()))
         (rofi-sh `("env"
                    ,(string-join
                      (map (lambda (x) (format #f "~a=~s" (car x) (cdr x))) env))
                    ,(format #f "rofi -dmenu -disable-history -no-fixed-num-lines -l ~a -i"
                             (if (list? buttons) (length buttons) 1))
                    ,(if (and only-match buttons) "-only-match" "")
                    ,(if (not buttons) "-input /dev/null" "")
                    ,(if visibility "" "-password")
                    ,(if title (format #f "-window-title ~s" title) "")
                    ,(format #f "-p ~s" prompt)
                    ,(if message (format #f "-mesg ~s" message) "")
                    ,@(map (lambda (option) (format #f "~s" option)) extra-options)))
         (pipe (open-pipe (string-join (concatenate `(,inputs ,rofi-sh))) OPEN_READ))
         (pass (get-string-all pipe))
         (status (close-pipe pipe)))
    (if (and (equal? (status:exit-val status) 0)) pass #f)))

(define (compose-message pinentry)
  "Create the message by combining the error and desc from PINENTRY"
  (if (pinentry-error pinentry)
      (format #f "~a&#10;~a"
              (pinentry-error pinentry)
              (pinentry-desc pinentry))
      (pinentry-desc pinentry)))

(define* (pinentry-getpin pinentry line pin-program #:key (port #t))
  "Get pin using PIN-PROGRAM if LINE is equal to GETPIN."
  (let ((getpin-re (make-regexp "^GETPIN$"))
        (regex-match #f))
    (when (set-and-return! regex-match (regexp-exec getpin-re line))
      (let ((pass (pin-program #:title (pinentry-title pinentry)
                               #:prompt (pinentry-prompt pinentry)
                               #:message (compose-message pinentry)
                               #:visibility (pinentry-visibility pinentry)
                               #:env `(("DISPLAY" . ,(pinentry-display pinentry))
                                       ("LC_CTYPE" . ,(pinentry-lc-ctype pinentry))
                                       ("LC_MESSAGES" . ,(pinentry-lc-messages pinentry)))
                               #:extra-options (pinentry-rofi-options pinentry))))
        (if (and pass (not (string-empty? (string-trim-both pass))))
            (begin
              (format port "D ~a~!" pass)
              (set-pinentry-ok! pinentry #t))
            (begin
              (format port "ERR 83886179 Operation cancelled <rofi>~%~!")
              (set-pinentry-ok! pinentry #f)))))
    regex-match))

(define* (pinentry-confirm pinentry line confirm-program #:key (port #t))
  (let ((confirm-re (make-regexp "^CONFIRM$"))
        (confirm-one-button-re
         (make-regexp "^CONFIRM[[:blank:]]+--one-button[[:blank:]]*$"))
        (message-re (make-regexp "^MESSAGE$"))
        (regex-match #f))
    (cond
     ((set-and-return! regex-match (regexp-exec confirm-re line))
      ;; Can probably do this with a pipe in both direction, but
      ;; manual warns about deadlocks so sticking with this for now.
      (let ((button (confirm-program
                     #:env `(("DISPLAY" . ,(pinentry-display pinentry))
                             ("LC_CTYPE" . ,(pinentry-lc-ctype pinentry))
                             ("LC_MESSAGES" . ,(pinentry-lc-messages pinentry)))
                     #:visibility #t
                     #:only-match #t
                     #:buttons `(,(pinentry-ok-button pinentry)
                                 ,(or (pinentry-notok-button pinentry)
                                      (pinentry-cancel-button pinentry)))
                     #:message (compose-message pinentry)
                     #:extra-options (pinentry-rofi-options pinentry))))
        (if (and button
                 (string=? (string-trim-right button) (pinentry-ok-button pinentry)))
            (set-pinentry-ok! pinentry #t)
            (begin
              (format port "ERR 277 Operation cancelled~%~!")
              (set-pinentry-ok! pinentry #f)))))
     ((or (set-and-return! regex-match (regexp-exec confirm-one-button-re line))
          (set-and-return! regex-match (regexp-exec message-re line)))
      (let ((button (confirm-program
                     #:env `(("DISPLAY" . ,(pinentry-display pinentry))
                             ("LC_CTYPE" . ,(pinentry-lc-ctype pinentry))
                             ("LC_MESSAGES" . ,(pinentry-lc-messages pinentry)))
                     #:visibility #t
                     #:only-match #t
                     #:buttons `(,(pinentry-ok-button pinentry))
                     #:message (compose-message pinentry))))
        (if (and button
                 (string=? (string-trim-right button) (pinentry-ok-button pinentry)))
            (set-pinentry-ok! pinentry #t)
            (begin
              (format port "ERR 277 Operation cancelled~%~!")
              (set-pinentry-ok! pinentry #f))))))
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
       ((pinentry-getpin pinentry line rofi-popup))
       ((pinentry-confirm pinentry line rofi-popup))
       ((pinentry-seterror pinentry line))
       ((pinentry-settitle pinentry line))
       ((pinentry-bye pinentry line))
       (#t (begin
             (let ((log (pinentry-logfile pinentry)))
               (when (file-port? log)
                 (format log "Unknown command: ~s~%~!" line)))
              ;; GPG_ERR_ASS_UNKNOWN_CMD = 275,
             (format #t "ERR 275 Unknown command ~s~%~!" line)
             (set-pinentry-ok! pinentry #f))))
      (when (pinentry-ok pinentry)
        (format #t "OK~%~!"))
      (pinentry-loop pinentry input-port))))
