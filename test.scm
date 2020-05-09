#!/usr/bin/guile \
--no-auto-compile -l pinentry-rofi.scm -s
!#

;;  Copyright © 2020 Fredrik "PlaTFooT" Salomonsson
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


(use-modules (srfi srfi-64)
             (ice-9 popen)
             (ice-9 textual-ports)
             (pinentry-rofi))

(test-begin "pinentry-rofi")
(test-assert (string? pinentry-rofi-guile-version))

(test-begin "pinentry")
(let ((pinentry (make-pinentry #t "Prompt" "Ok" "Cancel" ":1" "test.log")))
  (test-assert (pinentry? pinentry))
  (test-assert (pinentry-ok pinentry))
  (test-equal "Prompt" (pinentry-prompt pinentry))
  (test-equal "Ok" (pinentry-ok-button pinentry))
  (test-equal "Cancel" (pinentry-cancel-button pinentry))
  (test-equal ":1" (pinentry-display pinentry))
  (test-equal "test.log" (pinentry-logfile pinentry))
  (test-assert (not (pinentry-notok-button pinentry)))
  (test-assert (not (pinentry-visibility pinentry)))
  (test-assert (not (pinentry-error pinentry))))

(let ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log")))
  (test-assert (not (pinentry-ok pinentry))))

(test-end "pinentry")

(test-begin "utils")

(test-equal "Ok" (remove-underline "_Ok"))
(test-equal " Ok" (remove-underline " _Ok"))
(test-equal "foo_bar" (remove-underline "foo_bar"))
(test-equal "foo__bar" (remove-underline "foo__bar"))
(test-equal " __Ok" (remove-underline " __Ok"))
(test-equal "__Ok" (remove-underline "__Ok"))
(test-equal "Ok%0ACancel" (remove-underline "_Ok%0A_Cancel"))
(test-equal "Ok&#10;Cancel" (remove-underline "_Ok&#10;_Cancel"))

(test-equal "_Ok" (escape-underscore "_Ok"))
(test-equal " _Ok" (escape-underscore " _Ok"))
(test-equal "foo_bar" (escape-underscore "foo_bar"))
(test-equal "foo__bar" (escape-underscore "foo__bar"))
(test-equal " _Ok" (escape-underscore " __Ok"))
(test-equal "_Ok" (escape-underscore "__Ok"))
(test-equal "_Ok%0A_Cancel" (escape-underscore "__Ok%0A__Cancel"))
(test-equal "_Ok&#10;_Cancel" (escape-underscore "__Ok&#10;__Cancel"))

(test-end "utils")

(test-equal "This is one line\nThis is another%OA"
  (hex->char "%54his is one line%0AThis is another%OA"))

(test-begin "html")
(test-equal "%54his is one line&#10;This is another%OA"
  (html-newline "%54his is one line%0AThis is another%OA"))
(test-equal "%54his is one line\nThis is another%OA"
  (html-newline "%54his is one line\nThis is another%OA"))

(test-equal "<u>O</u>k" (html-underline "_Ok"))
(test-equal " <u>O</u>k" (html-underline " _Ok"))
(test-equal "foo_bar" (html-underline "foo_bar"))
(test-equal "foo__bar" (html-underline "foo__bar"))
(test-equal " __Ok" (html-underline " __Ok"))
(test-equal "__Ok" (html-underline "__Ok"))
(test-equal "<u>O</u>k%0A<u>C</u>ancel" (html-underline "_Ok%0A_Cancel"))
(test-equal "<u>O</u>k&#10;<u>C</u>ancel" (html-underline "_Ok&#10;_Cancel"))

(test-end "html")

(test-equal "<u>T</u>his is one line&#10;<u>T</u>his is another%OA"
  (pango-markup "_%54his is one line%0A_This is another%OA"))

(test-equal "Ok\nCancel"
  (input-string "_Ok\nCancel"))

(test-equal "Ok\n_Cancel"
  (input-string "_Ok\n__Cancel"))

(let ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log")))
  (pinentry-set set-pinentry-notok-button! pinentry "Not ok")
  (test-equal "Not ok" (pinentry-notok-button pinentry))
  (test-assert (pinentry-ok pinentry)))

(let ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log")))
  (test-error #t (pinentry-set pinentry-notok-button pinentry "Not ok")))

(let ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log")))
  (test-assert (pinentry-option pinentry "OPTION default-ok=Okay"))
  (test-equal "Okay" (pinentry-ok-button pinentry))
  (test-equal "Cancel" (pinentry-cancel-button pinentry))
  (test-equal "Prompt" (pinentry-prompt pinentry))
  (test-assert (pinentry-ok pinentry)))

(let ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log")))
  (test-assert (pinentry-option pinentry "OPTION default-cancel=No"))
  (test-equal "Ok" (pinentry-ok-button pinentry))
  (test-equal "No" (pinentry-cancel-button pinentry))
  (test-equal "Prompt" (pinentry-prompt pinentry))
  (test-assert (pinentry-ok pinentry)))

(let ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log")))
  (test-assert (pinentry-option pinentry "OPTION default-prompt=Password:"))
  (test-equal "Ok" (pinentry-ok-button pinentry))
  (test-equal "Cancel" (pinentry-cancel-button pinentry))
  (test-equal "Password:" (pinentry-prompt pinentry))
  (test-assert (pinentry-ok pinentry)))

(let ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log")))
  (test-assert (pinentry-option pinentry "OPTION foo bar")))

(let ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log")))
  (test-assert (not (pinentry-option pinentry " OPTION foo bar")))
  (test-assert (not (pinentry-option pinentry "OPTION")))
  (test-assert (not (pinentry-option pinentry "Foo"))))

(let* ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log"))
       (output "")
       (fake-port (make-soft-port
                  (vector
                   (lambda (c) (set! output (string-append output c)))
                   (lambda (s) (set! output (string-append output s)))
                   (lambda () #t)
                   #f
                   (lambda () #t))
                  "w")))
  (with-output-to-port
      fake-port
    (lambda ()
      (test-assert (pinentry-getinfo pinentry "GETINFO pid"))
      (test-equal (format #f "D ~a\n" (getpid)) output)))

  (test-assert (pinentry-getinfo pinentry "GETINFO foo bar"))
  (test-assert (not (pinentry-getinfo pinentry " GETINFO foo bar")))
  (test-assert (not (pinentry-getinfo pinentry "GETINFO")))
  (test-assert (not (pinentry-getinfo pinentry "Foo"))))

(let* ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log")))
  (test-assert (pinentry-setkeyinfo pinentry "SETKEYINFO Foo"))
  (test-assert (pinentry-ok pinentry))
  (test-assert (not (pinentry-setkeyinfo pinentry " SETKEYINFO foo bar")))
  (test-assert (not (pinentry-setkeyinfo pinentry "SETKEYINFO")))
  (test-assert (not (pinentry-setkeyinfo pinentry "Foo"))))

(let* ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log")))
  (test-assert (pinentry-setok pinentry "SETOK Foo"))
  (test-equal "Foo" (pinentry-ok-button pinentry))
  (test-assert (pinentry-ok pinentry))
  (test-assert (pinentry-setok pinentry "SETOK _Ok okay"))
  (test-equal "Ok okay" (pinentry-ok-button pinentry))
  (test-assert (not (pinentry-setok pinentry " SETOK foo bar")))
  (test-equal "Ok okay" (pinentry-ok-button pinentry))
  (test-assert (not (pinentry-setok pinentry "SETOK")))
  (test-equal "Ok okay" (pinentry-ok-button pinentry))
  (test-assert (not (pinentry-setok pinentry "Foo")))
  (test-equal "Ok okay" (pinentry-ok-button pinentry)))

(let* ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log")))
  (test-assert (pinentry-setnotok pinentry "SETNOTOK Foo"))
  (test-equal "Foo" (pinentry-notok-button pinentry))
  (test-assert (pinentry-ok pinentry))
  (test-assert (pinentry-setnotok pinentry "SETNOTOK Not _Ok"))
  (test-equal "Not Ok" (pinentry-notok-button pinentry))
  (test-assert (not (pinentry-setnotok pinentry " SETNOTOK foo bar")))
  (test-equal "Not Ok" (pinentry-notok-button pinentry))
  (test-assert (not (pinentry-setnotok pinentry "SETNOTOK")))
  (test-equal "Not Ok" (pinentry-notok-button pinentry))
  (test-assert (not (pinentry-setnotok pinentry "Foo")))
  (test-equal "Not Ok" (pinentry-notok-button pinentry)))

(let* ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log")))
  (test-assert (pinentry-setcancel pinentry "SETCANCEL Foo"))
  (test-equal "Foo" (pinentry-cancel-button pinentry))
  (test-assert (pinentry-ok pinentry))
  (test-assert (pinentry-setcancel pinentry "SETCANCEL _Abort"))
  (test-equal "Abort" (pinentry-cancel-button pinentry))
  (test-assert (not (pinentry-setcancel pinentry " SETCANCEL foo bar")))
  (test-equal "Abort" (pinentry-cancel-button pinentry))
  (test-assert (not (pinentry-setcancel pinentry "SETCANCEL")))
  (test-equal "Abort" (pinentry-cancel-button pinentry))
  (test-assert (not (pinentry-setcancel pinentry "Foo")))
  (test-equal "Abort" (pinentry-cancel-button pinentry)))

(let* ((pinentry (make-pinentry #f "Prompt" "Ok" "Desc" ":1" "test.log")))
  (test-assert (pinentry-setdesc pinentry "SETDESC Foo"))
  (test-equal "Foo" (pinentry-desc pinentry))
  (test-assert (pinentry-ok pinentry))
  (test-assert (pinentry-setdesc pinentry "SETDESC _%54his is a description%0A_On two lines"))
  (test-equal "<u>T</u>his is a description&#10;<u>O</u>n two lines" (pinentry-desc pinentry))
  (test-assert (not (pinentry-setdesc pinentry " SETDESC foo bar")))
  (test-equal "<u>T</u>his is a description&#10;<u>O</u>n two lines" (pinentry-desc pinentry))
  (test-assert (not (pinentry-setdesc pinentry "SETDESC")))
  (test-equal "<u>T</u>his is a description&#10;<u>O</u>n two lines" (pinentry-desc pinentry))
  (test-assert (not (pinentry-setdesc pinentry "Foo")))
  (test-equal "<u>T</u>his is a description&#10;<u>O</u>n two lines" (pinentry-desc pinentry)))

(let* ((pinentry (make-pinentry #f "Prompt" "Ok" "Error" ":1" "test.log")))
  (test-assert (pinentry-seterror pinentry "SETERROR Foo"))
  (test-equal "Foo" (pinentry-error pinentry))
  (test-assert (pinentry-ok pinentry))
  (test-assert (pinentry-seterror pinentry "SETERROR _%54his is an error%0A_On two lines"))
  (test-equal "<u>T</u>his is an error&#10;<u>O</u>n two lines" (pinentry-error pinentry))
  (test-assert (not (pinentry-seterror pinentry " SETERROR foo bar")))
  (test-equal "<u>T</u>his is an error&#10;<u>O</u>n two lines" (pinentry-error pinentry))
  (test-assert (not (pinentry-seterror pinentry "SETERROR")))
  (test-equal "<u>T</u>his is an error&#10;<u>O</u>n two lines" (pinentry-error pinentry))
  (test-assert (not (pinentry-seterror pinentry "Foo")))
  (test-equal "<u>T</u>his is an error&#10;<u>O</u>n two lines" (pinentry-error pinentry)))

(let* ((pinentry (make-pinentry #f "Prompt" "Ok" "Prompt" ":1" "test.log")))
  (test-assert (pinentry-setprompt pinentry "SETPROMPT Foo"))
  (test-equal "Foo" (pinentry-prompt pinentry))
  (test-assert (pinentry-ok pinentry))
  (test-assert (pinentry-setprompt pinentry "SETPROMPT _%54his is a prompt%0A_On two lines"))
  (test-equal "<u>T</u>his is a prompt&#10;<u>O</u>n two lines" (pinentry-prompt pinentry))
  (test-assert (not (pinentry-setprompt pinentry " SETPROMPT foo bar")))
  (test-equal "<u>T</u>his is a prompt&#10;<u>O</u>n two lines" (pinentry-prompt pinentry))
  (test-assert (not (pinentry-setprompt pinentry "SETPROMPT")))
  (test-equal "<u>T</u>his is a prompt&#10;<u>O</u>n two lines" (pinentry-prompt pinentry))
  (test-assert (not (pinentry-setprompt pinentry "Foo")))
  (test-equal "<u>T</u>his is a prompt&#10;<u>O</u>n two lines" (pinentry-prompt pinentry)))

(let* ((pinentry (make-pinentry #f "Prompt" "Ok" "Prompt" ":1" "test.log")))
  (test-error "(quit #t)" (pinentry-bye pinentry "BYE"))
  (test-assert (not (pinentry-bye pinentry "Hej då"))))

(let* ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log"))
       (output "")
       (fake-port (make-soft-port
                  (vector
                   (lambda (c) (set! output (string-append output c)))
                   (lambda (s) (set! output (string-append output s)))
                   (lambda () #t)
                   #f
                   (lambda () #t))
                  "w"))
       (description "This is a description")
       (error "Something went wrong")
       (display ":1"))
  (set-pinentry-desc! pinentry description)
  (set-pinentry-display! pinentry display)
  (with-output-to-port
      fake-port
    (lambda ()
      (test-assert (pinentry-getpin pinentry "GETPIN"
                                    (lambda* (#:key (env '())
                                                    visibility
                                                    (prompt ">")
                                                    message
                                                    buttons
                                                    only-match)
                                      (test-equal "Prompt" prompt)
                                      (test-equal description message)
                                      (test-assert (not visibility))
                                      (test-assert (not only-match))
                                      (test-assert (not buttons))
                                      (test-equal `(("DISPLAY" . ,display)) env)
                                      "password")))
      (test-equal (format #f "D password") output)))

  (set-pinentry-error! pinentry error)
  (set! output "")
  (with-output-to-port
      fake-port
    (lambda ()
      (test-assert (pinentry-getpin pinentry "GETPIN"
                                    (lambda* (#:key (env '())
                                                    visibility
                                                    (prompt ">")
                                                    message
                                                    buttons
                                                    only-match)
                                      (test-equal "Prompt" prompt)
                                      (test-equal (format #f "~a\n~a" error description)
                                        message)
                                      (test-assert (not visibility))
                                      (test-assert (not only-match))
                                      (test-assert (not buttons))
                                      (test-equal `(("DISPLAY" . ,display)) env)
                                      "password")))
      (test-equal (format #f "D password") output)))

  (test-assert (not (pinentry-getinfo pinentry " GETPIN")))
  (test-assert (not (pinentry-getinfo pinentry "Foo"))))

(let* ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log"))
       (description "This is a description")
       (error "Something went wrong")
       (display ":1"))
  (set-pinentry-desc! pinentry description)
  (set-pinentry-display! pinentry display)
  (test-assert (pinentry-confirm pinentry "CONFIRM"
                                (lambda* (#:key (env '())
                                                visibility
                                                (prompt ">")
                                                message
                                                buttons
                                                only-match)
                                  (test-equal ">" prompt)
                                  (test-equal description message)
                                  (test-assert visibility)
                                  (test-assert only-match)
                                  (test-equal `("Ok" "Cancel") buttons)
                                  (test-equal `(("DISPLAY" . ,display)) env)
                                  "Ok")))
  (test-assert (pinentry-ok pinentry))
  (set-pinentry-error! pinentry error)
  (let* ((output "")
         (fake-port (make-soft-port
                     (vector
                      (lambda (c) (set! output (string-append output c)))
                      (lambda (s) (set! output (string-append output s)))
                      (lambda () #t)
                      #f
                      (lambda () #t))
                     "w")))
    (with-output-to-port
        fake-port
      (lambda ()
        (test-assert (pinentry-confirm
                      pinentry
                      "CONFIRM"
                      (lambda* (#:key (env '())
                                      visibility
                                      (prompt ">")
                                      message
                                      buttons
                                      only-match)
                        (test-equal ">" prompt)
                        (test-equal (format #f "~a\n~a" error description)
                          message)
                        (test-assert visibility)
                        (test-assert only-match)
                        (test-equal `("Ok" "Cancel") buttons)
                        (test-equal `(("DISPLAY" . ,display)) env)
                        "Cancel")))
        (test-equal (format #f "ERR 277 Operation cancelled\n") output))))
  (test-assert (not (pinentry-ok pinentry)))
  (test-assert (not (pinentry-getinfo pinentry " CONFIRM")))
  (test-assert (not (pinentry-getinfo pinentry "Foo"))))

(let* ((pinentry (make-pinentry #f "Prompt" "Ok" "Cancel" ":1" "test.log"))
       (description "This is a description")
       (display ":1"))
  (set-pinentry-desc! pinentry description)
  (set-pinentry-display! pinentry display)
  (test-assert (pinentry-confirm pinentry "CONFIRM --one-button"
                                (lambda* (#:key (env '())
                                                visibility
                                                (prompt ">")
                                                message
                                                buttons
                                                only-match)
                                  (test-equal ">" prompt)
                                  (test-equal description message)
                                  (test-assert visibility)
                                  (test-assert only-match)
                                  (test-equal `("Ok") buttons)
                                  (test-equal `(("DISPLAY" . ,display)) env)
                                  "Ok")))
  (test-assert (pinentry-ok pinentry))
  (set-pinentry-ok! pinentry #f)
  (test-assert (pinentry-confirm pinentry "MESSAGE"
                                 (lambda* (#:key (env '())
                                                 visibility
                                                 (prompt ">")
                                                 message
                                                 buttons
                                                 only-match)
                                   (test-equal ">" prompt)
                                   (test-equal description message)
                                   (test-assert visibility)
                                   (test-assert only-match)
                                   (test-equal `("Ok") buttons)
                                   (test-equal `(("DISPLAY" . ,display)) env)
                                   "Ok")))
  (test-assert (pinentry-ok pinentry))
  (test-assert (not (pinentry-getinfo pinentry " CONFIRM --one-button")))
  (test-assert (not (pinentry-getinfo pinentry "MESSAGE --one-button")))
  (test-assert (not (pinentry-getinfo pinentry " MESSAGE")))
  (test-assert (not (pinentry-getinfo pinentry "Foo"))))

(test-end "pinentry-rofi")
