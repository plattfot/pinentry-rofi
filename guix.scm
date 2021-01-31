(use-modules
  (guix packages)
  (guix git-download)
  (guix gexp)
  (guix build-system gnu)
  ((guix licenses) #:prefix license:)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages pkg-config)
  (gnu packages texinfo)
  (gnu packages xdisorg)
  (ice-9 popen)
  (ice-9 rdelim)
  )

;; From the talk "Just build it with Guix" by Efraim Flashner
;; presented on the Guix days 2020
;; https://guix.gnu.org/en/blog/2020/online-guix-day-announce-2/
(define %source-dir (dirname (current-filename)))

(define %git-commit
  (read-string (open-pipe "git show HEAD | head -1 | cut -d ' ' -f2" OPEN_READ)))

(define (skip-git-directory file stat)
  "Skip the `.git` directory when collecting the source."
  (not (string=? (basename file) ".git")))

(define-public pinentry-rofi
  (package
    (name "pinentry-rofi")
    (version "2.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/plattfot/pinentry-rofi/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "044bnldz7k74s873jwsjgff176l1jsvpbaka7d1wcj8b5pwqv2av"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((ice-9 match)
        (ice-9 ftw)
        ,@%gnu-build-system-modules)
       #:phases
       (modify-phases
           %standard-phases
         (add-after 'install 'hall-wrap-binaries
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (site (string-append out "/share/guile/site"))
                    (rofi-bin (string-append (assoc-ref inputs "rofi") "/bin")))
               (match (scandir site)
                 (("." ".." version)
                  (wrap-program
                      (string-append bin "pinentry-rofi")
                    (list "PATH" ":" 'prefix `(,rofi-bin)))
                  #t)))))
         (add-after 'compress-documentation 'installcheck
           (lambda* rest
             (invoke "make" "installcheck"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("autoconf-archive" ,autoconf-archive)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))
    (inputs `(("guile" ,guile-3.0)
              ("rofi" ,rofi)))
    (synopsis "Rofi GUI for GnuPG's passphrase input")
    (description "Pinentry-rofi is a simple graphical user interface for
passphrase or PIN when required by @code{gpg} or other software.  It is using
the Rofi application launcher as the user interface.  Which makes it combined
with @code{rofi-pass} a good front end for @code{password-store}.")
    (home-page "https://github.com/plattfot/pinentry-rofi/")
    (license license:gpl3+)))
(package
  (inherit pinentry-rofi)
  (name "pinentry-rofi-git")
  (version (git-version (package-version pinentry-rofi) "HEAD" %git-commit))
  (source (local-file %source-dir
                      #:recursive? #t
                      #:select? skip-git-and-build-directory)))
