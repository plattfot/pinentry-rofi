(use-modules
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix git-download)
  (guix build-system gnu)
  (gnu packages)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo)
  (gnu packages xdisorg))

(package
  (name "pinentry-rofi")
  (version "2.0.1")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/plattfot/pinentry-rofi/")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256 (base32 "044bnldz7k74s873jwsjgff176l1jsvpbaka7d1wcj8b5pwqv2av"))))
  (build-system gnu-build-system)
  (arguments
    `(#:modules
      ((ice-9 match)
       (ice-9 ftw)
       ,@%gnu-build-system-modules)
      #:phases
      (modify-phases
        %standard-phases
        (add-after
          'install
          'hall-wrap-binaries
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((compiled-dir
                     (lambda (out version)
                       (string-append
                         out
                         "/lib/guile/"
                         version
                         "/site-ccache")))
                   (uncompiled-dir
                     (lambda (out version)
                       (string-append
                         out
                         "/share/guile/site"
                         (if (string-null? version) "" "/")
                         version)))
                   (dep-path
                     (lambda (env modules path)
                       (list env
                             ":"
                             'prefix
                             (cons modules
                                   (map (lambda (input)
                                          (string-append
                                            (assoc-ref inputs input)
                                            path))
                                        ,''("rofi"))))))
                   (out (assoc-ref outputs "out"))
                   (bin (string-append out "/bin/"))
                   (site (uncompiled-dir out "")))
              (match (scandir site)
                     (("." ".." version)
                      (for-each
                        (lambda (file)
                          (wrap-program
                            (string-append bin file)
                            (dep-path
                              "GUILE_LOAD_PATH"
                              (uncompiled-dir out version)
                              (uncompiled-dir "" version))
                            (dep-path
                              "GUILE_LOAD_COMPILED_PATH"
                              (compiled-dir out version)
                              (compiled-dir "" version))))
                        ,''("pinentry-rofi"))
                      #t))))))))
  (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)
      ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-3.0)))
  (propagated-inputs `(("rofi" ,rofi)))
  (synopsis "Rofi frontend to pinentry")
  (description
    "Simple pinentry gui using rofi, it is written in GNU guile.")
  (home-page
    "https://github.com/plattfot/pinentry-rofi/")
  (license license:gpl3+))

