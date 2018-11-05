
# Description
Based on [this gist](https://gist.github.com/sardemff7/759cbf956bea20d382a6128c641d2746)

Simple pinentry gui using rofi written in GNU guile.

It's similar in functionality as the gist but this one doesn't force
the width to be 27.

I also wanted a small project which I could use to practice writing
GNU guile.

# Install
## From source

```bash
$ git clone git@github.com:plattfot/pinentry-rofi.git
$ cd pinentry-rofi
$ make PREFIX=<install dir>
```

Where <install dir> is where you want to install it. By default it
will install the script in /usr/bin and the compiled source to where
the site cache for guile is configured to.

## Arch Linux
Clone my aur reop and the build the package using the PKGBUILD:

```bash
$ git clone git@github.com:plattfot/pinentry-rofi-aur.git
$ cd pinentry-rofi-aur
$ makepkg -ic
```
