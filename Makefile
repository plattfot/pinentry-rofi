
ifeq ($(filter $(.FEATURES),guile),)
  $(error No guile support bailing out)
endif

GUILE_SITE_CCACHE ?= $(guile (%site-ccache-dir))
PREFIX ?=
PREFIX_BIN ?= $(PREFIX)/usr/bin
install:
	@install -d $(PREFIX_BIN) $(PREFIX)$(GUILE_SITE_CCACHE)
	@install -T pinentry-rofi.scm $(PREFIX_BIN)/pinentry-rofi-guile
	@guild compile pinentry-rofi.scm -o $(PREFIX)$(GUILE_SITE_CCACHE)/pinentry-rofi-guile.go
