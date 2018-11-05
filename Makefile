
GUILE_SUPPORT = $(filter $(.FEATURES),guile)

ifeq ($(GUILE_SUPPORT),)
  $(error No guile support bailing out)
endif

GUILE_SITE_CCACHE=$(guile (%site-ccache-dir))
PREFIX ?=
PREFIX_BIN ?= $(PREFIX)/usr/bin
install:
	@install -d $(PREFIX_BIN) $(PREFIX)$(GUILE_SITE_CCACHE)
	@install -T rofi-pinentry.scm $(PREFIX_BIN)/pinentry-rofi
	@guild compile rofi-pinentry.scm -o $(PREFIX)$(GUILE_SITE_CCACHE)/pinentry-rofi.go
