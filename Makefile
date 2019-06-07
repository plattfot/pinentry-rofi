
PREFIX ?=
PREFIX_BIN ?= $(PREFIX)/usr/bin

.PHONY: install
install:
	@install -DT pinentry-rofi.scm $(PREFIX_BIN)/pinentry-rofi-guile

