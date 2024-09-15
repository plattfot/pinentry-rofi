dnl SPDX-FileCopyrightText: 2023-2024 Fredrik Salomonsson <plattfot@posteo.net>
dnl
dnl SPDX-License-Identifier: CC0-1.0

dnl -*- Autoconf -*-

AC_INIT(pinentry-rofi, 3.0.0)
AC_SUBST(HVERSION, "\"3.0.0\"")
AC_SUBST(AUTHOR, "\"Fredrik Salomonsson\"")
AC_SUBST(COPYRIGHT, "'(2020 2021 2022 2023 2024)")
AC_SUBST(LICENSE, gpl3+)
AC_CONFIG_SRCDIR(pinentry-rofi.scm)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects  color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([scripts/pinentry-rofi],[chmod +x scripts/pinentry-rofi])
dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS
GUILE_SITE_DIR
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

dnl Hall auto-generated guile-module dependencies


dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AX_COMPUTE_RELATIVE_PATHS([bindir:guilemoduledir:GUILE_MODULE_DIR bindir:guileobjectdir:GUILE_OBJECT_DIR])

AC_SUBST([GUILE_MODULE_DIR])
AC_SUBST([GUILE_OBJECT_DIR])

AC_OUTPUT
