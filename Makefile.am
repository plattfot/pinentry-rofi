

bin_SCRIPTS = scripts/pinentry-rofi

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]GUILE_OBJECT_DIR[@],$(prefix)/$(GUILE_OBJECT_DIR),g'	\
  -e 's,[@]GUILE_MODULE_DIR[@],$(exec_prefix)/$(GUILE_MODULE_DIR),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

nodist_noinst_SCRIPTS = pre-inst-env

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

SOURCES = pinentry-rofi.scm

TESTS = tests/pinentry-rofi.scm

TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main            \
      $(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not indivdual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

info_TEXINFOS = doc/pinentry-rofi.texi
dvi: # Don't build dvi docs

EXTRA_DIST += README.org \
              README \
              AUTHORS \
              NEWS \
              HACKING \
              COPYING \
              ChangeLog \
              guix.scm \
              hall.scm \
              build-aux/test-driver.scm \
              $(TESTS)

ACLOCAL_AMFLAGS = -I m4

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\
  $(GOBJECTS)					\
  $(TESTS:tests/%.scm=%.log)

# Sanity checks for the script
installcheck-local:
	[ `$(bindir)/pinentry-rofi --version` = $(HVERSION) ]
	$(bindir)/pinentry-rofi --help
	echo BYE | $(bindir)/pinentry-rofi
