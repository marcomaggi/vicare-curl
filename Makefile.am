## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I meta/autotools
AUTOMAKE_OPTIONS	= foreign
EXTRA_DIST		= INSTALL configure.sh

slsdir			= $(srcdir)/lib
fasldir			= $(builddir)/lib
testdir			= $(srcdir)/tests
bundledlibsdir		= $(libdir)/vicare-scheme
scriptsdir		= $(srcdir)/meta/scripts

CLEANFILES		=
MAINTAINERCLEANFILES	=
BUILT_SOURCES		=

#page
#### documentation

dist_doc_DATA			= README COPYING
AM_MAKEINFOFLAGS		= --no-split
info_TEXINFOS			= doc/vicare-curl.texi
doc_vicare_curl_TEXINFOS	= doc/fdl-1.3.texi doc/gpl-3.0.texi doc/macros.texi

#page
#### C language library

AM_CFLAGS		= -I $(srcdir)/src -Wall

vicare_curl_CURRENT	= @vicare_curl_VERSION_INTERFACE_CURRENT@
vicare_curl_REVISION= @vicare_curl_VERSION_INTERFACE_REVISION@
vicare_curl_AGE	= @vicare_curl_VERSION_INTERFACE_AGE@
vicare_curl_VERSION	= $(vicare_curl_CURRENT):$(vicare_curl_REVISION):$(vicare_curl_AGE)

lib_LTLIBRARIES			= libvicare-curl.la
libvicare_curl_la_LDFLAGS	= -version-info $(vicare_curl_VERSION)
libvicare_curl_la_SOURCES	= \
	src/vicare-curl-version.c	\
	src/vicare-curl-multi.c		\
	src/vicare-curl-misc.c		\
	src/vicare-curl-easy.c		\
	src/vicare-curl-internals.h

.PHONY: vicare-c-library

vicare-c-library: $(lib_LTLIBRARIES)

#page
#### libraries

VICARE_COMPILE_SOURCE_PATH	= $(fasldir):$(slsdir)
VICARE_COMPILE_LIBRARY_PATH	= $(fasldir):$(VICARE_LIBRARY_PATH)
VICARE_COMPILE_LD_PATH		= $(builddir)/.libs:$(LD_LIBRARY_PATH)
VICARE_COMPILE_DYLD_PATH	= $(builddir)/.libs:$(DYLD_LIBRARY_PATH)

VICARE_COMPILE_ENV	= \
	VICARE_SOURCE_PATH=$(VICARE_COMPILE_SOURCE_PATH);	export VICARE_SOURCE_PATH;	\
	VICARE_LIBRARY_PATH=$(VICARE_COMPILE_LIBRARY_PATH);	export VICARE_LIBRARY_PATH;	\
	LD_LIBRARY_PATH=$(VICARE_COMPILE_LD_PATH);		export LD_LIBRARY_PATH;		\
	DYLD_LIBRARY_PATH=$(VICARE_COMPILE_DYLD_PATH);		export DYLD_LIBRARY_PATH;
VICARE_COMPILE_FLAGS	= $(AX_VFLAGS) $(VFLAGS) \
			  --library-locator compile-time --build-directory $(fasldir)
VICARE_COMPILE_RUN	= $(VICARE_COMPILE_ENV) $(VICARE) $(VICARE_COMPILE_FLAGS)

FASL_PREREQUISTES	= $(lib_LTLIBRARIES) $(libvicare_curl_la_LINK)

## --------------------------------------------------------------------

BUILT_SOURCES		+= lib/vicare/net/curl/features.vicare.sls
CLEANFILES		+= lib/vicare/net/curl/features.vicare.sls
noinst_PROGRAMS		= features
features_SOURCES	= lib/features.c

lib/vicare/net/curl/features.vicare.sls : features
	$(MKDIR_P) lib/vicare/net/curl && ./features >$@

## --------------------------------------------------------------------

EXTRA_DIST += \
	lib/libraries.scm			\
	$(scriptsdir)/build-makefile-rules.sps

VICARE_BUILD_DEPS	= \
	$(VICARE_COMPILE_RUN) \
		--source-path $(testdir) --option print-loaded-libraries --library-locator source \
		--r6rs-script $(scriptsdir)/build-makefile-rules.sps --

.PHONY: dependencies

dependencies: lib/vicare/net/curl/features.vicare.sls
	$(VICARE_BUILD_DEPS) $(slsdir)/libraries.scm >$(slsdir)/dependencies.make

# NOTE This  INCLUDE directive is  processed by Automake.   To specify
# the  pathname  of the  include  file:  we  can use  only  predefined
# makefile variables like "top_srcdir"; we cannot use custom variables
# defined by this very makefile.
include $(top_srcdir)/lib/dependencies.make

## --------------------------------------------------------------------

.PHONY: clean-fasl

clean-fasl:
	find $(fasldir)        -name \*.fasl -execdir rm -v '{}' ';'
	find $(builddir)/tests -name \*.fasl -execdir rm -v '{}' ';'

#page
#### running the test suite: test files selection

VICARE_CURL_PLAIN_TESTS		= \
	tests/test-vicare-curl-version.sps	\
	tests/test-vicare-curl-init.sps		\
	tests/test-vicare-curl-core.sps		\
	tests/test-vicare-curl-callbacks.sps	\
	tests/test-vicare-curl-easy.sps		\
	tests/test-vicare-curl-multi.sps

# VICARE_CURL_NAU_TESTS		= \
# 	tests/test-nausicaa-curl.sps

TESTS	= $(VICARE_CURL_PLAIN_TESTS)
# if WANT_NAUSICAA
# TESTS	+= $(VICARE_CURL_NAU_TESTS)
# endif

EXTRA_DIST += \
	$(VICARE_CURL_PLAIN_TESTS)		\
	\
	tests/demo-for-plain-documentation.sps	\
	tests/demo-for-plain-easy.sps		\
	tests/demo-for-plain-multi.sps

VICARE_SELECTED_TESTS		= $(testdir)/test-*$(file)*.sps
VICARE_SELECTED_DEMOS		= $(testdir)/demo-*$(file)*.sps

#page
#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

VICARE_CHECK_LIBRARY_PATH	= $(fasldir):$(VICARE_LIBRARY_PATH)
VICARE_CHECK_SOURCE_PATH	= $(testdir):$(slsdir):$(fasldir)
VICARE_CHECK_LD_PATH		= $(builddir)/.libs:$(LD_LIBRARY_PATH)
VICARE_CHECK_DYLD_PATH		= $(builddir)/.libs:$(DYLD_LIBRARY_PATH)

VICARE_CHECK_FLAGS		= $(AX_VFLAGS) $(VFLAGS) --r6rs-script
VICARE_CHECK_ENV		= \
	VICARE_CHECK_QUIET=yes;					export VICARE_CHECK_QUIET;	\
	VICARE_SOURCE_PATH=$(VICARE_CHECK_SOURCE_PATH);		export VICARE_SOURCE_PATH;	\
	VICARE_LIBRARY_PATH=$(VICARE_CHECK_LIBRARY_PATH);	export VICARE_LIBRARY_PATH;	\
	CHECK_TEST_NAME=$(name);				export CHECK_TEST_NAME;		\
	VICARE_SRC_DIR=$(srcdir)/tests;				export VICARE_SRC_DIR;		\
	VICARE_BUILDDIR=$(builddir);				export VICARE_BUILDDIR;		\
	LD_LIBRARY_PATH=$(VICARE_CHECK_LD_PATH);		export LD_LIBRARY_PATH;		\
	DYLD_LIBRARY_PATH=$(VICARE_CHECK_DYLD_PATH);		export DYLD_LIBRARY_PATH;

# We must make sure that this ends with a semicolon!!!
AM_TESTS_ENVIRONMENT	= $(VICARE_CHECK_ENV)
TEST_EXTENSIONS		= .sps
SPS_LOG_COMPILER	= $(VICARE)
AM_SPS_LOG_FLAGS	= $(VICARE_CHECK_FLAGS)

#page
#### interface to "make instcheck"

VICARE_INSTCHECK_LIBRARY_PATH	= $(DESTDIR)$(bundledlibsdir):$(VICARE_LIBRARY_PATH)
VICARE_INSTCHECK_SOURCE_PATH	= $(DESTDIR)$(bundledlibsdir):$(VICARE_SOURCE_PATH):$(testdir)
VICARE_INSTCHECK_LD_PATH	= $(DESTDIR)$(libdir)
VICARE_INSTCHECK_DYLD_PATH	= $(DESTDIR)$(libdir)

VICARE_INSTCHECK_FLAGS	= --option print-loaded-libraries $(AX_VFLAGS) $(VFLAGS)
VICARE_INSTCHECK_ENV	= \
	VICARE_LIBRARY_PATH=$(VICARE_INSTCHECK_LIBRARY_PATH);	export VICARE_LIBRARY_PATH;	\
	VICARE_SOURCE_PATH=$(VICARE_INSTCHECK_SOURCE_PATH);	export VICARE_SOURCE_PATH;	\
	LD_LIBRARY_PATH=$(VICARE_INSTCHECK_LD_PATH);		export LD_LIBRARY_PATH;		\
	DYLD_LIBRARY_PATH=$(VICARE_INSTCHECK_DYLD_PATH);	export DYLD_LIBRARY_PATH;	\
	VICARE_SRC_DIR=$(srcdir)/tests;				export VICARE_SRC_DIR;		\
	VICARE_CHECK_QUIET=no;					export VICARE_CHECK_QUIET;
VICARE_INSTCHECK_RUN	= \
	$(VICARE_INSTCHECK_ENV) $(VICARE) $(VICARE_INSTCHECK_FLAGS)

installcheck-local:
	@for f in $(TESTS); do $(VICARE_INSTCHECK_RUN) $(srcdir)/$$f; done

#page
#### running the test suite: customised test rules

VICARE_TEST_SELECTED_TESTS		= $(testdir)/test-*$(file)*.sps
VICARE_TEST_SELECTED_DEMOS		= $(testdir)/demo-*$(file)*.sps

VICARE_TEST_LIBRARY_PATH		= $(fasldir):$(VICARE_LIBRARY_PATH)
VICARE_TEST_SOURCE_PATH			= $(testdir):$(slsdir):$(fasldir)
VICARE_TEST_LD_PATH			= $(builddir)/.libs:$(LD_LIBRARY_PATH)
VICARE_TEST_DYLD_PATH			= $(builddir)/.libs:$(DYLD_LIBRARY_PATH)

# Enable timing  of test files  execution.  We cannot include  this in
# the separator because it needs  to be placed between the environment
# variables and the actual command line.
if WANT_TIME_TESTS
VICARE_TIME_TESTS	= $(TIME_PROGRAM) -p
endif

VICARE_TEST_FLAGS	= $(AX_VFLAGS) $(VFLAGS) --r6rs-script
VICARE_TEST_ENV		= \
	VICARE_SOURCE_PATH=$(VICARE_TEST_SOURCE_PATH);		export VICARE_SOURCE_PATH;	\
	VICARE_LIBRARY_PATH=$(VICARE_TEST_LIBRARY_PATH);	export VICARE_LIBRARY_PATH;	\
	CHECK_TEST_NAME=$(name);				export CHECK_TEST_NAME;		\
	VICARE_SRC_DIR=$(srcdir)/tests;				export VICARE_SRC_DIR;		\
	VICARE_BUILDDIR=$(builddir);				export VICARE_BUILDDIR;		\
	LD_LIBRARY_PATH=$(VICARE_TEST_LD_PATH);			export LD_LIBRARY_PATH;		\
	DYLD_LIBRARY_PATH=$(VICARE_TEST_DYLD_PATH);		export DYLD_LIBRARY_PATH;
VICARE_TEST_RUN		= $(VICARE_TEST_ENV) $(VICARE_TIME_TESTS) $(GDB) $(VICARE) $(VICARE_TEST_FLAGS)

## --------------------------------------------------------------------

.PHONY: test tests
.PHONY: demo demos

test tests:
	@echo
	@for f in $(VICARE_TEST_SELECTED_TESTS); do $(VICARE_TEST_RUN) $$f; done
	@echo

demo demos:
	@echo
	@for f in $(VICARE_TEST_SELECTED_DEMOS); do $(VICARE_TEST_RUN) $$f; done
	@echo

#page
#### running vicare on the terminal from the build directory

RLWRAP_TRAMPOLINE	= $(scriptsdir)/rlwrap-trampoline
EXTRA_DIST		+= $(RLWRAP_TRAMPOLINE)

VICARE_TERMINAL_LIBRARY_PATH	= $(fasldir):$(VICARE_LIBRARY_PATH)
VICARE_TERMINAL_SOURCE_PATH	= $(slsdir)
VICARE_TERMINAL_LD_PATH		= $(builddir)/.libs:$(LD_LIBRARY_PATH)
VICARE_TERMINAL_DYLD_PATH	= $(builddir)/.libs:$(DYLD_LIBRARY_PATH)

VICARE_TERMINAL_RUN_FLAGS	= $(AX_VFLAGS) $(VFLAGS)
VICARE_TERMINAL_RUN_ENV		= \
	VICARE_LIBRARY_PATH=$(VICARE_TERMINAL_LIBRARY_PATH);	export VICARE_LIBRARY_PATH;	\
	VICARE_SOURCE_PATH=$(VICARE_TERMINAL_SOURCE_PATH);	export VICARE_SOURCE_PATH;	\
	VICARE_SRC_DIR=$(srcdir);				export VICARE_SRC_DIR;		\
	VICARE_BUILDDIR=$(builddir);				export VICARE_BUILDDIR;		\
	LD_LIBRARY_PATH=$(VICARE_TERMINAL_LD_PATH);		export LD_LIBRARY_PATH;		\
	DYLD_LIBRARY_PATH=$(VICARE_TERMINAL_DYLD_PATH);		export DYLD_LIBRARY_PATH;
VICARE_TERMINAL_RUN	= $(VICARE_TERMINAL_RUN_ENV) $(VICARE) $(VICARE_TERMINAL_RUN_FLAGS)

VICARE_TERMINAL_VIE_FLAGS	=  --raw-repl $(VICARE_TERMINAL_RUN_FLAGS)
VICARE_TERMINAL_VIE_ENV	= $(VICARE_TERMINAL_RUN_ENV)
VICARE_TERMINAL_VIE	= $(VICARE_TERMINAL_VIE_ENV) $(RLWRAP_TRAMPOLINE) $(VICARE) $(VICARE_TERMINAL_VIE_FLAGS)

.PHONY: test-run test-vie

test-run:
	$(VICARE_TERMINAL_RUN)

test-vie:
	$(VICARE_TERMINAL_VIE)

### end of file
