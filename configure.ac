dnl @configure_input@
dnl

AC_PREREQ([2.68])
AC_INIT([Vicare/cURL],[0.1d1],[marco.maggi-ipsu@poste.it],
  [vicare-curl],[http://marcomaggi.github.com/vicare.html])
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([-Wall])
dnl AM_MAINTAINER_MODE
AC_CONFIG_SRCDIR([src/])
AC_CONFIG_MACRO_DIR([m4])
AC_CANONICAL_HOST

VICARE_DEBUG=no
AC_MSG_CHECKING([whether debugging features are included in compiled Scheme libraries])
AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],
     [enable compilation of Scheme libraries with debugging features (default is disable)])],
  [AC_MSG_RESULT([$enableval])
  if test "z$enableval" = zno
  then VICARE_DEBUG=no
  else VICARE_DEBUG=yes
  fi],
  [VICARE_DEBUG=no
   AC_MSG_RESULT([no])])
AM_CONDITIONAL([WANT_VICARE_DEBUG],[test x$VICARE_DEBUG = xyes])

#page
#### compiler

AC_PROG_CC
AC_PROG_CC_C_O

AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

LT_PREREQ([2.4])
LT_INIT([shared disable-static])

dnl AC_LANG([C])
dnl AC_DEFINE([_GNU_SOURCE],[1],[enable everything GNU])

#page
#### vicare configuration

AC_CHECK_HEADERS([vicare.h],,[AC_MSG_FAILURE([missing vicare.h header],[2])])
AC_CHECK_PROG([VICARE],[vicare],[vicare],[:])

#page
#### cURL configuration

AC_CHECK_HEADERS([curl/curl.h],,
  [AC_MSG_FAILURE([missing curl/curl.h header],[2])])
AC_CHECK_LIB([curl],[curl_global_init],,
  [AC_MSG_FAILURE([missing cURL library],[2])])

dnl AC_CHECK_HEADER([ioctl.h])
dnl AC_CHECK_HEADER([time.h])

vicare_curl_VERSION_INTERFACE_CURRENT=0
vicare_curl_VERSION_INTERFACE_REVISION=0
vicare_curl_VERSION_INTERFACE_AGE=0
AC_DEFINE_UNQUOTED([vicare_curl_VERSION_INTERFACE_CURRENT],
  [$vicare_curl_VERSION_INTERFACE_CURRENT],
  [current interface number])
AC_DEFINE_UNQUOTED([vicare_curl_VERSION_INTERFACE_REVISION],
  [$vicare_curl_VERSION_INTERFACE_REVISION],
  [current interface implementation number])
AC_DEFINE_UNQUOTED([vicare_curl_VERSION_INTERFACE_AGE],
  [$vicare_curl_VERSION_INTERFACE_AGE],
  [current interface age number])
AC_DEFINE_UNQUOTED([vicare_curl_VERSION_INTERFACE_STRING],
  ["$vicare_curl_VERSION_INTERFACE_CURRENT.$vicare_curl_VERSION_INTERFACE_REVISION"],
  [library interface version])
AC_SUBST([vicare_curl_VERSION_INTERFACE_CURRENT])
AC_SUBST([vicare_curl_VERSION_INTERFACE_REVISION])
AC_SUBST([vicare_curl_VERSION_INTERFACE_AGE])

#page
#### symbols

m4_define([VICARE_INCLUDES],[
AC_INCLUDES_DEFAULT
#ifdef HAVE_VICARE_H
#  include <vicare.h>
#endif
#ifdef HAVE_CURL_CURL_H
#  include <curl/curl.h>
#endif
#ifdef HAVE_TIME_H
#  include <time.h>
#endif
])

dnl Stuff
dnl VICARE_CONSTANT_TESTS([XML_TRUE XML_FALSE])

#page
#### finish

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile]
        [src/Makefile]
        [doc/Makefile]
        [tests/Makefile]
        [lib/Makefile]
        [lib/vicare/net/curl/constants.sls]
        [meta/slackware/doinst.sh])
AC_OUTPUT

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^#page"
# End:
