/*
  Part of: Vicare/cURL
  Contents: internal header file for Vicare/cURL
  Date: Wed Sep  5, 2012

  Abstract



  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either version  3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See the  GNU
  General Public License for more details.

  You should  have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef VICARE_CURL_INTERNALS_H
#define VICARE_CURL_INTERNALS_H 1


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <vicare.h>
#include <curl/curl.h>
#include <curl/easy.h>
#include <curl/multi.h>


/** --------------------------------------------------------------------
 ** Type definitions.
 ** ----------------------------------------------------------------- */

typedef struct curl_httppost	ik_curl_http_post_t;
typedef struct curl_forms	ik_curl_forms_t;


/** --------------------------------------------------------------------
 ** Handling of Scheme objects.
 ** ----------------------------------------------------------------- */

#define ika_integer_from_curlcode(PCB,CODE) \
  ika_integer_from_int((PCB),(int)(CODE))

/* Accessors    for    the    fields    of    the    Scheme    structure
   "curl-version-info-data". */
#define IK_CURL_VERSION_INFO_DATA_AGE(OBJ)		IK_FIELD((OBJ),0)
#define IK_CURL_VERSION_INFO_DATA_VERSION(OBJ)		IK_FIELD((OBJ),1)
#define IK_CURL_VERSION_INFO_DATA_VERSION_NUM(OBJ)	IK_FIELD((OBJ),2)
#define IK_CURL_VERSION_INFO_DATA_HOST(OBJ)		IK_FIELD((OBJ),3)
#define IK_CURL_VERSION_INFO_DATA_FEATURES(OBJ)		IK_FIELD((OBJ),4)
#define IK_CURL_VERSION_INFO_DATA_SSL_VERSION(OBJ)	IK_FIELD((OBJ),5)
#define IK_CURL_VERSION_INFO_DATA_SSL_VERSION_NUM(OBJ)	IK_FIELD((OBJ),6)
#define IK_CURL_VERSION_INFO_DATA_LIBZ_VERSION(OBJ)	IK_FIELD((OBJ),7)
#define IK_CURL_VERSION_INFO_DATA_PROTOCOLS(OBJ)	IK_FIELD((OBJ),8)
#define IK_CURL_VERSION_INFO_DATA_ARES(OBJ)		IK_FIELD((OBJ),9)
#define IK_CURL_VERSION_INFO_DATA_ARES_NUM(OBJ)		IK_FIELD((OBJ),10)
#define IK_CURL_VERSION_INFO_DATA_LIBIDN(OBJ)		IK_FIELD((OBJ),11)
#define IK_CURL_VERSION_INFO_DATA_ICONV_VER_NUM(OBJ)	IK_FIELD((OBJ),12)
#define IK_CURL_VERSION_INFO_DATA_LIBSSH_VERSION(OBJ)	IK_FIELD((OBJ),13)

/* Accessors    for    the    fields    of    the    Scheme    structure
   "curl-easy-handle". */
#define IK_CURL_EASY_POINTER(EASY)	IK_FIELD((EASY),0)
#define IK_CURL_EASY(EASY)		\
  IK_POINTER_DATA_VOIDP(IK_CURL_EASY_POINTER(EASY))


/** --------------------------------------------------------------------
 ** Support for missing functions.
 ** ----------------------------------------------------------------- */

static IK_UNUSED void
feature_failure_ (const char * funcname)
{
  ik_abort("called unavailable cURL specific function, %s\n", funcname);
}

#define feature_failure(FN)     { feature_failure_(FN); return IK_VOID; }


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#endif /* VICARE_CURL_INTERNALS_H */

/* end of file */
