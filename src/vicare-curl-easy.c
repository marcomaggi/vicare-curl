/*
  Part of: Vicare/cURL
  Contents: Libcurl for Vicare, easy API
  Date: Wed Sep  5, 2012

  Abstract



  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either  version 3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <vicare.h>
#include <vicare-curl-internals.h>


/** --------------------------------------------------------------------
 ** Escaping URL strings.
 ** ----------------------------------------------------------------- */

#if 0
ikptr
ikrt_curl_easy_escape (ikptr s_easy, ikptr s_chars, ikptr s_length, ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_ESCAPE
  CURL *	easy	= IK_CURL_EASY(s_easy);
  const char *	chars	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_chars);
  int		length;
  char *	rv;
  if (IK_IS_BYTEVECTOR(s_chars))
    length = IK_BYTEVECTOR_LENGTH(s_chars);
  else if (IK_IS_POINTER(s_chars))
    length = ik_integer_to_int(s_length);
  else
    length = IK_MBLOCK_SIZE_T(s_chars);
  rv = curl_easy_escape(easy, chars, length);
  return (rv)? ika_pointer_alloc(pcb, (ik_ulong)rv) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_unescape (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_UNESCAPE
  curl_easy_unescape();
#else
  feature_failure(__func__);
#endif
}
#endif

/* end of file */
