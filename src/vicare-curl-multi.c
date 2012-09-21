/*
  Part of: Vicare/cURL
  Contents: Libcurl for Vicare, multi API
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

#include <vicare-curl-internals.h>


/** --------------------------------------------------------------------
 ** Multi API: initialisation and finalisation.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_multi_init (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_INIT
  CURLM *	 multi;
  multi = curl_multi_init();
  return (multi)? ika_pointer_alloc(pcb, (ik_ulong)multi) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_cleanup (ikptr s_multi, ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_CLEANUP
  ikptr		s_pointer	= IK_CURL_MULTI_POINTER(s_multi);
  CURLM *	multi		= IK_POINTER_DATA_VOIDP(s_pointer);
  CURLMcode	rv;
  if (multi) {
    rv = curl_multi_cleanup(multi);
    IK_POINTER_SET_NULL(s_pointer);
    return ika_integer_from_curlcode(pcb, rv);
  } else
    return ika_integer_from_curlcode(pcb, CURLM_OK);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Still to be interfaced.
 ** ----------------------------------------------------------------- */

#if 0
ikptr
ikrt_curl_multi_add_handle (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_ADD_HANDLE
  curl_multi_add_handle();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_remove_handle (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_REMOVE_HANDLE
  curl_multi_remove_handle();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_fdset (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_FDSET
  curl_multi_fdset();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_perform (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_PERFORM
  curl_multi_perform();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_info_read (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_INFO_READ
  curl_multi_info_read();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_strerror (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_STRERROR
  curl_multi_strerror();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_socket (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_SOCKET
  curl_multi_socket();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_socket_action (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_SOCKET_ACTION
  curl_multi_socket_action();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_socket_all (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_SOCKET_ALL
  curl_multi_socket_all();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_timeout (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_TIMEOUT
  curl_multi_timeout();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_setopt (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_SETOPT
  curl_multi_setopt();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_assign (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_ASSIGN
  curl_multi_assign();
#else
  feature_failure(__func__);
#endif
}
#endif

/* end of file */
