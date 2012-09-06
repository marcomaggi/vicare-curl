/*
  Part of: Vicare/cURL
  Contents: Libcurl for Vicare, miscellaneous functions
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
 ** Initialisation functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_global_init (ikptr s_flags, ikpcb * pcb)
{
#ifdef HAVE_CURL_GLOBAL_INIT
  long		flags = ik_integer_to_long(s_flags);
  CURLcode	rv;
  rv = curl_global_init(flags);
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_global_init_mem (ikptr s_flags,
			   ikptr s_malloc_callback,
			   ikptr s_free_callback,
			   ikptr s_realloc_callback,
			   ikptr s_strdup_callback,
			   ikptr s_calloc_callback,
			   ikpcb * pcb)
{
#ifdef HAVE_CURL_GLOBAL_INIT_MEM
  long		flags = ik_integer_to_long(s_flags);
  curl_malloc_callback	malloc_callback  = IK_POINTER_FROM_POINTER_OR_FALSE(s_malloc_callback);
  curl_free_callback	free_callback    = IK_POINTER_FROM_POINTER_OR_FALSE(s_free_callback);
  curl_realloc_callback realloc_callback = IK_POINTER_FROM_POINTER_OR_FALSE(s_realloc_callback);
  curl_strdup_callback	strdup_callback  = IK_POINTER_FROM_POINTER_OR_FALSE(s_strdup_callback);
  curl_calloc_callback	calloc_callback  = IK_POINTER_FROM_POINTER_OR_FALSE(s_calloc_callback);
  CURLcode	rv;
  rv = curl_global_init_mem(flags,
			    malloc_callback, free_callback, realloc_callback,
			    strdup_callback, calloc_callback);
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_global_cleanup (ikpcb * pcb)
{
#ifdef HAVE_CURL_GLOBAL_CLEANUP
  curl_global_cleanup();
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Miscellaneous functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_free (ikptr s_pointer, ikpcb * pcb)
{
#ifdef HAVE_CURL_FREE
  void *	ptr = IK_POINTER_FROM_POINTER_OR_FALSE(s_pointer);
  if (ptr) {
    curl_free(ptr);
    IK_POINTER_SET_NULL(s_pointer);
  }
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}

/* end of file */
