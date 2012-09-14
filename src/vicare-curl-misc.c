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

#include <vicare-curl-internals.h>

typedef struct curl_slist	ik_curl_slist_t;


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
 ** String lists.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_slist_append (ikptr s_slist, ikptr s_string, ikpcb * pcb)
{
#ifdef HAVE_CURL_SLIST_APPEND
  ik_curl_slist_t *	slist  = IK_VOIDP_FROM_POINTER_OR_FALSE(s_slist);
  const char *		string = IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_string);
  ik_curl_slist_t *	rv;
  /* fprintf(stderr, "%s: enter %s\n", __func__, string); */
  rv = curl_slist_append(slist, string);
  return (rv)? ika_pointer_alloc(pcb, (ik_ulong)rv) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_slist_free_all (ikptr s_slist, ikpcb * pcb)
{
#ifdef HAVE_CURL_SLIST_FREE_ALL
  ik_curl_slist_t *	slist  = IK_VOIDP_FROM_POINTER_OR_FALSE(s_slist);
  if (slist) {
    curl_slist_free_all(slist);
    IK_POINTER_SET_NULL(s_slist);
  }
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_slist_to_bytevectors (ikptr s_slist, ikpcb * pcb)
{
  ik_curl_slist_t *	slist  = IK_VOIDP_FROM_POINTER_OR_FALSE(s_slist);
  if (slist) {
    ikptr		s_strings = IK_NULL;
    ikptr		s_pair    = IK_NULL;
    ik_curl_slist_t *	next;
    pcb->root0 = &s_strings;
    pcb->root1 = &s_pair;
    {
      for (next=slist; next; next = next->next) {
	s_pair = ika_pair_alloc(pcb);
	if (next->data) {
	  IK_ASS(IK_CAR(s_pair), ika_bytevector_from_cstring(pcb, next->data));
	} else {
	  IK_CAR(s_pair) = IK_FALSE;
	}
	IK_CDR(s_pair) = s_strings;
	s_strings      = s_pair;
      }
    }
    pcb->root1 = NULL;
    pcb->root0 = NULL;
    return s_strings;
  } else
    return IK_NULL;
}


/** --------------------------------------------------------------------
 ** Form data composition.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_formadd_1 (ikptr s_form_data, ikptr s_last_item,
		     ikptr s_option, ikptr s_value,
		     ikpcb * pcb)
{
#ifdef HAVE_CURL_FORMADD
  ikptr			s_form_data_ptr	= IK_CURL_FORM_DATA_POINTER(s_form_data);
  ik_curl_http_post_t *	first_item	= IK_POINTER_DATA_VOIDP(s_form_data_ptr);
  ik_curl_http_post_t *	last_item	= IK_POINTER_DATA_VOIDP(s_last_item);
  CURLFORMcode		rv;
  if (IK_IS_INTEGER(s_value))
    rv = curl_formadd(&first_item, &last_item,
		      ik_integer_to_int(s_option),
		      ik_integer_to_long(s_value),
		      CURLFORM_END);
  else
    rv = curl_formadd(&first_item, &last_item,
		      ik_integer_to_int(s_option),
		      IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value),
		      CURLFORM_END);
  IK_POINTER_SET(s_form_data_ptr, first_item);
  IK_POINTER_SET(s_last_item, last_item);
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_formadd_2 (ikptr s_form_data, ikptr s_last_item,
		     ikptr s_option_1, ikptr s_value_1,
		     ikptr s_option_2, ikptr s_value_2,
		     ikpcb * pcb)
{
#ifdef HAVE_CURL_FORMADD
  ikptr			s_form_data_ptr	= IK_CURL_FORM_DATA_POINTER(s_form_data);
  ik_curl_http_post_t *	first_item	= IK_POINTER_DATA_VOIDP(s_form_data_ptr);
  ik_curl_http_post_t *	last_item	= IK_POINTER_DATA_VOIDP(s_last_item);
  int			isint1		= IK_IS_INTEGER(s_value_1);
  int			isint2		= IK_IS_INTEGER(s_value_2);
  CURLFORMcode		rv;
  if (isint1 && isint2)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       CURLFORM_END);
  else if (isint1)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       CURLFORM_END);
  else if (isint2)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       CURLFORM_END);
  else
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       CURLFORM_END);
  IK_POINTER_SET(s_form_data_ptr, first_item);
  IK_POINTER_SET(s_last_item, last_item);
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_formadd_3 (ikptr s_form_data, ikptr s_last_item,
		     ikptr s_option_1, ikptr s_value_1,
		     ikptr s_option_2, ikptr s_value_2,
		     ikptr s_option_3, ikptr s_value_3,
		     ikpcb * pcb)
{
#ifdef HAVE_CURL_FORMADD
  ikptr			s_form_data_ptr	= IK_CURL_FORM_DATA_POINTER(s_form_data);
  ik_curl_http_post_t *	first_item	= IK_POINTER_DATA_VOIDP(s_form_data_ptr);
  ik_curl_http_post_t *	last_item	= IK_POINTER_DATA_VOIDP(s_last_item);
  int			isint1		= IK_IS_INTEGER(s_value_1);
  int			isint2		= IK_IS_INTEGER(s_value_2);
  int			isint3		= IK_IS_INTEGER(s_value_3);
  CURLFORMcode		rv;
  if (isint1 && isint2 && isint3)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       ik_integer_to_int(s_option_3), ik_integer_to_long(s_value_3),
       CURLFORM_END);
  else if (isint1 && isint2)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       ik_integer_to_int(s_option_3), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_3),
       CURLFORM_END);
  else if (isint2 && isint3)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       ik_integer_to_int(s_option_3), ik_integer_to_long(s_value_3),
       CURLFORM_END);
  else if (isint3 && isint1)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       ik_integer_to_int(s_option_3), ik_integer_to_long(s_value_3),
       CURLFORM_END);
  else if (isint1)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       ik_integer_to_int(s_option_3), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_3),
       CURLFORM_END);
  else if (isint2)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       ik_integer_to_int(s_option_3), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_3),
       CURLFORM_END);
  else if (isint3)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       ik_integer_to_int(s_option_3), ik_integer_to_long(s_value_3),
       CURLFORM_END);
  else
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       ik_integer_to_int(s_option_3), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_3),
       CURLFORM_END);
  IK_POINTER_SET(s_form_data_ptr, first_item);
  IK_POINTER_SET(s_last_item, last_item);
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_formadd_4 (ikptr s_form_data, ikptr s_last_item,
		     ikptr s_option_1, ikptr s_value_1,
		     ikptr s_option_2, ikptr s_value_2,
		     ikptr s_option_3, ikptr s_value_3,
		     ikptr s_option_4, ikptr s_value_4,
		     ikpcb * pcb)
{
#ifdef HAVE_CURL_FORMADD
  ikptr			s_form_data_ptr	= IK_CURL_FORM_DATA_POINTER(s_form_data);
  ik_curl_http_post_t *	first_item	= IK_POINTER_DATA_VOIDP(s_form_data_ptr);
  ik_curl_http_post_t *	last_item	= IK_POINTER_DATA_VOIDP(s_last_item);
  int			isint1		= IK_IS_INTEGER(s_value_1);
  int			isint2		= IK_IS_INTEGER(s_value_2);
  int			isint3		= IK_IS_INTEGER(s_value_3);
  int			isint4		= IK_IS_INTEGER(s_value_4);
  CURLFORMcode		rv;
  /* fprintf(stderr, "%s: %d, %d, %d, %d\n", __func__, isint1, isint2, isint3, isint4); */
  if (isint1 && isint2 && isint3 && isint4)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       ik_integer_to_int(s_option_3), ik_integer_to_long(s_value_3),
       ik_integer_to_int(s_option_4), ik_integer_to_long(s_value_4),
       CURLFORM_END);
  else if (isint1 && isint2 && isint3)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       ik_integer_to_int(s_option_3), ik_integer_to_long(s_value_3),
       ik_integer_to_int(s_option_4), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_4),
       CURLFORM_END);
  else if (isint2 && isint3 && isint4)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       ik_integer_to_int(s_option_3), ik_integer_to_long(s_value_3),
       ik_integer_to_int(s_option_4), ik_integer_to_long(s_value_4),
       CURLFORM_END);
  else if (isint3 && isint4 && isint1)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       ik_integer_to_int(s_option_3), ik_integer_to_long(s_value_3),
       ik_integer_to_int(s_option_4), ik_integer_to_long(s_value_4),
       CURLFORM_END);
  else if (isint4 && isint1 && isint2)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       ik_integer_to_int(s_option_3), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_3),
       ik_integer_to_int(s_option_4), ik_integer_to_long(s_value_4),
       CURLFORM_END);
  else if (isint1 && isint2)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       ik_integer_to_int(s_option_3), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_3),
       ik_integer_to_int(s_option_4), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_4),
       CURLFORM_END);
  else if (isint2 && isint3)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       ik_integer_to_int(s_option_3), ik_integer_to_long(s_value_3),
       ik_integer_to_int(s_option_4), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_4),
       CURLFORM_END);
  else if (isint3 && isint4)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       ik_integer_to_int(s_option_3), ik_integer_to_long(s_value_3),
       ik_integer_to_int(s_option_4), ik_integer_to_long(s_value_4),
       CURLFORM_END);
  else if (isint4 && isint1)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       ik_integer_to_int(s_option_3), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_3),
       ik_integer_to_int(s_option_4), ik_integer_to_long(s_value_4),
       CURLFORM_END);
  else if (isint1 && isint3)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       ik_integer_to_int(s_option_3), ik_integer_to_long(s_value_3),
       ik_integer_to_int(s_option_4), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_4),
       CURLFORM_END);
  else if (isint2 && isint4)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       ik_integer_to_int(s_option_3), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_3),
       ik_integer_to_int(s_option_4), ik_integer_to_long(s_value_4),
       CURLFORM_END);
  else if (isint1)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), ik_integer_to_long(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       ik_integer_to_int(s_option_3), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_3),
       ik_integer_to_int(s_option_4), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_4),
       CURLFORM_END);
  else if (isint2)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), ik_integer_to_long(s_value_2),
       ik_integer_to_int(s_option_3), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_3),
       ik_integer_to_int(s_option_4), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_4),
       CURLFORM_END);
  else if (isint3)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       ik_integer_to_int(s_option_3), ik_integer_to_long(s_value_3),
       ik_integer_to_int(s_option_4), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_4),
       CURLFORM_END);
  else if (isint4)
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       ik_integer_to_int(s_option_3), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_3),
       ik_integer_to_int(s_option_4), ik_integer_to_long(s_value_4),
       CURLFORM_END);
  else
    rv = curl_formadd
      (&first_item, &last_item,
       ik_integer_to_int(s_option_1), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_1),
       ik_integer_to_int(s_option_2), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_2),
       ik_integer_to_int(s_option_3), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_3),
       ik_integer_to_int(s_option_4), IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_value_4),
       CURLFORM_END);
  IK_POINTER_SET(s_form_data_ptr, first_item);
  IK_POINTER_SET(s_last_item, last_item);
  /* fprintf(stderr, "%s: exit %d\n", __func__, rv); */
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_curl_formget (ikptr s_form_data, ikptr s_data, ikptr s_callback, ikpcb * pcb)
{
#ifdef HAVE_CURL_FORMGET
  ik_curl_http_post_t *	post		= IK_CURL_FORM_DATA(s_form_data);
  void *		custom_data	= IK_VOIDP_FROM_POINTER_OR_MBLOCK_OR_FALSE(s_data);
  curl_formget_callback	callback	= IK_VOIDP_FROM_POINTER_OR_FALSE(s_callback);
  ikptr			sk;
  int			rv;
  sk = ik_enter_c_function(pcb);
  {
    rv = curl_formget(post, custom_data, callback);
  }
  ik_leave_c_function(pcb, sk);
  return IK_BOOLEAN_FROM_INT(rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_formfree (ikptr s_form_data, ikpcb * pcb)
{
#ifdef HAVE_CURL_FORMFREE
  ikptr			s_pointer	= IK_CURL_FORM_DATA_POINTER(s_form_data);
  ik_curl_http_post_t *	post		= IK_POINTER_DATA_VOIDP(s_pointer);
  /* fprintf(stderr, "%s: enter %p\n", __func__, (void*)post); */
  if (post) {
    curl_formfree(post);
    IK_POINTER_SET_NULL(s_pointer);
  }
  /* fprintf(stderr, "%s: leave %p\n", __func__, IK_CURL_FORM_DATA(s_form_data)); */
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Escaping URL strings.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_escape (ikptr s_chars, ikptr s_length, ikpcb * pcb)
{
#ifdef HAVE_CURL_ESCAPE
  const char *	chars	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_chars);
  int		length;
  char *	rv;
  if (IK_IS_BYTEVECTOR(s_chars))
    length = IK_BYTEVECTOR_LENGTH(s_chars);
  else if (IK_IS_POINTER(s_chars))
    length = ik_integer_to_int(s_length);
  else
    length = IK_MBLOCK_SIZE_T(s_chars);
  rv = curl_escape(chars, length);
  if (rv) {
    ikptr bv = ika_bytevector_from_cstring(pcb, rv);
    curl_free(rv);
    return bv;
  } else
    return IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_unescape (ikptr s_chars, ikptr s_length, ikpcb * pcb)
{
#ifdef HAVE_CURL_UNESCAPE
  const char *	chars	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_chars);
  int		length;
  char *	rv;
  if (IK_IS_BYTEVECTOR(s_chars))
    length = IK_BYTEVECTOR_LENGTH(s_chars);
  else if (IK_IS_POINTER(s_chars))
    length = ik_integer_to_int(s_length);
  else
    length = IK_MBLOCK_SIZE_T(s_chars);
  rv = curl_unescape(chars, length);
  if (rv) {
    ikptr bv = ika_bytevector_from_cstring(pcb, rv);
    curl_free(rv);
    return bv;
  } else
    return IK_FALSE;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Shared option sets.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_share_init (ikpcb * pcb)
{
#ifdef HAVE_CURL_SHARE_INIT
  CURLSH	* rv;
  rv = curl_share_init();
  return (rv)? ika_pointer_alloc(pcb, (ik_ulong)rv) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_share_setopt (ikptr s_share, ikptr s_option, ikptr s_parameter, ikpcb * pcb)
{
#ifdef HAVE_CURL_SHARE_SETOPT
  CURLSH *	share	= IK_CURL_SHARE(s_share);
  CURLSHoption	option	= ik_integer_to_int(s_option);
  CURLSHcode	rv;
  switch (option) {
  case CURLSHOPT_SHARE:
  case CURLSHOPT_UNSHARE:
    rv = curl_share_setopt(share, option, ik_integer_to_int(s_parameter));
    break;
  default:	/* everything else is a pointer to callback function */
    rv = curl_share_setopt(share, option, IK_VOIDP_FROM_POINTER_OR_FALSE(s_parameter));
    break;
  }
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_share_cleanup (ikptr s_share, ikpcb * pcb)
{
#ifdef HAVE_CURL_SHARE_CLEANUP
  ikptr		s_pointer	= IK_CURL_SHARE_POINTER(s_share);
  CURLSH *	share		= IK_POINTER_DATA_VOIDP(s_pointer);
  CURLSHcode	rv;
  /* fprintf(stderr, "%s: enter %p\n", __func__, (void*)share); */
  if (share) {
    ikptr	sk;
    sk = ik_enter_c_function(pcb);
    {
      rv = curl_share_cleanup(share);
    }
    ik_leave_c_function(pcb, sk);
    if (CURLSHE_OK == rv) {
      IK_POINTER_SET_NULL(s_pointer);
    }
  } else
    rv = CURLSHE_OK;
  /* fprintf(stderr, "%s: leave %d %p\n", __func__, rv, IK_CURL_SHARE(s_share)); */
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_share_strerror (ikptr s_errcode, ikpcb * pcb)
{
#ifdef HAVE_CURL_SHARE_STRERROR
  CURLSHcode	errcode = ik_integer_to_int(s_errcode);
  const char *	rv;
  rv = curl_share_strerror(errcode);
  return ika_bytevector_from_cstring(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Accessors for "struct curl_sockaddr".
 ** ----------------------------------------------------------------- */

typedef struct curl_sockaddr	ik_curl_sockaddr_t;

ikptr
ikrt_curl_sockaddr_family (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_sockaddr_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_integer_from_int(pcb, S->family);
}
ikptr
ikrt_curl_sockaddr_socktype (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_sockaddr_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_integer_from_int(pcb, S->socktype);
}
ikptr
ikrt_curl_sockaddr_protocol (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_sockaddr_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_integer_from_int(pcb, S->protocol);
}
ikptr
ikrt_curl_sockaddr_addrlen (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_sockaddr_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_integer_from_uint(pcb, S->addrlen);
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_curl_sockaddr_addr (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_sockaddr_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_bytevector_from_memory_block(pcb, &(S->addr), S->addrlen);
}


/** --------------------------------------------------------------------
 ** Accessors for "struct curl_fileinfo".
 ** ----------------------------------------------------------------- */

typedef struct curl_fileinfo	ik_curl_fileinfo_t;

ikptr
ikrt_curl_fileinfo_filename (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_pointer_alloc(pcb, (ik_ulong)(S->filename));
}
ikptr
ikrt_curl_fileinfo_filetype (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_integer_from_int(pcb, S->filetype);
}
ikptr
ikrt_curl_fileinfo_time (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_integer_from_sint64(pcb, (int64_t)(S->time));
}
ikptr
ikrt_curl_fileinfo_perm (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_integer_from_uint(pcb, S->perm);
}
ikptr
ikrt_curl_fileinfo_uid (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_integer_from_int(pcb, S->uid);
}
ikptr
ikrt_curl_fileinfo_gid (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_integer_from_uint(pcb, S->gid);
}
ikptr
ikrt_curl_fileinfo_size (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_integer_from_off_t(pcb, S->size);
}
ikptr
ikrt_curl_fileinfo_hardlinks (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_integer_from_ulong(pcb, S->hardlinks);
}
ikptr
ikrt_curl_fileinfo_strings_time (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_pointer_alloc(pcb, (ik_ulong)S->strings.time);
}
ikptr
ikrt_curl_fileinfo_strings_perm (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_pointer_alloc(pcb, (ik_ulong)S->strings.perm);
}
ikptr
ikrt_curl_fileinfo_strings_user (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_pointer_alloc(pcb, (ik_ulong)S->strings.user);
}
ikptr
ikrt_curl_fileinfo_strings_group (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_pointer_alloc(pcb, (ik_ulong)S->strings.group);
}
ikptr
ikrt_curl_fileinfo_strings_target (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_pointer_alloc(pcb, (ik_ulong)S->strings.target);
}
ikptr
ikrt_curl_fileinfo_flags (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_fileinfo_t *	S = IK_POINTER_DATA_VOIDP(s_struct);
  return ika_integer_from_uint(pcb, S->flags);
}


/** --------------------------------------------------------------------
 ** Accessors for "struct curl_khkey".
 ** ----------------------------------------------------------------- */

typedef struct curl_khkey	ik_curl_khkey_t;

ikptr
ikrt_curl_khkey_key (ikptr s_struct, ikpcb * pcb)
{
  ik_curl_khkey_t *	S  = IK_POINTER_DATA_VOIDP(s_struct);
  ikptr			rv = ika_pair_alloc(pcb);
  pcb->root0 = &rv;
  {
    if (S->len) {
      /* The bytevector holds raw data. */
      IK_CAR(rv) = IK_FALSE;
      IK_ASS(IK_CDR(rv), ika_bytevector_from_memory_block(pcb, S->key, S->len));
    } else {
      /* The bytevector holds data encoded in base64. */
      IK_CAR(rv) = IK_TRUE;
      IK_ASS(IK_CDR(rv), ika_bytevector_from_cstring(pcb, S->key));
    }
  }
  pcb->root0 = NULL;
  return rv;
}


/** --------------------------------------------------------------------
 ** Accessors for "struct curl_forms".
 ** ----------------------------------------------------------------- */

typedef struct curl_forms	ik_curl_forms_t;

ikptr
ikrt_curl_forms_sizeof (ikptr s_number_of_items, ikpcb * pcb)
{
  int	nmemb = IK_UNFIX(s_number_of_items);
  return ika_integer_from_int(pcb, nmemb*sizeof(ik_curl_forms_t));
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_curl_forms_option (ikptr s_array_of_structs, ikptr s_index, ikpcb * pcb)
{
  ik_curl_forms_t *	S   = IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_array_of_structs);
  int			idx = IK_UNFIX(s_index);
  return ika_integer_from_int(pcb, S[idx].option);
}
ikptr
ikrt_curl_forms_option_set (ikptr s_array_of_structs, ikptr s_index, ikptr s_value, ikpcb * pcb)
{
  ik_curl_forms_t *	S   = IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_array_of_structs);
  int			idx = IK_UNFIX(s_index);
  CURLformoption	val = ik_integer_to_int(s_value);
  S[idx].option = val;
  return IK_VOID;
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_curl_forms_value (ikptr s_array_of_structs, ikptr s_index, ikpcb * pcb)
{
  ik_curl_forms_t *	S   = IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_array_of_structs);
  int			idx = IK_UNFIX(s_index);
  const char *		val = S[idx].value;
  return (val)? ika_bytevector_from_cstring(pcb, val) : IK_FALSE;
}
ikptr
ikrt_curl_forms_value_set (ikptr s_array_of_structs, ikptr s_index, ikptr s_value, ikpcb * pcb)
{
  ik_curl_forms_t *	S   = IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_array_of_structs);
  int			idx = IK_UNFIX(s_index);
  char *		val = IK_CHARP_FROM_POINTER_OR_MBLOCK(s_value);
  S[idx].value = val;
  return IK_VOID;
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
ikptr
ikrt_curl_getdate (ikptr s_date, ikpcb * pcb)
{
#ifdef HAVE_CURL_GETDATE
  const char *	date = IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_date);
  time_t	rv;
  rv = curl_getdate(date, NULL);
  return ika_integer_from_sint64(pcb, (int64_t)rv);
#else
  feature_failure(__func__);
#endif
}

/* end of file */
