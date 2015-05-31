/*
  Part of: Vicare/cURL
  Contents: Libcurl for Vicare, easy API
  Date: Wed Sep  5, 2012

  Abstract



  Copyright (C) 2012, 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>

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
 ** Initialisation and finalisation.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_easy_init (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_INIT
  CURL *	easy;
  easy = curl_easy_init();
  return (easy)? ika_pointer_alloc(pcb, (ik_ulong)easy) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_cleanup (ikptr s_easy, ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_CLEANUP
  ikptr		s_pointer	= IK_CURL_EASY_POINTER(s_easy);
  CURL *	easy		= IK_POINTER_DATA_VOIDP(s_pointer);
  int		owner		= IK_BOOLEAN_TO_INT(IK_CURL_EASY_OWNER(s_easy));
  /* fprintf(stderr, "%s: enter easy=%p, owner=%d\n", __func__, (void*)easy, owner); */
  if (easy && owner) {
    ik_enter_c_function(pcb);
    {
      curl_easy_cleanup(easy);
    }
    ik_leave_c_function(pcb);
  }
  if (easy)
    IK_POINTER_SET_NULL(s_pointer);
  /* fprintf(stderr, "%s: leave\n", __func__); */
  /* Return false so  that the return value  of "$curl-easy-finalise" is
     always false. */
  return IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_reset (ikptr s_easy, ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_RESET
  CURL *	easy = IK_CURL_EASY(s_easy);
  ik_enter_c_function(pcb);
  {
    curl_easy_reset(easy);
  }
  ik_leave_c_function(pcb);
  return IK_VOID;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Configuration options.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_easy_setopt (ikptr s_easy, ikptr s_option, ikptr s_parameter, ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_SETOPT
  CURL *	easy	= IK_CURL_EASY(s_easy);
  CURLoption	option	= ik_integer_to_int(s_option);
  CURLcode	rv;
  if (CURLOPTTYPE_OFF_T <= option)		/* off_t value */
    rv = curl_easy_setopt(easy, option, ik_integer_to_off_t(s_parameter));
  else if (CURLOPTTYPE_FUNCTIONPOINT <= option)	/* callback function */
    rv = curl_easy_setopt(easy, option, IK_VOIDP_FROM_POINTER_OR_FALSE(s_parameter));
  else if (CURLOPTTYPE_OBJECTPOINT <= option) {	/* data pointer */
    void *	parm = IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(s_parameter);
    rv = curl_easy_setopt(easy, option, parm);
  } else if (CURLOPTTYPE_LONG <= option) 	/* long value */
    rv = curl_easy_setopt(easy, option, ik_integer_to_long(s_parameter));
  else
    return IK_FALSE;
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_getinfo (ikptr s_easy, ikptr s_info, ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_GETINFO
  CURL *	easy	= IK_CURL_EASY(s_easy);
  CURLINFO	info	= ik_integer_to_int(s_info);
  CURLcode	rv;
  if (CURLINFO_CERTINFO == info) {
    /* The header file of cURL  version 7.27.0 classifies this option as
       returning a  slist, but  this is not  true: the  returned pointer
       references a "struct curl_certinfo".  For  this reason we have to
       handle  this  option  separately.   (Marco  Maggi;  Mon  Sep  17,
       2012) */
    struct curl_slist *		_result;
    struct curl_certinfo *	result;
    rv = curl_easy_getinfo(easy, info, &_result);
    result = (struct curl_certinfo *)_result;
    if (CURLE_OK == rv) {
      ikptr s_pair = ika_pair_alloc(pcb);
      pcb->root0 = &s_pair;
      {
	IK_ASS(IK_CAR(s_pair), IK_FALSE);
	IK_ASS(IK_CDR(s_pair),
	       ((result)? ika_pointer_alloc(pcb, (ik_ulong)result) : IK_FALSE));
      }
      pcb->root0 = NULL;
      return s_pair;
    }
  } else if (CURLINFO_PRIVATE == info) {
    /* The header file of cURL  version 7.27.0 classifies this option as
       returning a string, but this is not true: the returned pointer is
       a "void *"  referencing whatever data was registered  in the easy
       handler with  the option  CURLOPT_PRIVATE .   For this  reason we
       have to handle this option separately.  (Marco Maggi; Mon Sep 17,
       2012) */
    char *	result;
    rv = curl_easy_getinfo(easy, info, &result);
    if (CURLE_OK == rv) {
      ikptr s_pair = ika_pair_alloc(pcb);
      pcb->root0 = &s_pair;
      {
	IK_ASS(IK_CAR(s_pair), IK_FALSE);
	IK_ASS(IK_CDR(s_pair),
	       ((result)? ika_pointer_alloc(pcb, (ik_ulong)result) : IK_FALSE));
      }
      pcb->root0 = NULL;
      return s_pair;
    }
  }
#if ((defined HAVE_CURL_TLSSESSIONINFO) && (1 == HAVE_CURL_TLSSESSIONINFO))
  else if (CURLINFO_TLS_SESSION == info) {
    /* If successful: return a pair whose  car is false and whose cdr is
       the pointer to the C language struct. */
    struct curl_tlssessioninfo *	result;
    rv = curl_easy_getinfo(easy, info, &result);
    if (CURLE_OK == rv) {
      ikptr s_pair = ika_pair_alloc(pcb);
      pcb->root0 = &s_pair;
      {
	IK_ASS(IK_CAR(s_pair), IK_FALSE);
	IK_ASS(IK_CDR(s_pair),
	       ((result)? ika_pointer_alloc(pcb, (ik_ulong)result) : IK_FALSE));
      }
      pcb->root0 = NULL;
      return s_pair;
    }
  }
#endif
  else {
    switch (CURLINFO_TYPEMASK & info) {
    case CURLINFO_SLIST: {
      struct curl_slist *	result;
      rv = curl_easy_getinfo(easy, info, &result);
      if (CURLE_OK == rv) {
	ikptr s_pair = ika_pair_alloc(pcb);
	pcb->root0 = &s_pair;
	{
	  IK_ASS(IK_CAR(s_pair), ika_integer_from_int(pcb, CURLINFO_SLIST));
	  IK_ASS(IK_CDR(s_pair),
		 ((result)? ika_pointer_alloc(pcb, (ik_ulong)result) : IK_FALSE));
	}
	pcb->root0 = NULL;
	return s_pair;
      }
      break;
    }
    case CURLINFO_DOUBLE: {
      double	result;
      rv = curl_easy_getinfo(easy, info, &result);
      if (CURLE_OK == rv) {
	ikptr s_pair = ika_pair_alloc(pcb);
	pcb->root0 = &s_pair;
	{
	  IK_ASS(IK_CAR(s_pair), ika_integer_from_int(pcb, CURLINFO_DOUBLE));
	  IK_ASS(IK_CDR(s_pair), ika_flonum_from_double(pcb, result));
	}
	pcb->root0 = NULL;
	return s_pair;
      }
      break;
    }
    case CURLINFO_LONG: {
      long	result;
      rv = curl_easy_getinfo(easy, info, &result);
      if (CURLE_OK == rv) {
	ikptr s_pair = ika_pair_alloc(pcb);
	pcb->root0 = &s_pair;
	{
	  IK_ASS(IK_CAR(s_pair), ika_integer_from_int(pcb, CURLINFO_LONG));
	  IK_ASS(IK_CDR(s_pair), ika_integer_from_long(pcb, result));
	}
	pcb->root0 = NULL;
	return s_pair;
      }
      break;
    }
    case CURLINFO_STRING: {
      const char *result;
      rv = curl_easy_getinfo(easy, info, &result);
      if (CURLE_OK == rv) {
	ikptr s_pair = ika_pair_alloc(pcb);
	pcb->root0 = &s_pair;
	{
	  IK_ASS(IK_CAR(s_pair), ika_integer_from_int(pcb, CURLINFO_STRING));
	  IK_ASS(IK_CDR(s_pair),
		 ((result)? ika_bytevector_from_cstring(pcb, result) : IK_FALSE));
	}
	pcb->root0 = NULL;
	return s_pair;
      }
      break;
    }
    default:
      return IK_FALSE; /* unknown info type */
    }
  }
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Easy operations.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_easy_perform (ikptr s_easy, ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_PERFORM
  CURL *	easy = IK_CURL_EASY(s_easy);
  CURLcode	rv;
  ik_enter_c_function(pcb);
  {
    rv = curl_easy_perform(easy);
  }
  ik_leave_c_function(pcb);
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_duphandle (ikptr s_easy, ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_DUPHANDLE
  CURL *	easy = IK_CURL_EASY(s_easy);
  CURL *	rv;
  ik_enter_c_function(pcb);
  {
    rv = curl_easy_duphandle(easy);
  }
  ik_leave_c_function(pcb);
  return (rv)? ika_pointer_alloc(pcb, (ik_ulong)rv) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_pause (ikptr s_easy, ikptr s_bitmask, ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_PAUSE
  CURL *	easy	= IK_CURL_EASY(s_easy);
  int		bitmask	= ik_integer_to_int(s_bitmask);
  CURLcode	rv;
  ik_enter_c_function(pcb);
  {
    rv = curl_easy_pause(easy, bitmask);
  }
  ik_leave_c_function(pcb);
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Sending and receiving data.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_easy_recv (ikptr s_easy, ikptr s_buffer, ikptr s_buflen, ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_RECV
  CURL *	easy	= IK_CURL_EASY(s_easy);
  void *	buffer	= IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_buffer);
  size_t	buflen;
  size_t	received;
  CURLcode	rv;
  if (IK_IS_BYTEVECTOR(s_buffer))
    buflen = IK_BYTEVECTOR_LENGTH(s_buffer);
  else if (IK_IS_POINTER(s_buffer))
    buflen = ik_integer_to_size_t(s_buflen);
  else
    buflen = IK_MBLOCK_SIZE_T(s_buffer);
  ik_enter_c_function(pcb);
  {
    rv = curl_easy_recv(easy, buffer, buflen, &received);
  }
  ik_leave_c_function(pcb);
  if (CURLE_OK == rv) {
    ikptr	s_pair = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_curlcode(pcb, rv));
      IK_ASS(IK_CDR(s_pair), ika_integer_from_size_t(pcb, received));
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_send (ikptr s_easy, ikptr s_buffer, ikptr s_buflen, ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_SEND
  CURL *	easy	= IK_CURL_EASY(s_easy);
  void *	buffer	= IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_buffer);
  size_t	buflen;
  size_t	sent;
  CURLcode	rv;
  if (IK_IS_BYTEVECTOR(s_buffer))
    buflen = IK_BYTEVECTOR_LENGTH(s_buffer);
  else if (IK_IS_POINTER(s_buffer))
    buflen = ik_integer_to_size_t(s_buflen);
  else
    buflen = IK_MBLOCK_SIZE_T(s_buffer);
  ik_enter_c_function(pcb);
  {
    rv = curl_easy_send(easy, buffer, buflen, &sent);
  }
  ik_leave_c_function(pcb);
  if (CURLE_OK == rv) {
    ikptr	s_pair = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_curlcode(pcb, rv));
      IK_ASS(IK_CDR(s_pair), ika_integer_from_size_t(pcb, sent));
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Escaping URL strings.
 ** ----------------------------------------------------------------- */

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
  if (rv) {
    ikptr	result = ika_bytevector_from_cstring(pcb, rv);
    curl_free(rv);
    return result;
  } else
    return IK_FALSE;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_unescape (ikptr s_easy, ikptr s_chars, ikptr s_in_length, ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_UNESCAPE
  CURL *	easy = IK_CURL_EASY(s_easy);
  const char *	chars	= IK_CHARP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK(s_chars);
  int		in_length;
  int		ou_length;
  char *	rv;
  if (IK_IS_BYTEVECTOR(s_chars))
    in_length = IK_BYTEVECTOR_LENGTH(s_chars);
  else if (IK_IS_POINTER(s_chars))
    in_length = ik_integer_to_int(s_in_length);
  else
    in_length = IK_MBLOCK_SIZE_T(s_chars);
  rv = curl_easy_unescape(easy, chars, in_length, &ou_length);
  if (rv) {
    ikptr	result = ika_bytevector_from_memory_block(pcb, rv, ou_length);
    curl_free(rv);
    return result;
  } else
    return IK_FALSE;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Miscellaneous functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_easy_strerror (ikptr s_code, ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_STRERROR
  CURLcode	code = ik_integer_to_int(s_code);
  const char *	rv;
  rv = curl_easy_strerror(code);
  return (rv)? ika_bytevector_from_cstring(pcb, rv) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}

/* end of file */
