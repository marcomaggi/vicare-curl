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
    {
      ikptr	sk;
      sk = ik_enter_c_function(pcb);
      {
	rv = curl_multi_cleanup(multi);
      }
      ik_leave_c_function(pcb, sk);
    }
    IK_POINTER_SET_NULL(s_pointer);
    return ika_integer_from_curlcode(pcb, rv);
  } else
    return ika_integer_from_curlcode(pcb, CURLM_OK);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Multi API: adding and removing easy handles.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_multi_add_handle (ikptr s_multi, ikptr s_easy, ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_ADD_HANDLE
  CURLM *	multi	= IK_CURL_MULTI(s_multi);
  CURL *	easy	= IK_CURL_MULTI(s_easy);
  CURLMcode	rv;
  {
    ikptr	sk;
    sk = ik_enter_c_function(pcb);
    {
      rv = curl_multi_add_handle(multi, easy);
    }
    ik_leave_c_function(pcb, sk);
  }
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_remove_handle (ikptr s_multi, ikptr s_easy, ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_REMOVE_HANDLE
  CURLM *	multi	= IK_CURL_MULTI(s_multi);
  CURL *	easy	= IK_CURL_MULTI(s_easy);
  CURLMcode	rv;
  {
    ikptr	sk;
    sk = ik_enter_c_function(pcb);
    {
      rv = curl_multi_remove_handle(multi, easy);
    }
    ik_leave_c_function(pcb, sk);
  }
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Multi API: setting options.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_multi_setopt (ikptr s_multi, ikptr s_option, ikptr s_parameter, ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_SETOPT
  CURLM *	multi	= IK_CURL_MULTI(s_multi);
  CURLMoption	option	= ik_integer_to_int(s_option);
  CURLMcode	rv;
  if (CURLOPTTYPE_OFF_T <= option)		/* off_t value */
    rv = curl_multi_setopt(multi, option, ik_integer_to_off_t(s_parameter));
  else if (CURLOPTTYPE_FUNCTIONPOINT <= option)	/* callback function */
    rv = curl_multi_setopt(multi, option, IK_VOIDP_FROM_POINTER_OR_FALSE(s_parameter));
  else if (CURLOPTTYPE_OBJECTPOINT <= option) {	/* data pointer */
    void *	parm = IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(s_parameter);
    rv = curl_multi_setopt(multi, option, parm);
  } else if (CURLOPTTYPE_LONG <= option) 	/* long value */
    rv = curl_multi_setopt(multi, option, ik_integer_to_long(s_parameter));
  else
    return IK_FALSE;
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Multi API: performing.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_multi_perform (ikptr s_multi, ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_PERFORM
  CURLM *	multi	= IK_CURL_MULTI(s_multi);
  int		running_handles = 0;
  CURLMcode	rv;
  {
    ikptr	sk;
    sk = ik_enter_c_function(pcb);
    {
      rv = curl_multi_perform(multi, &running_handles);
    }
    ik_leave_c_function(pcb, sk);
  }
  {
    ikptr	s_pair = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_curlcode(pcb, rv));
      IK_ASS(IK_CDR(s_pair), ika_integer_from_int(pcb, running_handles));
    }
    pcb->root0 = NULL;
    return s_pair;
  }
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Multi API: socket functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_multi_fdset (ikptr s_multi,
		       ikptr s_read_fds, ikptr s_write_fds, ikptr s_exc_fds,
		       ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_FDSET
  CURLM *	multi		= IK_CURL_MULTI(s_multi);
  /* These   fd_sets   must   be    pointers   or   memory-blocks,   not
     bytevectors. */
  fd_set *	read_fds	= \
    IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(s_read_fds);
  fd_set *	write_fds	= \
    IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(s_write_fds);
  fd_set *	exc_fds		= \
    IK_VOIDP_FROM_BYTEVECTOR_OR_POINTER_OR_MBLOCK_OR_FALSE(s_exc_fds);
  fd_set	empty_read_fds;
  fd_set	empty_write_fds;
  fd_set	empty_exc_fds;
  int		max_fd = -1;
  CURLMcode	rv;
  if (NULL == read_fds) {
    read_fds = &empty_read_fds;
    FD_ZERO(read_fds);
  }
  if (NULL == write_fds) {
    write_fds = &empty_write_fds;
    FD_ZERO(write_fds);
  }
  if (NULL == exc_fds) {
    exc_fds = &empty_exc_fds;
    FD_ZERO(exc_fds);
  }
  {
    ikptr	sk;
    sk = ik_enter_c_function(pcb);
    {
      rv = curl_multi_fdset(multi, read_fds, write_fds, exc_fds, &max_fd);
    }
    ik_leave_c_function(pcb, sk);
  }
  {
    ikptr	s_pair = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_curlcode(pcb, rv));
      IK_ASS(IK_CDR(s_pair), ika_integer_from_int(pcb, max_fd));
    }
    pcb->root0 = NULL;
    return s_pair;
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_socket_action (ikptr s_multi, ikptr s_sock_fd, ikptr s_ev_bitmask, ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_SOCKET_ACTION
  CURLM *	multi		= IK_CURL_MULTI(s_multi);
  int		sock_fd		= IK_UNFIX(s_sock_fd);
  int		ev_bitmask	= ik_integer_to_int(s_ev_bitmask);
  int		running_handles = 0;
  CURLMcode	rv;
  {
    ikptr	sk;
    sk = ik_enter_c_function(pcb);
    {
      rv = curl_multi_socket_action(multi, sock_fd, ev_bitmask, &running_handles);
    }
    ik_leave_c_function(pcb, sk);
  }
  {
    ikptr	s_pair = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_curlcode(pcb, rv));
      IK_ASS(IK_CDR(s_pair), ika_integer_from_int(pcb, running_handles));
    }
    pcb->root0 = NULL;
    return s_pair;
  }
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_curl_multi_timeout (ikptr s_multi, ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_TIMEOUT
  CURLM *	multi	= IK_CURL_MULTI(s_multi);
  long		timeout_milliseconds;
  CURLMcode	rv;
  rv = curl_multi_timeout(multi, &timeout_milliseconds);
  if (CURLM_OK == rv) {
    ikptr	s_pair = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_curlcode(pcb, CURLM_OK));
      IK_ASS(IK_CDR(s_pair), ika_integer_from_long(pcb, timeout_milliseconds));
    }
    pcb->root0 = NULL;
    return s_pair;
  } else
    return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_curl_multi_assign (ikptr s_multi, ikptr s_fd, ikptr s_custom_data, ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_ASSIGN
  CURLM *	multi	= IK_CURL_MULTI(s_multi);
  int		fd	= IK_UNFIX(s_fd);
  void *	data	= IK_VOIDP_FROM_POINTER_OR_FALSE(s_custom_data);
  CURLMcode	rv;
  rv = curl_multi_assign(multi, fd, data);
  return ika_integer_from_curlcode(pcb, rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Multi API: miscellaneous functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_multi_info_read (ikptr s_multi, ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_INFO_READ
  CURLM *	multi	= IK_CURL_MULTI(s_multi);
  int		number_of_messages_in_queue;
  CURLMsg *	next_msg;
  ikptr		s_pair;
  next_msg = curl_multi_info_read(multi, &number_of_messages_in_queue);
  s_pair = ika_pair_alloc(pcb);
  pcb->root0 = &s_pair;
  {
    IK_ASS(IK_CAR(s_pair),
	   ((next_msg)? ika_pointer_alloc(pcb, (ik_ulong)next_msg) : IK_FALSE));
    IK_ASS(IK_CDR(s_pair), ika_integer_from_int(pcb, number_of_messages_in_queue));
  }
  pcb->root0 = NULL;
  return s_pair;
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_strerror (ikptr s_code, ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_STRERROR
  CURLMcode	code = ik_integer_to_int(s_code);
  const char *	rv;
  rv = curl_multi_strerror(code);
  return (rv)? ika_bytevector_from_cstring(pcb, rv) : IK_FALSE;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Multi API: deprecated functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_multi_socket (ikptr s_multi, ikptr s_sock_fd, ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_SOCKET
  CURLM *	multi	= IK_CURL_MULTI(s_multi);
  int		sock	= IK_UNFIX(s_sock_fd);
  int		running_handles = 0;
  CURLMcode	rv;
  {
    ikptr	sk;
    sk = ik_enter_c_function(pcb);
    {
      rv = curl_multi_socket(multi, sock, &running_handles);
    }
    ik_leave_c_function(pcb, sk);
  }
  {
    ikptr	s_pair = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_curlcode(pcb, rv));
      IK_ASS(IK_CDR(s_pair), ika_integer_from_int(pcb, running_handles));
    }
    pcb->root0 = NULL;
    return s_pair;
  }
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_socket_all (ikptr s_multi, ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_SOCKET_ALL
  CURLM *	multi	= IK_CURL_MULTI(s_multi);
  int		running_handles = 0;
  CURLMcode	rv;
  {
    ikptr	sk;
    sk = ik_enter_c_function(pcb);
    {
      rv = curl_multi_socket_all(multi, &running_handles);
    }
    ik_leave_c_function(pcb, sk);
  }
  {
    ikptr	s_pair = ika_pair_alloc(pcb);
    pcb->root0 = &s_pair;
    {
      IK_ASS(IK_CAR(s_pair), ika_integer_from_curlcode(pcb, rv));
      IK_ASS(IK_CDR(s_pair), ika_integer_from_int(pcb, running_handles));
    }
    pcb->root0 = NULL;
    return s_pair;
  }
#else
  feature_failure(__func__);
#endif
}


/* end of file */
