/*
  Part of: Vicare/cURL
  Contents: functions still to be interfaced
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


/** --------------------------------------------------------------------
 ** Still to be interfaced.
 ** ----------------------------------------------------------------- */

#if 0
ikptr
ikrt_curl_slist_append (ikpcb * pcb)
{
#ifdef HAVE_CURL_SLIST_APPEND
  curl_slist_append();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_slist_free_all (ikpcb * pcb)
{
#ifdef HAVE_CURL_SLIST_FREE_ALL
  curl_slist_free_all();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_formadd (ikpcb * pcb)
{
#ifdef HAVE_CURL_FORMADD
  curl_formadd();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_formget (ikpcb * pcb)
{
#ifdef HAVE_CURL_FORMGET
  curl_formget();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_formfree (ikpcb * pcb)
{
#ifdef HAVE_CURL_FORMFREE
  curl_formfree();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_escape (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_ESCAPE
  curl_easy_escape();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_escape (ikpcb * pcb)
{
#ifdef HAVE_CURL_ESCAPE
  curl_escape();
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
ikptr
ikrt_curl_unescape (ikpcb * pcb)
{
#ifdef HAVE_CURL_UNESCAPE
  curl_unescape();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_getdate (ikpcb * pcb)
{
#ifdef HAVE_CURL_GETDATE
  curl_getdate();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_share_init (ikpcb * pcb)
{
#ifdef HAVE_CURL_SHARE_INIT
  curl_share_init();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_share_setopt (ikpcb * pcb)
{
#ifdef HAVE_CURL_SHARE_SETOPT
  curl_share_setopt();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_share_cleanup (ikpcb * pcb)
{
#ifdef HAVE_CURL_SHARE_CLEANUP
  curl_share_cleanup();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_share_strerror (ikpcb * pcb)
{
#ifdef HAVE_CURL_SHARE_STRERROR
  curl_share_strerror();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_init (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_INIT
  curl_easy_init();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_setopt (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_SETOPT
  curl_easy_setopt();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_perform (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_PERFORM
  curl_easy_perform();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_cleanup (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_CLEANUP
  curl_easy_cleanup();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_getinfo (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_GETINFO
  curl_easy_getinfo();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_duphandle (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_DUPHANDLE
  curl_easy_duphandle();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_reset (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_RESET
  curl_easy_reset();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_recv (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_RECV
  curl_easy_recv();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_send (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_SEND
  curl_easy_send();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_strerror (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_STRERROR
  curl_easy_strerror();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_easy_pause (ikpcb * pcb)
{
#ifdef HAVE_CURL_EASY_PAUSE
  curl_easy_pause();
#else
  feature_failure(__func__);
#endif
}
ikptr
ikrt_curl_multi_init (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_INIT
  curl_multi_init();
#else
  feature_failure(__func__);
#endif
}
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
ikrt_curl_multi_cleanup (ikpcb * pcb)
{
#ifdef HAVE_CURL_MULTI_CLEANUP
  curl_multi_cleanup();
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
