/*
  Part of: Vicare/cURL
  Contents: print platform features library
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


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <stdlib.h>


int
main (int argc, const char *const argv[])
{
  printf(";;; -*- coding: utf-8-unix -*-\n\
;;;\n\
;;;Part of: Vicare/cURL\n\
;;;Contents: static platform inspection\n\
;;;Date: Wed Sep  5, 2012\n\
;;;\n\
;;;Abstract\n\
;;;\n\
;;;\n\
;;;\n\
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>\n\
;;;\n\
;;;This program is free software:  you can redistribute it and/or modify\n\
;;;it under the terms of the  GNU General Public License as published by\n\
;;;the Free Software Foundation, either version 3 of the License, or (at\n\
;;;your option) any later version.\n\
;;;\n\
;;;This program is  distributed in the hope that it  will be useful, but\n\
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of\n\
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU\n\
;;;General Public License for more details.\n\
;;;\n\
;;;You should  have received a  copy of  the GNU General  Public License\n\
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\
;;;\n\
\n\
\n\
#!r6rs\n\
(library (vicare net curl features)\n\
  (export\n\
    HAVE_CURL_VERSION\n\
    HAVE_CURL_VERSION_INFO\n\
    HAVE_CURL_GLOBAL_INIT\n\
    HAVE_CURL_GLOBAL_INIT_MEM\n\
    HAVE_CURL_GLOBAL_CLEANUP\n\
    HAVE_CURL_FREE\n\
    HAVE_CURL_SLIST_APPEND\n\
    HAVE_CURL_SLIST_FREE_ALL\n\
    HAVE_CURL_FORMADD\n\
    HAVE_CURL_FORMGET\n\
    HAVE_CURL_FORMFREE\n\
    HAVE_CURL_EASY_ESCAPE\n\
    HAVE_CURL_ESCAPE\n\
    HAVE_CURL_EASY_UNESCAPE\n\
    HAVE_CURL_UNESCAPE\n\
    HAVE_CURL_GETDATE\n\
    HAVE_CURL_SHARE_INIT\n\
    HAVE_CURL_SHARE_SETOPT\n\
    HAVE_CURL_SHARE_CLEANUP\n\
    HAVE_CURL_SHARE_STRERROR\n\
    HAVE_CURL_EASY_INIT\n\
    HAVE_CURL_EASY_SETOPT\n\
    HAVE_CURL_EASY_PERFORM\n\
    HAVE_CURL_EASY_CLEANUP\n\
    HAVE_CURL_EASY_GETINFO\n\
    HAVE_CURL_EASY_DUPHANDLE\n\
    HAVE_CURL_EASY_RESET\n\
    HAVE_CURL_EASY_RECV\n\
    HAVE_CURL_EASY_SEND\n\
    HAVE_CURL_EASY_STRERROR\n\
    HAVE_CURL_EASY_PAUSE\n\
    HAVE_CURL_MULTI_INIT\n\
    HAVE_CURL_MULTI_ADD_HANDLE\n\
    HAVE_CURL_MULTI_REMOVE_HANDLE\n\
    HAVE_CURL_MULTI_FDSET\n\
    HAVE_CURL_MULTI_PERFORM\n\
    HAVE_CURL_MULTI_CLEANUP\n\
    HAVE_CURL_MULTI_INFO_READ\n\
    HAVE_CURL_MULTI_STRERROR\n\
    HAVE_CURL_MULTI_SOCKET\n\
    HAVE_CURL_MULTI_SOCKET_ACTION\n\
    HAVE_CURL_MULTI_SOCKET_ALL\n\
    HAVE_CURL_MULTI_TIMEOUT\n\
    HAVE_CURL_MULTI_SETOPT\n\
    HAVE_CURL_MULTI_ASSIGN\n\
    )\n\
  (import (rnrs))\n\
\n\
;;;; helpers\n\
\n\
(define-syntax define-inline-constant\n\
  (syntax-rules ()\n\
    ((_ ?name ?value)\n\
     (define-syntax ?name (identifier-syntax ?value)))))\n\
\n\
\n\
;;;; code\n\n");


printf("(define-inline-constant HAVE_CURL_VERSION %s)\n",
#ifdef HAVE_CURL_VERSION
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_VERSION_INFO %s)\n",
#ifdef HAVE_CURL_VERSION_INFO
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_GLOBAL_INIT %s)\n",
#ifdef HAVE_CURL_GLOBAL_INIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_GLOBAL_INIT_MEM %s)\n",
#ifdef HAVE_CURL_GLOBAL_INIT_MEM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_GLOBAL_CLEANUP %s)\n",
#ifdef HAVE_CURL_GLOBAL_CLEANUP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_FREE %s)\n",
#ifdef HAVE_CURL_FREE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_SLIST_APPEND %s)\n",
#ifdef HAVE_CURL_SLIST_APPEND
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_SLIST_FREE_ALL %s)\n",
#ifdef HAVE_CURL_SLIST_FREE_ALL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_FORMADD %s)\n",
#ifdef HAVE_CURL_FORMADD
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_FORMGET %s)\n",
#ifdef HAVE_CURL_FORMGET
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_FORMFREE %s)\n",
#ifdef HAVE_CURL_FORMFREE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_EASY_ESCAPE %s)\n",
#ifdef HAVE_CURL_EASY_ESCAPE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_ESCAPE %s)\n",
#ifdef HAVE_CURL_ESCAPE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_EASY_UNESCAPE %s)\n",
#ifdef HAVE_CURL_EASY_UNESCAPE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_UNESCAPE %s)\n",
#ifdef HAVE_CURL_UNESCAPE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_GETDATE %s)\n",
#ifdef HAVE_CURL_GETDATE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_SHARE_INIT %s)\n",
#ifdef HAVE_CURL_SHARE_INIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_SHARE_SETOPT %s)\n",
#ifdef HAVE_CURL_SHARE_SETOPT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_SHARE_CLEANUP %s)\n",
#ifdef HAVE_CURL_SHARE_CLEANUP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_SHARE_STRERROR %s)\n",
#ifdef HAVE_CURL_SHARE_STRERROR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_EASY_INIT %s)\n",
#ifdef HAVE_CURL_EASY_INIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_EASY_SETOPT %s)\n",
#ifdef HAVE_CURL_EASY_SETOPT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_EASY_PERFORM %s)\n",
#ifdef HAVE_CURL_EASY_PERFORM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_EASY_CLEANUP %s)\n",
#ifdef HAVE_CURL_EASY_CLEANUP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_EASY_GETINFO %s)\n",
#ifdef HAVE_CURL_EASY_GETINFO
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_EASY_DUPHANDLE %s)\n",
#ifdef HAVE_CURL_EASY_DUPHANDLE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_EASY_RESET %s)\n",
#ifdef HAVE_CURL_EASY_RESET
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_EASY_RECV %s)\n",
#ifdef HAVE_CURL_EASY_RECV
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_EASY_SEND %s)\n",
#ifdef HAVE_CURL_EASY_SEND
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_EASY_STRERROR %s)\n",
#ifdef HAVE_CURL_EASY_STRERROR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_EASY_PAUSE %s)\n",
#ifdef HAVE_CURL_EASY_PAUSE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_INIT %s)\n",
#ifdef HAVE_CURL_MULTI_INIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_ADD_HANDLE %s)\n",
#ifdef HAVE_CURL_MULTI_ADD_HANDLE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_REMOVE_HANDLE %s)\n",
#ifdef HAVE_CURL_MULTI_REMOVE_HANDLE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_FDSET %s)\n",
#ifdef HAVE_CURL_MULTI_FDSET
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_PERFORM %s)\n",
#ifdef HAVE_CURL_MULTI_PERFORM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_CLEANUP %s)\n",
#ifdef HAVE_CURL_MULTI_CLEANUP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_INFO_READ %s)\n",
#ifdef HAVE_CURL_MULTI_INFO_READ
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_STRERROR %s)\n",
#ifdef HAVE_CURL_MULTI_STRERROR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_SOCKET %s)\n",
#ifdef HAVE_CURL_MULTI_SOCKET
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_SOCKET_ACTION %s)\n",
#ifdef HAVE_CURL_MULTI_SOCKET_ACTION
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_SOCKET_ALL %s)\n",
#ifdef HAVE_CURL_MULTI_SOCKET_ALL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_TIMEOUT %s)\n",
#ifdef HAVE_CURL_MULTI_TIMEOUT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_SETOPT %s)\n",
#ifdef HAVE_CURL_MULTI_SETOPT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CURL_MULTI_ASSIGN %s)\n",
#ifdef HAVE_CURL_MULTI_ASSIGN
  "#t"
#else
  "#f"
#endif
  );


  printf("\n\
;;;; done\n\
\n\
)\n\
\n\
;;; end of file\n");
  exit(EXIT_SUCCESS);
}

/* end of file */
