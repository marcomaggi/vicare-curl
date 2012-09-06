;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/cURL
;;;Contents: unsafe interface to the C language API
;;;Date: Wed Sep  5, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare net curl unsafe-capi)
  (export

    ;; version functions
    vicare-curl-version-interface-current
    vicare-curl-version-interface-revision
    vicare-curl-version-interface-age
    vicare-curl-version
    curl-version			curl-version-info

    ;; initialisation functions
    curl-global-init			curl-global-init-mem
    curl-global-cleanup

    ;; miscellaneous functions
    curl-free

;;; --------------------------------------------------------------------
;;; still to be implemented

    curl-slist-append
    curl-slist-free-all
    curl-formadd
    curl-formget
    curl-formfree
    curl-easy-escape
    curl-escape
    curl-easy-unescape
    curl-unescape
    curl-getdate
    curl-share-init
    curl-share-setopt
    curl-share-cleanup
    curl-share-strerror
    curl-easy-init
    curl-easy-setopt
    curl-easy-perform
    curl-easy-cleanup
    curl-easy-getinfo
    curl-easy-duphandle
    curl-easy-reset
    curl-easy-recv
    curl-easy-send
    curl-easy-strerror
    curl-easy-pause
    curl-multi-init
    curl-multi-add-handle
    curl-multi-remove-handle
    curl-multi-fdset
    curl-multi-perform
    curl-multi-cleanup
    curl-multi-info-read
    curl-multi-strerror
    curl-multi-socket
    curl-multi-socket-action
    curl-multi-socket-all
    curl-multi-timeout
    curl-multi-setopt
    curl-multi-assign
    )
  (import (vicare))


;;;; helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))


;;;; version functions

(define-inline (vicare-curl-version-interface-current)
  (foreign-call "ikrt_vicare_curl_version_interface_current"))

(define-inline (vicare-curl-version-interface-revision)
  (foreign-call "ikrt_vicare_curl_version_interface_revision"))

(define-inline (vicare-curl-version-interface-age)
  (foreign-call "ikrt_vicare_curl_version_interface_age"))

(define-inline (vicare-curl-version)
  (foreign-call "ikrt_vicare_curl_version"))

;;; --------------------------------------------------------------------

(define-inline (curl-version)
  (foreign-call "ikrt_curl_version"))

(define-inline (curl-version-info rtd version-code)
  (foreign-call "ikrt_curl_version_info" rtd version-code))


;;;; initialisation functions

(define-inline (curl-global-init flags)
  (foreign-call "ikrt_curl_global_init" flags))

(define-inline (curl-global-init-mem flags
				     malloc-callback free-callback realloc-callback
				     strdup-callback calloc-callback)
  (foreign-call "ikrt_curl_global_init_mem" flags
		malloc-callback free-callback realloc-callback
		strdup-callback calloc-callback))

(define-inline (curl-global-cleanup)
  (foreign-call "ikrt_curl_global_cleanup"))


;;;; miscellaneous functions

(define-inline (curl-free ptr)
  (foreign-call "ikrt_curl_free" ptr))


;;;; still to be implemented

(define-inline (curl-slist-append)
  (foreign-call "ikrt_curl_slist_append"))

(define-inline (curl-slist-free-all)
  (foreign-call "ikrt_curl_slist_free_all"))

(define-inline (curl-formadd)
  (foreign-call "ikrt_curl_formadd"))

(define-inline (curl-formget)
  (foreign-call "ikrt_curl_formget"))

(define-inline (curl-formfree)
  (foreign-call "ikrt_curl_formfree"))

(define-inline (curl-easy-escape)
  (foreign-call "ikrt_curl_easy_escape"))

(define-inline (curl-escape)
  (foreign-call "ikrt_curl_escape"))

(define-inline (curl-easy-unescape)
  (foreign-call "ikrt_curl_easy_unescape"))

(define-inline (curl-unescape)
  (foreign-call "ikrt_curl_unescape"))

(define-inline (curl-getdate)
  (foreign-call "ikrt_curl_getdate"))

(define-inline (curl-share-init)
  (foreign-call "ikrt_curl_share_init"))

(define-inline (curl-share-setopt)
  (foreign-call "ikrt_curl_share_setopt"))

(define-inline (curl-share-cleanup)
  (foreign-call "ikrt_curl_share_cleanup"))

(define-inline (curl-share-strerror)
  (foreign-call "ikrt_curl_share_strerror"))

(define-inline (curl-easy-init)
  (foreign-call "ikrt_curl_easy_init"))

(define-inline (curl-easy-setopt)
  (foreign-call "ikrt_curl_easy_setopt"))

(define-inline (curl-easy-perform)
  (foreign-call "ikrt_curl_easy_perform"))

(define-inline (curl-easy-cleanup)
  (foreign-call "ikrt_curl_easy_cleanup"))

(define-inline (curl-easy-getinfo)
  (foreign-call "ikrt_curl_easy_getinfo"))

(define-inline (curl-easy-duphandle)
  (foreign-call "ikrt_curl_easy_duphandle"))

(define-inline (curl-easy-reset)
  (foreign-call "ikrt_curl_easy_reset"))

(define-inline (curl-easy-recv)
  (foreign-call "ikrt_curl_easy_recv"))

(define-inline (curl-easy-send)
  (foreign-call "ikrt_curl_easy_send"))

(define-inline (curl-easy-strerror)
  (foreign-call "ikrt_curl_easy_strerror"))

(define-inline (curl-easy-pause)
  (foreign-call "ikrt_curl_easy_pause"))

(define-inline (curl-multi-init)
  (foreign-call "ikrt_curl_multi_init"))

(define-inline (curl-multi-add-handle)
  (foreign-call "ikrt_curl_multi_add_handle"))

(define-inline (curl-multi-remove-handle)
  (foreign-call "ikrt_curl_multi_remove_handle"))

(define-inline (curl-multi-fdset)
  (foreign-call "ikrt_curl_multi_fdset"))

(define-inline (curl-multi-perform)
  (foreign-call "ikrt_curl_multi_perform"))

(define-inline (curl-multi-cleanup)
  (foreign-call "ikrt_curl_multi_cleanup"))

(define-inline (curl-multi-info-read)
  (foreign-call "ikrt_curl_multi_info_read"))

(define-inline (curl-multi-strerror)
  (foreign-call "ikrt_curl_multi_strerror"))

(define-inline (curl-multi-socket)
  (foreign-call "ikrt_curl_multi_socket"))

(define-inline (curl-multi-socket-action)
  (foreign-call "ikrt_curl_multi_socket_action"))

(define-inline (curl-multi-socket-all)
  (foreign-call "ikrt_curl_multi_socket_all"))

(define-inline (curl-multi-timeout)
  (foreign-call "ikrt_curl_multi_timeout"))

(define-inline (curl-multi-setopt)
  (foreign-call "ikrt_curl_multi_setopt"))

(define-inline (curl-multi-assign)
  (foreign-call "ikrt_curl_multi_assign"))


;;;; done

)

;;; end of file
