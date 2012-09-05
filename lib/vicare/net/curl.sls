;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/cURL
;;;Contents: Libcurl bindings
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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
#!(load-shared-library "vicare-curl")
(library (vicare net curl)
  (export

    ;; version numbers and strings
    vicare-curl-version-interface-current
    vicare-curl-version-interface-revision
    vicare-curl-version-interface-age
    vicare-curl-version

;;; --------------------------------------------------------------------

    ;; still to be implemented
    curl-version
    curl-version-info
    curl-global-init
    curl-global-init-mem
    curl-global-cleanup
    curl-free
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
  (import (vicare)
    (vicare net curl constants)
    (prefix (vicare net curl unsafe-capi) capi.)
    (vicare syntactic-extensions)
    #;(prefix (vicare words) words.))


;;;; arguments validation

#;(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))

#;(define-argument-validation (pointer who obj)
  (pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

#;(define-argument-validation (callback who obj)
  (pointer? obj)
  (assertion-violation who "expected callback as argument" obj))

#;(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))



;;;; version functions

(define (vicare-curl-version-interface-current)
  (capi.vicare-curl-version-interface-current))

(define (vicare-curl-version-interface-revision)
  (capi.vicare-curl-version-interface-revision))

(define (vicare-curl-version-interface-age)
  (capi.vicare-curl-version-interface-age))

(define (vicare-curl-version)
  (ascii->string (capi.vicare-curl-version)))


;;;; callback makers

 ;; int (*curl_progress_callback)(void *clientp,
 ;;                                      double dltotal,
 ;;                                      double dlnow,
 ;;                                      double ultotal,
 ;;                                      double ulnow);

 ;; size_t (*curl_write_callback)(char *buffer,
 ;;                                      size_t size,
 ;;                                      size_t nitems,
 ;;                                      void *outstream);

 ;; long (*curl_chunk_bgn_callback)(const void *transfer_info,
 ;;                                        void *ptr,
 ;;                                        int remains);

;; long (*curl_chunk_end_callback)(void *ptr);

 ;; int (*curl_fnmatch_callback)(void *ptr,
 ;;                                     const char *pattern,
 ;;                                     const char *string);

 ;; int (*curl_seek_callback)(void *instream,
 ;;                                  curl_off_t offset,
 ;;                                  int origin);

 ;; size_t (*curl_read_callback)(char *buffer,
 ;;                                      size_t size,
 ;;                                      size_t nitems,
 ;;                                      void *instream);

 ;; int (*curl_sockopt_callback)(void *clientp,
 ;;                                     curl_socket_t curlfd,
 ;;                                     curlsocktype purpose);

;;  curl_socket_t
;; (*curl_opensocket_callback)(void *clientp,
;;                             curlsocktype purpose,
;;                             struct curl_sockaddr *address);

;; int
;; (*curl_closesocket_callback)(void *clientp, curl_socket_t item);

 ;; curlioerr (*curl_ioctl_callback)(CURL *handle,
 ;;                                         int cmd,
 ;;                                         void *clientp);

 ;; int (*curl_debug_callback)
 ;;       (CURL *handle,      /* the handle/transfer this concerns */
 ;;        curl_infotype type, /* what kind of data */
 ;;        char *data,        /* points to the data */
 ;;        size_t size,       /* size of the data pointed to */
 ;;        void *userptr);    /* whatever the user please */

 ;; CURLcode (*curl_conv_callback)(char *buffer, size_t length);

 ;; CURLcode (*curl_ssl_ctx_callback)(CURL *curl,    /* easy handle */
 ;;                                          void *ssl_ctx, /* actually an
 ;;                                                            OpenSSL SSL_CTX */
 ;;                                          void *userptr);

 ;; int
 ;;  (*curl_sshkeycallback) (CURL *easy,     /* easy handle */
 ;;                          const struct curl_khkey *knownkey, /* known */
 ;;                          const struct curl_khkey *foundkey, /* found */
 ;;                          enum curl_khmatch, /* libcurl's view on the keys */
 ;;                          void *clientp); /* custom pointer passed from app */

 ;; size_t (*curl_formget_callback)(void *arg, const char *buf,
 ;;                                        size_t len);

 ;; void (*curl_lock_function)(CURL *handle,
 ;;                                   curl_lock_data data,
 ;;                                   curl_lock_access locktype,
 ;;                                   void *userptr);

 ;; void (*curl_unlock_function)(CURL *handle,
 ;;                                     curl_lock_data data,
 ;;                                     void *userptr);

 ;; int (*curl_socket_callback)(CURL *easy,
 ;;                                    curl_socket_t s,
 ;;                                    int what,
 ;;                                    void *userp,
 ;;                                    void *socketp);

 ;; int (*curl_multi_timer_callback)(CURLM *multi, long timeout_ms, void *userp);



;;;; still to be implemented

(define-inline (unimplemented who)
  (assertion-violation who "unimplemented function"))

(define (curl-version . args)
  (define who 'curl-version)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-version-info . args)
  (define who 'curl-version-info)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-global-init . args)
  (define who 'curl-global-init)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-global-init-mem . args)
  (define who 'curl-global-init-mem)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-global-cleanup . args)
  (define who 'curl-global-cleanup)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-free . args)
  (define who 'curl-free)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-slist-append . args)
  (define who 'curl-slist-append)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-slist-free-all . args)
  (define who 'curl-slist-free-all)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-formadd . args)
  (define who 'curl-formadd)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-formget . args)
  (define who 'curl-formget)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-formfree . args)
  (define who 'curl-formfree)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-easy-escape . args)
  (define who 'curl-easy-escape)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-escape . args)
  (define who 'curl-escape)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-easy-unescape . args)
  (define who 'curl-easy-unescape)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-unescape . args)
  (define who 'curl-unescape)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-getdate . args)
  (define who 'curl-getdate)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-share-init . args)
  (define who 'curl-share-init)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-share-setopt . args)
  (define who 'curl-share-setopt)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-share-cleanup . args)
  (define who 'curl-share-cleanup)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-share-strerror . args)
  (define who 'curl-share-strerror)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-easy-init . args)
  (define who 'curl-easy-init)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-easy-setopt . args)
  (define who 'curl-easy-setopt)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-easy-perform . args)
  (define who 'curl-easy-perform)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-easy-cleanup . args)
  (define who 'curl-easy-cleanup)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-easy-getinfo . args)
  (define who 'curl-easy-getinfo)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-easy-duphandle . args)
  (define who 'curl-easy-duphandle)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-easy-reset . args)
  (define who 'curl-easy-reset)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-easy-recv . args)
  (define who 'curl-easy-recv)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-easy-send . args)
  (define who 'curl-easy-send)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-easy-strerror . args)
  (define who 'curl-easy-strerror)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-easy-pause . args)
  (define who 'curl-easy-pause)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-init . args)
  (define who 'curl-multi-init)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-add-handle . args)
  (define who 'curl-multi-add-handle)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-remove-handle . args)
  (define who 'curl-multi-remove-handle)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-fdset . args)
  (define who 'curl-multi-fdset)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-perform . args)
  (define who 'curl-multi-perform)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-cleanup . args)
  (define who 'curl-multi-cleanup)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-info-read . args)
  (define who 'curl-multi-info-read)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-strerror . args)
  (define who 'curl-multi-strerror)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-socket . args)
  (define who 'curl-multi-socket)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-socket-action . args)
  (define who 'curl-multi-socket-action)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-socket-all . args)
  (define who 'curl-multi-socket-all)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-timeout . args)
  (define who 'curl-multi-timeout)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-setopt . args)
  (define who 'curl-multi-setopt)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-assign . args)
  (define who 'curl-multi-assign)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))


;;;; done

#;(set-rtd-printer! (type-descriptor XML_ParsingStatus) %struct-XML_ParsingStatus-printer)

#;(post-gc-hooks (cons %free-allocated-parser (post-gc-hooks)))

)

;;; end of file
