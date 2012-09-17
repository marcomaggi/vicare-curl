;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/cURL
;;;Contents: demos for easy API
;;;Date: Mon Sep 17, 2012
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


#!r6rs
(import (vicare)
  (vicare net curl)
  (vicare net curl constants)
  (vicare net curl features)
  (prefix (vicare ffi) ffi.)
  (vicare syntactic-extensions)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** demonstrating Vicare Libcurl bindings, easy API\n")

(assert (= CURLE_OK (curl-global-init CURL_GLOBAL_ALL)))


;;;; helpers

(define-inline (%pretty-print ?thing)
  (pretty-print ?thing (current-error-port)))


(parametrise ((check-test-name	'perform))

  (define (write-func buffer size nitems outstream)
    (let ((nbytes (* size nitems)))
;;;      (check-pretty-print (list 'enter size nitems nbytes))
      (guard (E (else (check-pretty-print E) nbytes))
	(fprintf (current-error-port) "Google's Home page:\n~a\n"
		 (utf8->string (cstring->bytevector buffer nbytes))))
;;;      (check-pretty-print (list 'leave size nitems nbytes))
      nbytes))

  (define (debug-func easy type data size custom-data)
    #;(fprintf (current-error-port)
	     (case-integers type
	       ((CURLINFO_TEXT)
		"Text: ~a")
	       ((CURLINFO_HEADER_IN)
		"Header-In: ~a")
	       ((CURLINFO_HEADER_OUT)
		"Header-Out: ~a")
	       ((CURLINFO_DATA_IN)
		"Data-In:\n~a\n")
	       ((CURLINFO_DATA_OUT)
		"Data-Out:\n~a\n")
	       (else
		"Boh:\n~a\n"))
	     (cstring->string data size))
    0)

;;; --------------------------------------------------------------------

  (check	;no redirection
      (let ((easy	(curl-easy-init))
	    (write-cb	(make-curl-write-callback write-func))
	    (debug-cb	(make-curl-debug-callback debug-func)))
	(unwind-protect
	    (begin
	      (curl-easy-setopt easy CURLOPT_URL "http://google.com/")
	      (curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
	      (curl-easy-setopt easy CURLOPT_WRITEDATA #f)
	      (curl-easy-setopt easy CURLOPT_VERBOSE #t)
	      (curl-easy-setopt easy CURLOPT_DEBUGFUNCTION debug-cb)
	      (curl-easy-setopt easy CURLOPT_DEBUGDATA #f)
	      (curl-easy-perform easy))
	  ;;Close the connection before releasing the callbacks!!!
	  (curl-easy-cleanup easy)
	  (ffi.free-c-callback write-cb)
	  (ffi.free-c-callback debug-cb)))
    => CURLE_OK)

  (check	;follow location
      (let ((easy	(curl-easy-init))
	    (write-cb	(make-curl-write-callback write-func))
	    (debug-cb	(make-curl-debug-callback debug-func)))
	(unwind-protect
	    (begin
	      (curl-easy-setopt easy CURLOPT_URL "http://google.com/")
	      (curl-easy-setopt easy CURLOPT_FOLLOWLOCATION 1)
	      (curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
	      (curl-easy-setopt easy CURLOPT_WRITEDATA #f)
	      (curl-easy-setopt easy CURLOPT_VERBOSE #t)
	      (curl-easy-setopt easy CURLOPT_DEBUGFUNCTION debug-cb)
	      (curl-easy-setopt easy CURLOPT_DEBUGDATA #f)
	      (curl-easy-perform easy))
	  ;;Close the connection before releasing the callbacks!!!
	  (curl-easy-cleanup easy)
	  (ffi.free-c-callback write-cb)
	  (ffi.free-c-callback debug-cb)))
    => CURLE_OK)

  (collect))


(parametrise ((check-test-name	'pause))


  (collect))


;;;; done

(check-report)

;;; end of file
