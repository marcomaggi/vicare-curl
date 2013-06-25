;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/cURL
;;;Contents: demos for documentation
;;;Date: Wed Sep  5, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (prefix (vicare ffi) ffi.)
  (vicare language-extensions syntaxes))


;;;; helpers

(define-inline (%pretty-print ?thing)
  (pretty-print ?thing (current-error-port)))


;;;; version functions

(when #f
  (let ()

    (%pretty-print (list (vicare-curl-version-interface-current)
			 (vicare-curl-version-interface-revision)
			 (vicare-curl-version-interface-age)
			 (vicare-curl-version)))

    #f))


;;;; getting informations

(when #f
  (let ()

    (define (dummy-write-cb buffer size nitems outstream)
      (* size nitems))

    (let ((easy		(curl-easy-init))
	  (write-cb	(make-curl-write-callback dummy-write-cb)))
      (unwind-protect
	  (begin
	    (curl-easy-setopt easy CURLOPT_URL "http://www.google.com/")
	    ;;In a way or the other  a WRITEFUNCTION is always there; to
	    ;;discard data we have to register a WRITEFUNCTION that does
	    ;;nothing!!!
	    (curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
	    (curl-easy-setopt easy CURLOPT_WRITEDATA #f)
	    (assert (= CURLE_OK (curl-easy-perform easy)))
	    (let-values
		(((code info)
		  (curl-easy-getinfo easy CURLINFO_EFFECTIVE_URL)))
	      (assert (= code CURLE_OK))
	      (printf "Effective URL: ~a\n" info))
	    (let-values
		(((code info)
		  (curl-easy-getinfo easy CURLINFO_RESPONSE_CODE)))
	      (assert (= code CURLE_OK))
	      (printf "Response code: ~a\n" info))
	    (let-values
		(((code info)
		  (curl-easy-getinfo easy CURLINFO_TOTAL_TIME)))
	      (assert (= code CURLE_OK))
	      (printf "Total time: ~a\n" info))

	    (flush-output-port (current-output-port)))
	;;Close the connection before releasing the callbacks!!!
	(curl-easy-cleanup easy)
	(ffi.free-c-callback write-cb)))
    ))


;;;; getting certificate informations

(when #t
  (let ()

    (define (dummy-write-cb buffer size nitems outstream)
      (* size nitems))

    (let ((easy		(curl-easy-init))
	  (write-cb	(make-curl-write-callback dummy-write-cb)))
      (unwind-protect
	  (begin
	    (curl-easy-setopt easy CURLOPT_URL "https://github.com/")
	    ;;In a way or the other  a WRITEFUNCTION is always there; to
	    ;;discard data we have to register a WRITEFUNCTION that does
	    ;;nothing!!!
	    (curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
	    (curl-easy-setopt easy CURLOPT_WRITEDATA #f)
	    (curl-easy-setopt easy CURLOPT_CERTINFO #t)
	    (assert (= CURLE_OK (curl-easy-perform easy)))
	    (let-values
		(((code info)
		  (curl-easy-getinfo easy CURLINFO_CERTINFO)))
	      (assert (= code CURLE_OK))
	      (printf "Number of certificates: ~a\n"
		      (vector-length info))
	      (printf "First certificate data: \n")
	      (pretty-print (vector-ref info 0)))

	    (flush-output-port (current-output-port)))
	;;Close the connection before releasing the callbacks!!!
	(curl-easy-cleanup easy)
	(ffi.free-c-callback write-cb)))
    ))


;;;; done


;;; end of file
