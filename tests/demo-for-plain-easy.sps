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
;;;Copyright (C) 2012, 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare language-extensions syntaxes)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** demonstrating Vicare Libcurl bindings, easy API\n")

(assert (= CURLE_OK (curl-global-init CURL_GLOBAL_ALL)))


;;;; helpers

(define-inline (%pretty-print ?thing)
  (pretty-print ?thing (current-error-port)))

(define debug-print-data
  (make-parameter #f))

(define (debug-func easy type data size custom-data)
  (define (%print template)
    (fprintf (current-error-port) template (cstring->string data size)))
  (cond ((= type CURLINFO_TEXT)
	 (%print "Text: ~a"))
	((= type CURLINFO_HEADER_IN)
	 (%print "Header-In: ~a"))
	((= type CURLINFO_HEADER_OUT)
	 (%print "Header-Out: ~a"))
	((= type CURLINFO_DATA_IN)
	 (when (debug-print-data)
	   (%print "Data-In:\n~a\n")))
	((= type CURLINFO_DATA_OUT)
	 (when (debug-print-data)
	   (%print "Data-Out:\n~a\n")))
	(else
	 (%print "Boh:\n~a\n")))
  0)


(parametrise ((check-test-name	'perform))

  (define (write-func buffer size nitems outstream)
    (let ((nbytes (* size nitems)))
;;;      (check-pretty-print (list 'enter size nitems nbytes))
      (guard (E (else (check-pretty-print E) nbytes))
	(fprintf (current-error-port) "Google's Home page:\n~a\n"
		 (utf8->string (cstring->bytevector buffer nbytes))))
;;;      (check-pretty-print (list 'leave size nitems nbytes))
      nbytes))

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


(parametrise ((check-test-name	'getinfo))

  (check	;CURLINFO_EFFECTIVE_URL, string return value
      (let ((easy	(curl-easy-init))
	    (write-cb	(make-curl-write-callback
			 (lambda (buffer size nitems outstream)
			   (* size nitems))))
	    (debug-cb	(make-curl-debug-callback debug-func)))
	(unwind-protect
	    (begin
	      (curl-easy-setopt easy CURLOPT_URL "http://www.google.com/")
	      ;;In a way  or the other a WRITEFUNCTION  is always there;
	      ;;to discard data we have to register a WRITEFUNCTION that
	      ;;does nothing!!!
	      (curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
	      (curl-easy-setopt easy CURLOPT_WRITEDATA #f)
	      (when #f
		(curl-easy-setopt easy CURLOPT_VERBOSE #f)
		(curl-easy-setopt easy CURLOPT_DEBUGFUNCTION debug-cb)
		(curl-easy-setopt easy CURLOPT_DEBUGDATA #f))
	      (curl-easy-perform easy)
	      (let-values (((code info)
			    (curl-easy-getinfo easy CURLINFO_EFFECTIVE_URL)))
		(check-pretty-print info)
		(unless (= code CURLE_OK)
		  (check-pretty-print (curl-easy-strerror code)))
		code))
	  ;;Close the connection before releasing the callbacks!!!
	  (curl-easy-cleanup easy)
	  (ffi.free-c-callback write-cb)
	  (ffi.free-c-callback debug-cb)))
    => CURLE_OK)

;;; --------------------------------------------------------------------

  (check	;CURLINFO_RESPONSE_CODE, "long" return value
      (let ((easy	(curl-easy-init))
	    (write-cb	(make-curl-write-callback
			 (lambda (buffer size nitems outstream)
			   (* size nitems))))
	    (debug-cb	(make-curl-debug-callback debug-func)))
	(unwind-protect
	    (begin
	      (curl-easy-setopt easy CURLOPT_URL "http://www.google.com/")
	      ;;In a way  or the other a WRITEFUNCTION  is always there;
	      ;;to discard data we have to register a WRITEFUNCTION that
	      ;;does nothing!!!
	      (curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
	      (curl-easy-setopt easy CURLOPT_WRITEDATA #f)
	      (when #f
		(curl-easy-setopt easy CURLOPT_VERBOSE #f)
		(curl-easy-setopt easy CURLOPT_DEBUGFUNCTION debug-cb)
		(curl-easy-setopt easy CURLOPT_DEBUGDATA #f))
	      (curl-easy-perform easy)
	      (let-values (((code info)
			    (curl-easy-getinfo easy CURLINFO_RESPONSE_CODE)))
		(check-pretty-print info)
		(unless (= code CURLE_OK)
		  (check-pretty-print (curl-easy-strerror code)))
		code))
	  ;;Close the connection before releasing the callbacks!!!
	  (curl-easy-cleanup easy)
	  (ffi.free-c-callback write-cb)
	  (ffi.free-c-callback debug-cb)))
    => CURLE_OK)

;;; --------------------------------------------------------------------

  (check	;CURLINFO_TOTAL_TIME, "double" return value
      (let ((easy	(curl-easy-init))
	    (write-cb	(make-curl-write-callback
			 (lambda (buffer size nitems outstream)
			   (* size nitems))))
	    (debug-cb	(make-curl-debug-callback debug-func)))
	(unwind-protect
	    (begin
	      (curl-easy-setopt easy CURLOPT_URL "http://www.google.com/")
	      ;;In a way  or the other a WRITEFUNCTION  is always there;
	      ;;to discard data we have to register a WRITEFUNCTION that
	      ;;does nothing!!!
	      (curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
	      (curl-easy-setopt easy CURLOPT_WRITEDATA #f)
	      (when #f
		(curl-easy-setopt easy CURLOPT_VERBOSE #f)
		(curl-easy-setopt easy CURLOPT_DEBUGFUNCTION debug-cb)
		(curl-easy-setopt easy CURLOPT_DEBUGDATA #f))
	      (curl-easy-perform easy)
	      (let-values (((code info)
			    (curl-easy-getinfo easy CURLINFO_TOTAL_TIME)))
		(check-pretty-print info)
		(unless (= code CURLE_OK)
		  (check-pretty-print (curl-easy-strerror code)))
		code))
	  ;;Close the connection before releasing the callbacks!!!
	  (curl-easy-cleanup easy)
	  (ffi.free-c-callback write-cb)
	  (ffi.free-c-callback debug-cb)))
    => CURLE_OK)

;;; --------------------------------------------------------------------

  (check	;CURLINFO_SSL_ENGINES, slist return value
      (let ((easy	(curl-easy-init))
	    (write-cb	(make-curl-write-callback
			 (lambda (buffer size nitems outstream)
			   (* size nitems))))
	    (debug-cb	(make-curl-debug-callback debug-func)))
	(unwind-protect
	    (begin
	      (curl-easy-setopt easy CURLOPT_URL "https://github.com/")
	      ;;In a way  or the other a WRITEFUNCTION  is always there;
	      ;;to discard data we have to register a WRITEFUNCTION that
	      ;;does nothing!!!
	      (curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
	      (curl-easy-setopt easy CURLOPT_WRITEDATA #f)
	      (when #f
		(curl-easy-setopt easy CURLOPT_VERBOSE #f)
		(curl-easy-setopt easy CURLOPT_DEBUGFUNCTION debug-cb)
		(curl-easy-setopt easy CURLOPT_DEBUGDATA #f))
	      (curl-easy-perform easy)
	      (let-values (((code info)
			    (curl-easy-getinfo easy CURLINFO_SSL_ENGINES)))
		(check-pretty-print info)
		(unless (= code CURLE_OK)
		  (check-pretty-print (curl-easy-strerror code)))
		code))
	  ;;Close the connection before releasing the callbacks!!!
	  (curl-easy-cleanup easy)
	  (ffi.free-c-callback write-cb)
	  (ffi.free-c-callback debug-cb)))
    => CURLE_OK)

;;; --------------------------------------------------------------------

  (check	;CURLINFO_PRIVATE, "void *" return value
      (let ((easy	(curl-easy-init))
	    (write-cb	(make-curl-write-callback
			 (lambda (buffer size nitems outstream)
			   (* size nitems))))
	    (debug-cb	(make-curl-debug-callback debug-func)))
	(unwind-protect
	    (begin
	      (curl-easy-setopt easy CURLOPT_URL "http://www.google.com/")
	      ;;In a way  or the other a WRITEFUNCTION  is always there;
	      ;;to discard data we have to register a WRITEFUNCTION that
	      ;;does nothing!!!
	      (curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
	      (curl-easy-setopt easy CURLOPT_WRITEDATA #f)
	      (curl-easy-setopt easy CURLOPT_PRIVATE (integer->pointer 123))
	      (when #f
		(curl-easy-setopt easy CURLOPT_VERBOSE #f)
		(curl-easy-setopt easy CURLOPT_DEBUGFUNCTION debug-cb)
		(curl-easy-setopt easy CURLOPT_DEBUGDATA #f))
	      (curl-easy-perform easy)
	      (let-values (((code info)
			    (curl-easy-getinfo easy CURLINFO_PRIVATE)))
		(check-pretty-print (pointer->integer info))
		(unless (= code CURLE_OK)
		  (check-pretty-print (curl-easy-strerror code)))
		code))
	  ;;Close the connection before releasing the callbacks!!!
	  (curl-easy-cleanup easy)
	  (ffi.free-c-callback write-cb)
	  (ffi.free-c-callback debug-cb)))
    => CURLE_OK)
  #t)


(parametrise ((check-test-name	'certinfo))

;;;Certificate  informations are  big, so  this  test is  in a  separate
;;;secion.

  (check	;certinfo
      (let ((easy	(curl-easy-init))
	    (write-cb	(make-curl-write-callback
			 (lambda ( buffer size nitems outstream)
			   (* size nitems))))
	    (debug-cb	(make-curl-debug-callback debug-func)))
	(unwind-protect
	    (begin
	      (curl-easy-setopt easy CURLOPT_URL "https://github.com/")
	      (curl-easy-setopt easy CURLOPT_CERTINFO #t)
	      ;;In a way  or the other a WRITEFUNCTION  is always there;
	      ;;to discard data we have to register a WRITEFUNCTION that
	      ;;does nothing!!!
	      (curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
	      (curl-easy-setopt easy CURLOPT_WRITEDATA #f)
	      (when #f
		(curl-easy-setopt easy CURLOPT_VERBOSE #f)
		(curl-easy-setopt easy CURLOPT_DEBUGFUNCTION debug-cb)
		(curl-easy-setopt easy CURLOPT_DEBUGDATA #f))
	      (curl-easy-perform easy)
	      (let-values (((code info)
			    (curl-easy-getinfo easy CURLINFO_CERTINFO)))
		(check-pretty-print info)
		(unless (= code CURLE_OK)
		  (check-pretty-print (curl-easy-strerror code)))
		code))
	  ;;Close the connection before releasing the callbacks!!!
	  (curl-easy-cleanup easy)
	  (ffi.free-c-callback write-cb)
	  (ffi.free-c-callback debug-cb)))
    => CURLE_OK)

  #t)


(parametrise ((check-test-name	'raw-data))

  (define (write-func buffer size nitems outstream)
    (let ((nbytes (* size nitems)))
;;;      (check-pretty-print (list 'enter size nitems nbytes))
      (guard (E (else (check-pretty-print E) nbytes))
	(fprintf (current-error-port) "Google's Home page:\n~a\n"
		 (utf8->string (cstring->bytevector buffer nbytes))))
;;;      (check-pretty-print (list 'leave size nitems nbytes))
      nbytes))

;;; --------------------------------------------------------------------

  (check	;into bytevector
      (let ((easy	(curl-easy-init)))
	(unwind-protect
	    (begin
	      (curl-easy-setopt easy CURLOPT_URL "http://google.com/")
	      (curl-easy-setopt easy CURLOPT_CONNECT_ONLY #t)
	      (assert (= CURLE_OK (curl-easy-perform easy)))
	      (let-values (((code sent)
			    (curl-easy-send easy "GET index.html HTTP/1.1\r\n\r\n")))
		#;(check-pretty-print (curl-easy-strerror code))
		code)
	      (let ((buffer (make-bytevector 4096)))
		(let loop ()
		  (let-values (((code recv)
				(curl-easy-recv easy buffer)))
		    (if (= code CURLE_AGAIN)
			(loop)
		      (begin
			#;(check-pretty-print (ascii->string (subbytevector-u8 buffer 0 recv)))
			#;(check-pretty-print (curl-easy-strerror code))
			code))))))
	  ;;Close the connection before releasing the callbacks!!!
	  (curl-easy-cleanup easy)))
    => CURLE_OK)

;;; --------------------------------------------------------------------

  (check	;into malloc'ed buffer
      (let ((easy	(curl-easy-init)))
	(unwind-protect
	    (begin
	      (curl-easy-setopt easy CURLOPT_URL "http://google.com/")
	      (curl-easy-setopt easy CURLOPT_CONNECT_ONLY #t)
	      (assert (= CURLE_OK (curl-easy-perform easy)))
	      (let-values (((code sent)
			    (curl-easy-send easy "GET index.html HTTP/1.1\r\n\r\n")))
		#;(check-pretty-print (curl-easy-strerror code))
		code)
	      (let* ((buf.len 4096)
		     (buf.ptr (guarded-malloc buf.len)))
		(let loop ()
		  (let-values (((code recv)
				(curl-easy-recv easy buf.ptr buf.len)))
		    (if (= code CURLE_AGAIN)
			(loop)
		      (begin
			#;(check-pretty-print (cstring->string buf.ptr recv))
			#;(check-pretty-print (curl-easy-strerror code))
			code))))))
	  ;;Close the connection before releasing the callbacks!!!
	  (curl-easy-cleanup easy)))
    => CURLE_OK)

;;; --------------------------------------------------------------------

  (check	;into malloc'ed memory-block
      (let ((easy	(curl-easy-init)))
	(unwind-protect
	    (begin
	      (curl-easy-setopt easy CURLOPT_URL "http://google.com/")
	      (curl-easy-setopt easy CURLOPT_CONNECT_ONLY #t)
	      (assert (= CURLE_OK (curl-easy-perform easy)))
	      (let-values (((code sent)
			    (curl-easy-send easy "GET index.html HTTP/1.1\r\n\r\n")))
		#;(check-pretty-print (curl-easy-strerror code))
		code)
	      (let* ((buf.len 4096)
		     (buf.ptr (guarded-malloc buf.len))
		     (buf     (make-memory-block buf.ptr buf.len)))
		(let loop ()
		  (let-values (((code recv)
				(curl-easy-recv easy buf)))
		    (if (= code CURLE_AGAIN)
			(loop)
		      (begin
			#;(check-pretty-print
			 (cstring->string (memory-block-pointer buf) recv))
			#;(check-pretty-print (curl-easy-strerror code))
			code))))))
	  ;;Close the connection before releasing the callbacks!!!
	  (curl-easy-cleanup easy)))
    => CURLE_OK)

  #t)


;;;; done

(check-report)

;;; end of file
