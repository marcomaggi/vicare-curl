;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/cURL
;;;Contents: tests for the callback makers
;;;Date: Wed Sep 12, 2012
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
(import (vicare)
  (vicare net curl)
  (vicare net curl constants)
  (vicare net curl features)
  (vicare syntactic-extensions)
  (prefix (vicare ffi) ffi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare Libcurl bindings, callback makers\n")

(assert (= CURLE_OK (curl-global-init CURL_GLOBAL_ALL)))


(parametrise ((check-test-name	'makers))

  (check	;make-curl-progress-callback
      (with-result
       (let ((co (ffi.make-c-callout-maker 'signed-int
					   '(pointer double double double double)))
	     (cb (make-curl-progress-callback
		  (lambda (custom-data dltotal dlnow ultotal ulnow)
		    (add-result (list custom-data dltotal dlnow ultotal ulnow))
		    #t))))
	 (unwind-protect
	     ((co cb) (null-pointer) 1.0 2.0 3.0 4.0)
	   (ffi.free-c-callback cb))))
    => '(1 ((#f 1.0 2.0 3.0 4.0))))

;;; --------------------------------------------------------------------

  (check	;make-curl-write-callback
      (with-result
       (let ((co (ffi.make-c-callout-maker 'size_t
					   '(pointer size_t size_t pointer)))
	     (cb (make-curl-write-callback
		  (lambda (buffer size nitems outstream)
		    (add-result (list buffer size nitems outstream))
		    123))))
	 (unwind-protect
	     ((co cb) (null-pointer) 1 2 (null-pointer))
	   (ffi.free-c-callback cb))))
    => `(123 ((,(null-pointer) 1 2 #f))))

;;; --------------------------------------------------------------------

  (check	;make-curl-read-callback
      (with-result
       (let ((co (ffi.make-c-callout-maker 'size_t
					   '(pointer size_t size_t pointer)))
	     (cb (make-curl-read-callback
		  (lambda (buffer size nitems outstream)
		    (add-result (list buffer size nitems outstream))
		    123))))
	 (unwind-protect
	     ((co cb) (null-pointer) 1 2 (null-pointer))
	   (ffi.free-c-callback cb))))
    => `(123 ((,(null-pointer) 1 2 #f))))

  #t)


;;;; done

(check-report)

;;; end of file
