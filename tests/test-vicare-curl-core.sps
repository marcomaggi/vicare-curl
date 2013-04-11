;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/cURL
;;;Contents: tests for Libcurl bindings, core functions
;;;Date: Thu Sep  6, 2012
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
  (vicare net curl features)
  (vicare syntactic-extensions)
  (prefix (vicare ffi) ffi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare Libcurl bindings, core functions\n")

(assert (= CURLE_OK (curl-global-init CURL_GLOBAL_ALL)))


;;;; helpers



(parametrise ((check-test-name	'misc))

  (check
      (curl-free (null-pointer))
    => (void))

  (check
      (curl-free #f)
    => (void))

;;; --------------------------------------------------------------------

  (check
      (curl-getdate "Sun Nov 6 94")
    => 784080000)

  #t)


(parametrise ((check-test-name	'slists))

  (check
      (let ((slist (curl-slist-append #f "ciao")))
	(curl-slist-free-all slist))
    => (void))

  (check
      (let ((slist (curl-slist-append (null-pointer) "ciao")))
	(curl-slist-free-all slist))
    => (void))

  (check
      (let ((slist (curl-slist-append "ciao")))
	(curl-slist-free-all slist))
    => (void))

  (check
      (let* ((slist (curl-slist-append "ciao"))
	     (slist (curl-slist-append slist "hello"))
	     (slist (curl-slist-append slist "salut")))
	(curl-slist-free-all slist))
    => (void))

  (check
      (let* ((slist (curl-slist-append "ciao"))
	     (slist (curl-slist-append slist "hello"))
	     (slist (curl-slist-append slist "salut")))
	(curl-slist-free-all slist)
;;;	(check-pretty-print slist)
	slist)
    => (null-pointer))

;;; --------------------------------------------------------------------

  (check
      (let ((slist (curl-slist-append "ciao")))
	(curl-slist->list slist))
    => '("ciao"))

  (check
      (let* ((slist (curl-slist-append "ciao"))
	     (slist (curl-slist-append slist "hello"))
	     (slist (curl-slist-append slist "salut")))
	(curl-slist->list slist))
    => '("ciao" "hello" "salut"))

  (check
      (curl-slist->list (list->curl-slist '("ciao" "hello" "salut")))
    => '("ciao" "hello" "salut"))

  (collect))


(parametrise ((check-test-name				'formdata)
	      (struct-guardian-logger			#f))

  (check
      (curl-formfree (make-curl-form-data))
    => (void))

;;; --------------------------------------------------------------------

  (check
      (let* ((data	"")
	     (cb	(make-curl-formget-callback
			 (lambda (custom-data cstring.ptr cstring.len)
			   (set! data (string-append data
						     (cstring->string cstring.ptr
								      cstring.len)))
			   cstring.len)))
	     (post	(make-curl-form-data))
	     (last	(null-pointer))
	     (rv	(curl-formadd post last
				      CURLFORM_COPYNAME "name"
				      CURLFORM_COPYCONTENTS "contents"
				      CURLFORM_END)))
	(unwind-protect
	    (if (= rv CURL_FORMADD_OK)
		(curl-formget post #f cb)
	      rv)
;;;(check-pretty-print data)
	  (ffi.free-c-callback cb)
	  (curl-formfree post)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((data	"")
	     (cb	(make-curl-formget-callback
			 (lambda (custom-data cstring.ptr cstring.len)
			   (set! data (string-append data
						     (cstring->string cstring.ptr
								      cstring.len)))
			   cstring.len)))
	     (post	(make-curl-form-data))
	     (last	(null-pointer))
	     (rv	(curl-formadd post last
				      CURLFORM_NAMELENGTH (string-length "name")
				      CURLFORM_COPYNAME "name"
				      CURLFORM_CONTENTSLENGTH (string-length "contents")
				      CURLFORM_COPYCONTENTS "contents"
				      CURLFORM_END)))
	(unwind-protect
	    (if (= rv CURL_FORMADD_OK)
		(curl-formget post #f cb)
	      rv)
;;;(check-pretty-print data)
	  (ffi.free-c-callback cb)
	  (curl-formfree post)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((data	"")
	     (cb	(make-curl-formget-callback
			 (lambda (custom-data cstring.ptr cstring.len)
			   (set! data (string-append data
						     (cstring->string cstring.ptr
								      cstring.len)))
			   cstring.len)))
	     (post	(make-curl-form-data))
	     (last	(null-pointer))
	     (rv	(curl-formadd post last
				      CURLFORM_COPYNAME "name"
				      CURLFORM_COPYCONTENTS "<html></html>"
				      CURLFORM_CONTENTTYPE "text/html"
				      CURLFORM_END)))
	(unwind-protect
	    (if (= rv CURL_FORMADD_OK)
		(curl-formget post #f cb)
	      rv)
;;;(check-pretty-print data)
	  (ffi.free-c-callback cb)
	  (curl-formfree post)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((data	"")
	     (cb	(make-curl-formget-callback
			 (lambda (custom-data cstring.ptr cstring.len)
			   (set! data (string-append data
						     (cstring->string cstring.ptr
								      cstring.len)))
			   cstring.len)))
	     (post	(make-curl-form-data))
	     (last	(null-pointer))
	     (rv	(curl-formadd post last
				      CURLFORM_COPYNAME "html_code_with_hole"
				      CURLFORM_COPYCONTENTS "<html></html>"
				      CURLFORM_CONTENTSLENGTH (string-length "<html></html>")
				      CURLFORM_CONTENTTYPE "text/html"
				      CURLFORM_END)))
;;;(check-pretty-print post)
	(unwind-protect
	    (if (= rv CURL_FORMADD_OK)
		(curl-formget post #f cb)
	      rv)
;;;(check-pretty-print data)
	  (ffi.free-c-callback cb)
	  #;(curl-formfree post)))
    => #f)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
       (let ((httppost (make-curl-form-data)))
	 (set-curl-form-data-custom-destructor! httppost (lambda (httppost)
							   (add-result 123)))
	 (curl-formfree httppost)))
    => `(,(void) (123)))

  (collect))


(parametrise ((check-test-name	'escaping))

  (check
      (curl-escape/string "http://www.marco.it/")
    => "http%3A%2F%2Fwww.marco.it%2F")

  (check
      (curl-escape/string "ciao")
    => "ciao")

;;; --------------------------------------------------------------------

  (check	;dots encoded
      (curl-unescape/string "http%3A%2F%2Fwww%2Emarco%2Eit%2F")
    => "http://www.marco.it/")

  (check	;dots not encoded
      (curl-unescape/string "http%3A%2F%2Fwww.marco.it%2F")
    => "http://www.marco.it/")

  (check
      (curl-unescape/string "ciao")
    => "ciao")

  #t)


(parametrise ((check-test-name				'shares)
	      (struct-guardian-logger			#f))

  (check
      (let ((share (curl-share-init)))
	(curl-share-cleanup share))
    => CURLSHE_OK)

  (check
      (let ((share (curl-share-init)))
	(curl-share-cleanup share)
	(curl-share-cleanup share))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((share (curl-share-init)))
	(curl-share-setopt share CURLSHOPT_SHARE CURL_LOCK_DATA_COOKIE))
    => CURLSHE_OK)

  (check
      (let ((share (curl-share-init)))
	(curl-share-setopt share CURLSHOPT_UNSHARE CURL_LOCK_DATA_DNS))
    => CURLSHE_OK)

;;; --------------------------------------------------------------------

  (check
      (let ((share (curl-share-init)))
	(curl-share-setopt share CURLSHOPT_USERDATA (null-pointer)))
    => CURLSHE_OK)

  (check
      (let ((share (curl-share-init)))
	(curl-share-setopt share CURLSHOPT_USERDATA #f))
    => CURLSHE_OK)

;;; --------------------------------------------------------------------

  (check
      (let ((share (curl-share-init)))
	(curl-share-setopt share CURLSHOPT_LOCKFUNC #f))
    => CURLSHE_OK)

  (check
      (let ((share	(curl-share-init))
	    (cb		(make-curl-lock-function
			 (lambda (easy what-to-lock how-to-lock custom-data)
			   (void)))))
	(unwind-protect
	    (curl-share-setopt share CURLSHOPT_LOCKFUNC cb)
	  (curl-share-cleanup share)
	  (ffi.free-c-callback cb)))
    => CURLSHE_OK)

  (check
      (let ((share (curl-share-init)))
	(curl-share-setopt share CURLSHOPT_UNLOCKFUNC #f))
    => CURLSHE_OK)

  (check
      (let ((share (curl-share-init)))
	(curl-share-setopt share CURLSHOPT_UNLOCKFUNC
			   (make-curl-lock-function
			    (lambda (easy what-to-lock custom-data)
			      (void)))))
    => CURLSHE_OK)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
       (let ((share (curl-share-init)))
	 (set-curl-share-custom-destructor! share (lambda (share)
						    (add-result 123)))
	 (curl-share-cleanup share)))
    => `(,CURLSHE_OK (123)))
  (collect))


;;;; done

(check-report)

;;; end of file
