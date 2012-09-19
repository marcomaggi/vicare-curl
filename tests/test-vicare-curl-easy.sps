;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/cURL
;;;Contents: tests for Libcurl bindings, easy API
;;;Date: Sun Sep  9, 2012
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
  (vicare syntactic-extensions)
  (prefix (vicare ffi) ffi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare Libcurl bindings, easy API\n")

(assert (= CURLE_OK (curl-global-init CURL_GLOBAL_ALL)))


;;;; helpers



(parametrise ((check-test-name			'init)
	      (struct-guardian-logger		#f))

  #;(check	;create and collect with logging function
      (with-result
       (collect)
       (parametrise ((struct-guardian-logger
		      (lambda (easy)
			(add-result 1))))
	 (curl-easy-init)
	 (curl-easy-init)
	 (curl-easy-init)
	 (collect)))
    => `(,(void) (1 1 1)))

  (check	;this will be garbage collected
      (let ((easy (curl-easy-init)))
;;;	(pretty-print easy (current-error-port))
	(curl-easy? easy))
    => #t)

  (check
      (curl-easy?/alive (curl-easy-init))
    => #t)

  (check
      (let ((easy (curl-easy-init)))
  	(curl-easy-cleanup easy))
    => (void))

  (check
      (let ((easy (curl-easy-init)))
  	(curl-easy-cleanup easy)
  	(curl-easy-cleanup easy))
    => (void))

  (check
      (let ((easy (curl-easy-init)))
  	(curl-easy-cleanup easy)
  	(curl-easy?/alive easy))
    => #f)

;;; --------------------------------------------------------------------
;;; reset

  (check
      (let ((easy (curl-easy-init)))
  	(curl-easy-reset easy)
  	(curl-easy-cleanup easy))
    => (void))

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
       (let ((easy (curl-easy-init)))
	 (set-curl-easy-destructor! easy (lambda (easy)
					   (add-result 123)))
	 (curl-easy-cleanup easy)))
    => `(,(void) (123)))

;;; --------------------------------------------------------------------

  (check
      (let* ((easy  (curl-easy-init))
	     (clone (curl-easy-duphandle easy)))
  	(curl-easy-cleanup easy)
	(curl-easy-cleanup clone))
    => (void))

  (check
      (let* ((easy (curl-easy-init))
	     (rv1  (curl-easy-pause easy CURLPAUSE_RECV))
	     (rv2  (curl-easy-pause easy CURLPAUSE_RECV)))
	(curl-easy-cleanup easy)
	(list rv1 rv2))
    => (list CURLE_OK CURLE_OK))

  (collect))


(parametrise ((check-test-name	'escaping))

  (check
      (curl-easy-escape/string (curl-easy-init) "http://www.marco.it/")
    => "http%3A%2F%2Fwww.marco.it%2F")

  (check
      (curl-easy-escape/string (curl-easy-init) "ciao")
    => "ciao")

;;; --------------------------------------------------------------------

  (check	;dots encoded
      (curl-easy-unescape/string (curl-easy-init) "http%3A%2F%2Fwww%2Emarco%2Eit%2F")
    => "http://www.marco.it/")

  (check	;dots not encoded
      (curl-easy-unescape/string (curl-easy-init) "http%3A%2F%2Fwww.marco.it%2F")
    => "http://www.marco.it/")

  (check
      (curl-easy-unescape/string (curl-easy-init) "ciao")
    => "ciao")

  (collect))


(parametrise ((check-test-name	'strerror))

  (check
      (curl-easy-strerror CURLE_OK)
    => "No error")

  (check
      (curl-easy-strerror -1)
    => "Unknown error")

  (check
      (curl-easy-strerror CURLE_UNSUPPORTED_PROTOCOL)
    => "Unsupported protocol")

  #f)


;;;; done

(check-report)

;;; end of file
