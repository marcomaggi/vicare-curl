;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/cURL
;;;Contents: tests for Libcurl bindings, multi API
;;;Date: Fri Sep 21, 2012
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
(check-display "*** testing Vicare Libcurl bindings, multi API\n")

(assert (= CURLE_OK (curl-global-init CURL_GLOBAL_ALL)))


;;;; helpers



(parametrise ((check-test-name			'init)
	      (struct-guardian-logger		#f))

  (check	;this will be garbage collected
      (let ((multi (curl-multi-init)))
;;;	(pretty-print multi (current-error-port))
	(curl-multi? multi))
    => #t)

  (check
      (curl-multi?/alive (curl-multi-init))
    => #t)

  (check
      (let ((multi (curl-multi-init)))
  	(curl-multi-cleanup multi))
    => CURLM_OK)

  (check
      (let ((multi (curl-multi-init)))
  	(curl-multi-cleanup multi)
  	(curl-multi-cleanup multi))
    => CURLM_OK)

  (check
      (let ((multi (curl-multi-init)))
  	(curl-multi-cleanup multi)
  	(curl-multi?/alive multi))
    => #f)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
       (let ((multi (curl-multi-init)))
	 (set-curl-multi-destructor! multi (lambda (multi)
					     (add-result 123)))
	 (curl-multi-cleanup multi)))
    => `(,CURLM_OK (123)))

  (collect))


;;;; done

(collect)
(check-report)

;;; end of file
