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

(define (debug-log obj)

(current-error-port))


(parametrise ((check-test-name			'init)
	      (curl-easy-garbage-collection-log	#t))

  (check
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

  (collect))


;;;; done

(check-report)

;;; end of file
