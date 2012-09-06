;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/cURL
;;;Contents: tests for Libcurl bindings, version functions
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


#!r6rs
(import (vicare)
  (vicare net curl)
  (vicare net curl constants)
  (vicare net curl features)
;;;  (prefix (vicare ffi) ffi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare Libcurl bindings, version functions\n")


;;;; helpers



(parametrise ((check-test-name	'version))

  (check
      (fixnum? (vicare-curl-version-interface-current))
    => #t)

  (check
      (fixnum? (vicare-curl-version-interface-revision))
    => #t)

  (check
      (fixnum? (vicare-curl-version-interface-age))
    => #t)

  (check
      (string? (vicare-curl-version))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((v (curl-version)))
;;;(check-pretty-print v)
	(string? v))
    => #t)

;;; --------------------------------------------------------------------

  (when #f
    (check-pretty-print (curl-version-info CURLVERSION_FIRST))
    (check-pretty-print (curl-version-info CURLVERSION_SECOND))
    (check-pretty-print (curl-version-info CURLVERSION_THIRD))
    (check-pretty-print (curl-version-info CURLVERSION_FOURTH)))
  (when #f
    (check-pretty-print (curl-version-info CURLVERSION_NOW)))

  (check
      (for-all symbol?
	(curl-version-info-features->symbols (curl-version-info CURLVERSION_NOW)))
    => #t)

  (check
      (curl-version-feature? (curl-version-info CURLVERSION_NOW) CURL_VERSION_LIBZ)
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
