;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/cURL
;;;Contents: tests for Libcurl bindings, init functions
;;;Date: Thu Sep  6, 2012
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
  #;(vicare language-extensions syntaxes)
  (prefix (vicare ffi) ffi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare Libcurl bindings, init functions\n")


;;;; helpers



(parametrise ((check-test-name	'init))

  (check
      (begin0
	(curl-global-init CURL_GLOBAL_ALL)
	(curl-global-cleanup))
    => CURLE_OK)

  (check
      (let ((malloc	(make-curl-malloc-callback malloc))
	    (free	(make-curl-free-callback free))
	    (realloc	(make-curl-realloc-callback realloc))
	    (strdup	(make-curl-strdup-callback strdup))
	    (calloc	(make-curl-calloc-callback calloc)))
	(unwind-protect
	    (begin0
	      (curl-global-init-mem CURL_GLOBAL_ALL
				    malloc free realloc strdup calloc)
	      (curl-global-cleanup))
	  (ffi.free-c-callback malloc)
	  (ffi.free-c-callback free)
	  (ffi.free-c-callback realloc)
	  (ffi.free-c-callback strdup)
	  (ffi.free-c-callback calloc)))
    => CURLE_OK)

  #t)


;;;; done

(check-report)

;;; end of file
