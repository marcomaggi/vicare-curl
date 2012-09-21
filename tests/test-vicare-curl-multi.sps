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
#;(struct-guardian-logger #t)


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


(parametrise ((check-test-name	'add-remove))

  (check
      (let* ((multi (curl-multi-init))
	     (easy  (curl-easy-init)))
	(let* ((rv1 (curl-multi-add-handle multi easy))
	       (rv2 (curl-multi-remove-handle multi easy))
	       (rv3 (curl-multi-cleanup multi)))
	  (list rv1 rv2 rv3)))
    => `(,CURLM_OK ,CURLM_OK ,CURLM_OK))

  (check
      (let* ((multi (curl-multi-init))
	     (easy1 (curl-easy-init))
	     (easy2 (curl-easy-init))
	     (easy3 (curl-easy-init)))
	(let* ((rv1 (curl-multi-add-handle multi easy1))
	       (rv2 (curl-multi-add-handle multi easy2))
	       (rv3 (curl-multi-add-handle multi easy3))
	       (rv4 (curl-multi-remove-handle multi easy1))
	       (rv5 (curl-multi-remove-handle multi easy2))
	       (rv6 (curl-multi-remove-handle multi easy3))
	       (rv7 (curl-multi-cleanup multi)))
	  (curl-easy-cleanup easy1)
	  (curl-easy-cleanup easy2)
	  (curl-easy-cleanup easy3)
	  (list rv1 rv2 rv3 rv4 rv5 rv6 rv7)))
    => `(,CURLM_OK ,CURLM_OK ,CURLM_OK  ,CURLM_OK ,CURLM_OK ,CURLM_OK  ,CURLM_OK))

;;; --------------------------------------------------------------------

  (check
      (let* ((multi (curl-multi-init))
	     (easy  (curl-easy-init)))
	(curl-multi-add-handle multi easy))
    => CURLM_OK)

  (check
      (let* ((multi (curl-multi-init))
	     (easy1 (curl-easy-init))
	     (easy2 (curl-easy-init))
	     (easy3 (curl-easy-init)))
	(curl-multi-add-handle multi easy1)
	(curl-multi-add-handle multi easy2)
	(curl-multi-add-handle multi easy3))
    => CURLM_OK)

  (check
      (let* ((multi (curl-multi-init))
	     (easy1 (curl-easy-init))
	     (easy2 (curl-easy-init))
	     (easy3 (curl-easy-init)))
	(curl-multi-add-handle multi easy1)
	(curl-multi-add-handle multi easy2)
	(curl-multi-add-handle multi easy3)
	(vector-length (curl-multi-easies multi)))
    => 3)

;;; --------------------------------------------------------------------

  (collect))


(parametrise ((check-test-name	'setopt))

  (check
      (let ((multi (curl-multi-init))
	    (cb    (make-curl-socket-callback
		    (lambda (handle sock what callback-custom-data socket-custom-data)
		      0))))
	(let* ((rv1 (curl-multi-setopt multi CURLMOPT_SOCKETFUNCTION cb))
	       (rv2 (curl-multi-setopt multi CURLMOPT_SOCKETDATA #f)))
	  (list rv1 rv2)))
    => `(,CURLM_OK ,CURLM_OK))

;;; --------------------------------------------------------------------

  (check
      (let ((multi (curl-multi-init)))
	(curl-multi-setopt multi CURLMOPT_PIPELINING #t))
    => CURLM_OK)

  (check
      (let ((multi (curl-multi-init)))
	(curl-multi-setopt multi CURLMOPT_PIPELINING #f))
    => CURLM_OK)

;;; --------------------------------------------------------------------

  (check
      (let ((multi (curl-multi-init))
	    (cb    (make-curl-multi-timer-callback
		    (lambda (multi timeout-ms custom-data)
		      0))))
	(let* ((rv1 (curl-multi-setopt multi CURLMOPT_TIMERFUNCTION cb))
	       (rv2 (curl-multi-setopt multi CURLMOPT_TIMERDATA #f)))
	  (list rv1 rv2)))
    => `(,CURLM_OK ,CURLM_OK))

;;; --------------------------------------------------------------------

  (check
      (let ((multi (curl-multi-init)))
	(curl-multi-setopt multi CURLMOPT_MAXCONNECTS 123))
    => CURLM_OK)

  (collect))


(parametrise ((check-test-name	'sockets))

  (check
      (let ((multi (curl-multi-init)))
	(let-values (((code milliseconds)
		      (curl-multi-timeout multi)))
	  (cons code milliseconds)))
    => `(,CURLM_OK . -1))

;;; --------------------------------------------------------------------

  (check
      (let ((multi (curl-multi-init)))
	(curl-multi-assign multi 0 #f))
    => CURLM_BAD_SOCKET)

;;; --------------------------------------------------------------------

  (check
      (let ((multi (curl-multi-init)))
	(let-values (((code fd-max)
		      (curl-multi-fdset multi #f #f #f)))
	  (cons code fd-max)))
    => `(,CURLM_OK . -1))

  #t)


(parametrise ((check-test-name	'misc))

  (check
      (let ((multi (curl-multi-init)))
	(let-values (((next num)
		      (curl-multi-info-read multi)))
	  (cons next num)))
    => '(#f . 0))

;;; --------------------------------------------------------------------

  (check
      (curl-multi-strerror CURLM_OK)
    => "No error")

  #t)


;;;; done

(collect)
(check-report)

;;; end of file
