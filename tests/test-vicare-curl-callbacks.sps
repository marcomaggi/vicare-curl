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
;;;Copyright (C) 2012, 2013, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(program (test-vicare-curl-callbacks)
  (options typed-language)
  (import (vicare)
    (vicare net curl)
    (vicare net curl constants)
    (vicare net curl features)
    (vicare platform constants)
    (prefix (vicare ffi) ffi::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare Libcurl bindings, callback makers\n")

(assert (= CURLE_OK (curl-global-init CURL_GLOBAL_ALL)))


(parametrise ((check-test-name	'easy-makers))

  (check	;make-curl-write-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'size_t
					   '(pointer size_t size_t pointer)))
	     (cb (make-curl-write-callback
		  (lambda (buffer size nitems outstream)
		    (add-result (list buffer size nitems outstream))
		    123))))
	 (unwind-protect
	     ((co cb) (null-pointer) 1 2 (null-pointer))
	   (ffi::free-c-callback cb))))
    => `(123 ((,(null-pointer) 1 2 #f))))

;;; --------------------------------------------------------------------

  (check	;make-curl-read-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'size_t
					   '(pointer size_t size_t pointer)))
	     (cb (make-curl-read-callback
		  (lambda (buffer size nitems outstream)
		    (add-result (list buffer size nitems outstream))
		    123))))
	 (unwind-protect
	     ((co cb) (null-pointer) 1 2 (null-pointer))
	   (ffi::free-c-callback cb))))
    => `(123 ((,(null-pointer) 1 2 #f))))

;;; --------------------------------------------------------------------

  (check	;make-curl-ioctl-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer signed-int pointer)))
	     (cb (make-curl-ioctl-callback
		  (lambda (easy cmd custom-data)
		    (add-result (list (curl-easy? easy) cmd custom-data))
		    CURLIOE_OK))))
	 (unwind-protect
	     ((co cb) (null-pointer) CURLIOCMD_NOP (null-pointer))
	   (ffi::free-c-callback cb))))
    => `(,CURLIOE_OK ((#t ,CURLIOCMD_NOP #f))))

;;; --------------------------------------------------------------------

  (check	;make-curl-seek-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer off_t signed-int)))
	     (cb (make-curl-seek-callback
		  (lambda (custom-data offset origin)
		    (add-result (list custom-data offset origin))
		    CURL_SEEKFUNC_OK))))
	 (unwind-protect
	     ((co cb) (null-pointer) 123 SEEK_CUR)
	   (ffi::free-c-callback cb))))
    => `(,CURL_SEEKFUNC_OK ((#f 123 ,SEEK_CUR))))

;;; --------------------------------------------------------------------

  (check	;make-curl-socket-option-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer signed-int signed-int)))
	     (cb (make-curl-socket-option-callback
		  (lambda (custom-data curlfd purpose)
		    (add-result (list custom-data curlfd purpose))
		    CURL_SOCKOPT_OK))))
	 (unwind-protect
	     ((co cb) (null-pointer) 123 CURLSOCKTYPE_IPCXN)
	   (ffi::free-c-callback cb))))
    => `(,CURL_SOCKOPT_OK ((#f 123 ,CURLSOCKTYPE_IPCXN))))

;;; --------------------------------------------------------------------

  (check	;make-curl-open-socket-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer signed-int pointer)))
	     (cb (make-curl-open-socket-callback
		  (lambda (custom-data purpose address)
		    (add-result (list custom-data purpose address))
		    123))))
	 (unwind-protect
	     ((co cb) (null-pointer) CURLSOCKTYPE_IPCXN (null-pointer))
	   (ffi::free-c-callback cb))))
    => `(123 ((#f ,CURLSOCKTYPE_IPCXN ,(null-pointer)))))

;;; --------------------------------------------------------------------

  (check	;make-curl-close-socket-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer signed-int)))
	     (cb (make-curl-close-socket-callback
		  (lambda (custom-data sockfd)
		    (add-result (list custom-data sockfd))
		    0))))
	 (unwind-protect
	     ((co cb) (null-pointer) 123)
	   (ffi::free-c-callback cb))))
    => `(0 ((#f 123))))

;;; --------------------------------------------------------------------

  (check	;make-curl-progress-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer double double double double)))
	     (cb (make-curl-progress-callback
		  (lambda (custom-data dltotal dlnow ultotal ulnow)
		    (add-result (list custom-data dltotal dlnow ultotal ulnow))
		    #t))))
	 (unwind-protect
	     ((co cb) (null-pointer) 1.0 2.0 3.0 4.0)
	   (ffi::free-c-callback cb))))
    => '(1 ((#f 1.0 2.0 3.0 4.0))))

;;; --------------------------------------------------------------------

  (check	;make-curl-header-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'size_t
					   '(pointer size_t size_t pointer)))
	     (cb (make-curl-header-callback
		  (lambda (buffer size nmemb custom-data)
		    (add-result (list buffer size nmemb custom-data))
		    0))))
	 (unwind-protect
	     ((co cb) (null-pointer) 1 2 (null-pointer))
	   (ffi::free-c-callback cb))))
    => `(0 ((,(null-pointer) 1 2 #f))))

;;; --------------------------------------------------------------------

  (check	;make-curl-debug-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer signed-int pointer size_t pointer)))
	     (cb (make-curl-debug-callback
		  (lambda (easy type data size custom-data)
		    (add-result (list (curl-easy? easy) type data size custom-data))
		    0))))
	 (unwind-protect
	     ((co cb) (null-pointer) CURLINFO_TEXT (null-pointer) 123 (null-pointer))
	   (ffi::free-c-callback cb))))
    => `(0 ((#t ,CURLINFO_TEXT ,(null-pointer) 123 #f))))

;;; --------------------------------------------------------------------

  (check	;make-curl-ssl-ctx-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer pointer pointer)))
	     (cb (make-curl-ssl-ctx-callback
		  (lambda (easy ssl-ctx custom-data)
		    (add-result (list (curl-easy? easy) ssl-ctx custom-data))
		    CURLE_OK))))
	 (unwind-protect
	     ((co cb) (null-pointer) (null-pointer) (null-pointer))
	   (ffi::free-c-callback cb))))
    => `(,CURLE_OK ((#t ,(null-pointer) #f))))

;;; --------------------------------------------------------------------

  (check	;make-curl-conv-to-network-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer size_t)))
	     (cb (make-curl-conv-to-network-callback
		  (lambda (buffer nbytes)
		    (add-result (list buffer nbytes))
		    CURLE_OK))))
	 (unwind-protect
	     ((co cb) (null-pointer) 123)
	   (ffi::free-c-callback cb))))
    => `(,CURLE_OK ((,(null-pointer) 123))))

;;; --------------------------------------------------------------------

  (check	;make-curl-conv-from-network-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer size_t)))
	     (cb (make-curl-conv-from-network-callback
		  (lambda (buffer nbytes)
		    (add-result (list buffer nbytes))
		    CURLE_OK))))
	 (unwind-protect
	     ((co cb) (null-pointer) 123)
	   (ffi::free-c-callback cb))))
    => `(,CURLE_OK ((,(null-pointer) 123))))

;;; --------------------------------------------------------------------

  (check	;make-curl-conv-from-utf8-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer size_t)))
	     (cb (make-curl-conv-from-utf8-callback
		  (lambda (buffer nbytes)
		    (add-result (list buffer nbytes))
		    CURLE_OK))))
	 (unwind-protect
	     ((co cb) (null-pointer) 123)
	   (ffi::free-c-callback cb))))
    => `(,CURLE_OK ((,(null-pointer) 123))))

;;; --------------------------------------------------------------------

  (check		;make-curl-interleave-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'size_t
					   '(pointer size_t size_t pointer)))
	     (cb (make-curl-interleave-callback
		  (lambda (buffer size nmemb custom-data)
		    (add-result (list buffer size nmemb custom-data))
		    0))))
	 (unwind-protect
	     ((co cb) (null-pointer) 123 456 (null-pointer))
	   (ffi::free-c-callback cb))))
    => `(0 ((,(null-pointer) 123 456 #f))))

;;; --------------------------------------------------------------------

  (check	;make-curl-chunk-begin-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-long
					   '(pointer pointer signed-int)))
	     (cb (make-curl-chunk-begin-callback
		  (lambda (info custom-data remains)
		    (add-result (list info custom-data remains))
		    CURL_CHUNK_BGN_FUNC_OK))))
	 (unwind-protect
	     ((co cb) (null-pointer) (null-pointer) 123)
	   (ffi::free-c-callback cb))))
    => `(,CURL_CHUNK_BGN_FUNC_OK ((,(null-pointer) #f 123))))

;;; --------------------------------------------------------------------

  (check	;make-curl-chunk-end-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-long
					   '(pointer)))
	     (cb (make-curl-chunk-end-callback
		  (lambda (custom-data)
		    (add-result custom-data)
		    CURL_CHUNK_END_FUNC_OK))))
	 (unwind-protect
	     ((co cb) (null-pointer))
	   (ffi::free-c-callback cb))))
    => `(,CURL_CHUNK_END_FUNC_OK (#f)))

;;; --------------------------------------------------------------------

  (check	;make-curl-fnmatch-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer pointer pointer)))
	     (cb (make-curl-fnmatch-callback
		  (lambda (custom-data pattern text)
		    (add-result (list custom-data pattern text))
		    CURL_FNMATCHFUNC_MATCH))))
	 (unwind-protect
	     ((co cb) (null-pointer) (null-pointer) (null-pointer))
	   (ffi::free-c-callback cb))))
    => `(,CURL_FNMATCHFUNC_MATCH ((#f ,(null-pointer) ,(null-pointer)))))

;;; --------------------------------------------------------------------

  (check	;make-curl-sshkey-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer pointer pointer signed-int pointer)))
	     (cb (make-curl-sshkey-callback
		  (lambda (easy knownkey foundkey khmatch custom-data)
		    (add-result (list (curl-easy? easy) knownkey foundkey
				      khmatch custom-data))
		    CURLKHSTAT_FINE))))
	 (unwind-protect
	     ((co cb) (null-pointer) (null-pointer) (null-pointer) CURLKHMATCH_MISMATCH
	      (null-pointer))
	   (ffi::free-c-callback cb))))
    => `(,CURLKHSTAT_FINE ((#t #f ,(null-pointer) ,CURLKHMATCH_MISMATCH #f))))

  (collect))


(parametrise ((check-test-name	'multi-makers))

  (check	;make-curl-socket-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer signed-int signed-int pointer pointer)))
	     (cb (make-curl-socket-callback
		  (lambda (easy sock what callback-custom-data socket-custom-data)
		    (add-result (list (curl-easy? easy) sock what
				      callback-custom-data socket-custom-data))))))
	 (unwind-protect
	     ((co cb) (null-pointer) 123 CURL_POLL_IN (null-pointer) (null-pointer))
	   (ffi::free-c-callback cb))))
    => `(0 ((#t 123 ,CURL_POLL_IN #f #f))))

;;; --------------------------------------------------------------------

  (check	;make-curl-multi-timer-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer signed-long pointer)))
	     (cb (make-curl-multi-timer-callback
		  (lambda (multi milliseconds custom-data)
		    (add-result (list (curl-multi? multi) milliseconds custom-data))
		    123))))
	 (unwind-protect
	     ((co cb) (null-pointer) 456 (null-pointer))
	   (ffi::free-c-callback cb))))
    => `(123 ((#t 456 #f))))

  #t)


(parametrise ((check-test-name	'xferinfo))

  (check	;make-curl-xferinfo-callback
      (with-result
       (let ((co (ffi::make-c-callout-maker 'signed-int
					   '(pointer off_t off_t off_t off_t)))
	     (cb (make-curl-xferinfo-callback
		  (lambda (custom-data dltotal dlnow ultotal ulnow)
		    (add-result (list custom-data dltotal dlnow ultotal ulnow))
		    0))))
	 (unwind-protect
	     ((co cb) (null-pointer) 1 2 3 4)
	   (ffi::free-c-callback cb))))
    => `(0 ((#f 1 2 3 4))))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
