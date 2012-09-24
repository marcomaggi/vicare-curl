;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/cURL
;;;Contents: demos for multi API
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
  (prefix (vicare ffi) ffi.)
  (prefix (vicare posix) px.)
  (vicare syntactic-extensions)
  (vicare platform-constants)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** demonstrating Vicare Libcurl bindings, multi API\n")

(assert (= CURLE_OK (curl-global-init CURL_GLOBAL_ALL)))


;;;; helpers

(define-inline (%pretty-print ?thing)
  (pretty-print ?thing (current-error-port)))

(define debug-print-data
  (make-parameter #f))

(define (debug-func easy type data size custom-data)
  (define (%print template)
    (fprintf (current-error-port) template (cstring->string data size)))
  (case-integers type
    ((CURLINFO_TEXT)
     (%print "Text: ~a"))
    ((CURLINFO_HEADER_IN)
     (%print "Header-In: ~a"))
    ((CURLINFO_HEADER_OUT)
     (%print "Header-Out: ~a"))
    ((CURLINFO_DATA_IN)
     (when (debug-print-data)
       (%print "Data-In:\n~a\n")))
    ((CURLINFO_DATA_OUT)
     (when (debug-print-data)
       (%print "Data-Out:\n~a\n")))
    (else
     (%print "Boh:\n~a\n")))
  0)


(parametrise ((check-test-name	'perform))

  (check	;ugly perform, no redirection
      (let ()
	(define (write-func buffer size nitems outstream)
	  (let ((nbytes (* size nitems)))
;;;      (check-pretty-print (list 'enter size nitems nbytes))
	    (guard (E (else (check-pretty-print E) nbytes))
	      (fprintf (current-error-port) "Google's Home page:\n~a\n"
		       (utf8->string (cstring->bytevector buffer nbytes))))
;;;      (check-pretty-print (list 'leave size nitems nbytes))
	    nbytes))
	(let ((multi	(curl-multi-init))
	      (easy	(curl-easy-init))
	      (write-cb	(make-curl-write-callback write-func))
	      (debug-cb	(make-curl-debug-callback debug-func)))
	  (unwind-protect
	      (begin
		(curl-easy-setopt easy CURLOPT_URL "http://google.com/")
		(curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
		(curl-easy-setopt easy CURLOPT_WRITEDATA #f)
		#;(curl-easy-setopt easy CURLOPT_VERBOSE #t)
		#;(curl-easy-setopt easy CURLOPT_DEBUGFUNCTION debug-cb)
		#;(curl-easy-setopt easy CURLOPT_DEBUGDATA #f)
		(curl-multi-add-handle multi easy)
		(let loop ()
		  (let-values (((code running)
				(curl-multi-perform multi)))
		    (when (or (= code CURLM_CALL_MULTI_PERFORM)
			      (not (zero? running)))
		      (loop))))
		(let-values (((msg nmsgs)
			      (curl-multi-info-read multi)))
		  (when msg
		    (%pretty-print (curl-constant-msg->symbol (curl-msg.msg msg)))))
		#t)
	    (curl-multi-cleanup multi)
	    ;;Close the connection before releasing the callbacks!!!
	    (curl-easy-cleanup easy)
	    (ffi.free-c-callback write-cb)
	    (ffi.free-c-callback debug-cb))))
    => #t)

;;; --------------------------------------------------------------------

  (check	;better perform, no redirection
      (let ()
	(define (write-func buffer size nitems outstream)
	  (let ((nbytes (* size nitems)))
	    (guard (E (else (check-pretty-print E) nbytes))
	      (fprintf (current-error-port) "Google's Home page:\n~a\n"
		       (utf8->string (cstring->bytevector buffer nbytes))))
	    nbytes))
	(define (%curl-multi-perform multi)
	  (let loop ()
	    (let-values (((code still-running)
			  (curl-multi-perform multi)))
	      (if (= code CURLM_CALL_MULTI_PERFORM)
		  (loop)
		(values code still-running)))))
	(let ((multi	(curl-multi-init))
	      (easy	(curl-easy-init))
	      (rfdset	(px.make-fd-set-pointer))
	      (wfdset	(px.make-fd-set-pointer))
	      (write-cb	(make-curl-write-callback write-func)))
	  (unwind-protect
	      (begin
		(curl-easy-setopt easy CURLOPT_URL "http://google.com/")
		(curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
		(curl-easy-setopt easy CURLOPT_WRITEDATA #f)
		(curl-multi-add-handle multi easy)
		(let loop ()
		  (let-values (((code still-running)
				(%curl-multi-perform multi)))
		    (unless (zero? still-running)
		      (let-values (((code milliseconds)
				    (curl-multi-timeout multi)))
			(when (and (= code CURLM_OK)
				   (<= 0 milliseconds))
			  (px.FD_ZERO rfdset)
			  (px.FD_ZERO wfdset)
			  (let-values (((code max-fd)
					(curl-multi-fdset multi rfdset wfdset #f))
				       ((secs nsecs)
					(div-and-mod milliseconds 1000)))
			    (when (= code CURLM_OK)
			      (px.select-from-sets (+ 1 max-fd) rfdset wfdset #f secs nsecs)
			      (loop))))))))
		(let-values (((msg nmsgs)
			      (curl-multi-info-read multi)))
		  (when msg
		    (%pretty-print (curl-constant-msg->symbol (curl-msg.msg msg)))))
		#t)
	    ;;Close handles before releasing the callbacks!!!
	    (curl-multi-cleanup multi)
	    (curl-easy-cleanup easy)
	    (ffi.free-c-callback write-cb)
	    (free rfdset)
	    (free wfdset))))
    => #t)

;;; --------------------------------------------------------------------

  (check	;better perform, timeout callback, no redirection
      (let ()
	(define (write-func buffer size nitems outstream)
	  (let ((nbytes (* size nitems)))
	    (guard (E (else (check-pretty-print E) nbytes))
	      (fprintf (current-error-port) "Google's Home page:\n~a\n"
		       (utf8->string (cstring->bytevector buffer nbytes))))
	    nbytes))
	(define (%curl-multi-perform multi)
	  (let loop ()
	    (let-values (((code still-running)
			  (curl-multi-perform multi)))
	      (if (= code CURLM_CALL_MULTI_PERFORM)
		  (loop)
		(values code still-running)))))
	(let* ((multi		(curl-multi-init))
	       (easy		(curl-easy-init))
	       (milliseconds	-1)
	       (rfdset		(px.make-fd-set-pointer))
	       (wfdset		(px.make-fd-set-pointer))
	       (write-cb	(make-curl-write-callback write-func))
	       (timer-cb	(make-curl-multi-timer-callback
				 (lambda (multi ms custom-data)
				   (set! milliseconds ms)
				   0))))
	  (unwind-protect
	      (begin
		(curl-easy-setopt easy CURLOPT_URL "http://google.com/")
		(curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
		(curl-easy-setopt easy CURLOPT_WRITEDATA #f)
		(curl-multi-setopt multi CURLMOPT_TIMERFUNCTION timer-cb)
		(curl-multi-setopt multi CURLMOPT_TIMERDATA     #f)
		(curl-multi-add-handle multi easy)
		(let loop ()
		  (let-values (((code still-running)
				(%curl-multi-perform multi)))
		    (when (and (not (zero? still-running))
			       (<= 0 milliseconds))
		      (px.FD_ZERO rfdset)
		      (px.FD_ZERO wfdset)
		      (let-values (((code max-fd)
				    (curl-multi-fdset multi rfdset wfdset #f))
				   ((secs nsecs)
				    (div-and-mod milliseconds 1000)))
			(when (= code CURLM_OK)
			  (px.select-from-sets (+ 1 max-fd) rfdset wfdset #f secs nsecs)
			  (loop))))))
		(let-values (((msg nmsgs)
			      (curl-multi-info-read multi)))
		  (when msg
		    (%pretty-print (curl-constant-msg->symbol (curl-msg.msg msg)))))
		#t)
	    ;;Close handles before releasing the callbacks!!!
	    (curl-multi-cleanup multi)
	    (curl-easy-cleanup easy)
	    (ffi.free-c-callback write-cb)
	    (ffi.free-c-callback timer-cb)
	    (free rfdset)
	    (free wfdset))))
    => #t)

  (collect))


(parametrise ((check-test-name	'sockets))

  (define-struct pending-socks
    (rd-requests
		;Null or a list of  socket descriptors for which reading
		;is requested.
     wr-requests
		;Null or a list of  socket descriptors for which writing
		;is requested.
     rw-requests
		;Null or a list of  socket descriptors for which reading
		;or writing is requested.
     ))

  (define (%make-pending-socks)
    (make-pending-socks '() '() '()))

  (define (pending-socks-remove! ps sock-fd)
    (pending-socks-remove-from-rd-requests! ps sock-fd)
    (pending-socks-remove-from-wr-requests! ps sock-fd)
    (pending-socks-remove-from-rw-requests! ps sock-fd))

  (define (pending-socks-remove-from-rd-requests! ps sock-fd)
    (set-pending-socks-rd-requests! ps (remq sock-fd (pending-socks-rd-requests ps))))

  (define (pending-socks-remove-from-wr-requests! ps sock-fd)
    (set-pending-socks-wr-requests! ps (remq sock-fd (pending-socks-wr-requests ps))))

  (define (pending-socks-remove-from-rw-requests! ps sock-fd)
    (set-pending-socks-rw-requests! ps (remq sock-fd (pending-socks-rw-requests ps))))

  (define (pending-socks-rd-request! ps sock-fd)
    (set-pending-socks-rd-requests! ps (cons sock-fd (pending-socks-rd-requests ps))))

  (define (pending-socks-wr-request! ps sock-fd)
    (set-pending-socks-wr-requests! ps (cons sock-fd (pending-socks-wr-requests ps))))

  (define (pending-socks-rw-request! ps sock-fd)
    (set-pending-socks-rw-requests!
     ps (cons sock-fd (pending-socks-rw-requests ps))))

  #;(define (pending-socks-clean-rd-requests! ps)
  (set-pending-socks-rd-requests! ps '()))

  #;(define (pending-socks-clean-wr-requests! ps)
  (set-pending-socks-wr-requests! ps '()))

  #;(define (pending-socks-clean-rw-requests! ps)
  (set-pending-socks-rw-requests! ps '()))

  (define (%curl-multi-socket-action multi sock-fd events)
    (let loop ()
      (let-values (((code still-running)
		    (curl-multi-socket-action multi sock-fd events)))
	(if (= code CURLM_CALL_MULTI_PERFORM)
	    (loop)
	  (values code still-running)))))

  (define (write-func buffer size nitems outstream)
    (let ((nbytes (* size nitems)))
      (guard (E (else (check-pretty-print E) nbytes))
	(fprintf (current-error-port) "Google's Home page:\n~a\n"
		 (utf8->string (cstring->bytevector buffer nbytes))))
      nbytes))

  (define (socket-func easy sock-fd poll-type callback-data sock-fd-data)
    (define ps
      (retrieve-to-avoid-collecting callback-data))
    (case-integers poll-type
      ((CURL_POLL_NONE)
       (void))
      ((CURL_POLL_IN)
;;;	     (check-pretty-print (list 'poll-in sock-fd))
       (pending-socks-rd-request! ps sock-fd))
      ((CURL_POLL_OUT)
;;;	     (check-pretty-print (list 'poll-out sock-fd))
       (pending-socks-wr-request! ps sock-fd))
      ((CURL_POLL_INOUT)
;;;	     (check-pretty-print (list 'poll-inout sock-fd))
       (pending-socks-rw-request! ps sock-fd))
      ((CURL_POLL_REMOVE)
;;;	     (check-pretty-print (list 'poll-remove sock-fd))
       (pending-socks-remove! ps sock-fd))))

  (define (timer-func multi milliseconds timeout-pointer)
    (replace-to-avoid-collecting timeout-pointer milliseconds)
    0)

  (module (%select)
    (define (%select rd-requests wr-requests rw-requests milliseconds)
      ;;Perform a SELECT  call for the requested sockets;  use the given
      ;;milliseconds as timeout.   Return two values: null or  a list of
      ;;socket descriptors ready  for reading; null or a  list of socket
      ;;descriptors ready for writing.
      ;;
      (let ((fdsets (px.make-fd-set-bytevector 3)))
	(%set-requests rd-requests fdsets 0)
	(%set-requests wr-requests fdsets 1)
	(%set-requests rw-requests fdsets 0)
	(%set-requests rw-requests fdsets 1)
	(cond ((let-values (((secs nsecs)
			     (div-and-mod milliseconds 1000)))
		 (px.select-from-sets-array FD_SETSIZE fdsets secs nsecs))
	       => (lambda (fdsets)
		    (values (%filter-ready (append rd-requests rw-requests)
					   fdsets 0)
			    (%filter-ready (append wr-requests rw-requests)
					   fdsets 1))))
	      (else ;expired timeout
	       (values '() '())))))

    (define (%filter-ready requests fdsets idx)
      ;;Filter from REQUESTS  the socket descriptors which  are ready in
      ;;FDSETS at fd_set index IDX; return the list of ready sockets.
      ;;
      (let loop ((ready    '())
		 (requests requests))
	(if (null? requests)
	    ready
	  (let ((sock-fd (car requests)))
	    (if (px.FD_ISSET sock-fd fdsets idx)
		(loop (cons sock-fd ready) (cdr requests))
	      (loop ready (cdr requests)))))))

    (define (%set-requests requests fdsets idx)
      ;;Set the  socket descriptors from  REQUESTS in FDSETS  and fd_set
      ;;index IDX; return unspecified values.
      ;;
      (for-each (lambda (sock-fd)
		  (px.FD_SET sock-fd fdsets idx))
	requests))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (check	;socket action, no redirection
      (let* ((multi			(curl-multi-init))
	     (easy			(curl-easy-init))
	     (write-cb			(make-curl-write-callback write-func))
	     (socket-cb			(make-curl-socket-callback socket-func))
	     (timer-cb			(make-curl-multi-timer-callback timer-func))
	     (timeout-pointer		(register-to-avoid-collecting -1))
	     (pending-socks		(%make-pending-socks))
	     (pending-socks-pointer	(register-to-avoid-collecting pending-socks)))
	(unwind-protect
	    (begin
	      (curl-easy-setopt easy CURLOPT_URL "http://google.com/")
	      (curl-easy-setopt easy CURLOPT_WRITEFUNCTION write-cb)
	      (curl-easy-setopt easy CURLOPT_WRITEDATA #f)
	      (curl-multi-setopt multi CURLMOPT_TIMERFUNCTION timer-cb)
	      (curl-multi-setopt multi CURLMOPT_TIMERDATA timeout-pointer)
	      (curl-multi-setopt multi CURLMOPT_SOCKETFUNCTION socket-cb)
	      (curl-multi-setopt multi CURLMOPT_SOCKETDATA pending-socks-pointer)
	      (curl-multi-add-handle multi easy)
	      (let loop ()
		(let-values (((code still-running)
			      (%curl-multi-socket-action multi CURL_SOCKET_TIMEOUT 0)))
		  (when (and (= code CURLM_OK)
			     (not (zero? still-running)))
		    (let-values
			(((read-ready write-ready)
			  (%select (pending-socks-rd-requests pending-socks)
				   (pending-socks-wr-requests pending-socks)
				   (pending-socks-rw-requests pending-socks)
				   (retrieve-to-avoid-collecting timeout-pointer))))
		      (for-each
			  (lambda (sock-fd)
			    (%curl-multi-socket-action multi sock-fd CURL_CSELECT_IN))
			read-ready)
		      (for-each
			  (lambda (sock-fd)
			    (%curl-multi-socket-action multi sock-fd CURL_CSELECT_OUT))
			write-ready)
		      (loop)))))
	      (let-values (((msg nmsgs)
			    (curl-multi-info-read multi)))
		(when msg
		  (%pretty-print (curl-constant-msg->symbol (curl-msg.msg msg)))))
	      #t)
	  ;;Close handles before releasing the callbacks!!!
	  (curl-multi-cleanup multi)
	  (curl-easy-cleanup easy)
	  (ffi.free-c-callback write-cb)
	  (ffi.free-c-callback timer-cb)
	  (forget-to-avoid-collecting pending-socks-pointer)))
    => #t)

  (collect))


;;;; done

(collect)
(check-report)

;;; end of file
