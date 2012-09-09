;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/cURL
;;;Contents: Libcurl bindings
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


#!vicare
#!(load-shared-library "vicare-curl")
(library (vicare net curl)
  (export

    ;; version numbers and strings
    vicare-curl-version-interface-current
    vicare-curl-version-interface-revision
    vicare-curl-version-interface-age
    vicare-curl-version
    curl-version			curl-version-info
    curl-version-info-features->symbols	curl-version-feature?

    ;; version info data structure
    curl-version-info-data		curl-version-info-data?
    curl-version-info-data-age		curl-version-info-data-version
    curl-version-info-data-version-num	curl-version-info-data-host
    curl-version-info-data-features	curl-version-info-data-ssl-version
    curl-version-info-data-ssl-version-num
    curl-version-info-data-libz-version	curl-version-info-data-protocols
    curl-version-info-data-ares		curl-version-info-data-ares-num
    curl-version-info-data-libidn	curl-version-info-data-iconv-ver-num
    curl-version-info-data-libssh-version

    ;; initialisation and finalisation functions
    curl-global-init			curl-global-init-mem
    curl-global-cleanup
    make-curl-malloc-callback		make-curl-free-callback
    make-curl-realloc-callback		make-curl-strdup-callback
    make-curl-calloc-callback

    ;; string lists
    curl-slist-append			curl-slist-free-all
    curl-slist->list			list->curl-slist

    ;; multipart/formdata composition
    curl-formadd			curl-formfree
    curl-formget			make-curl-formget-callback

    curl-form-data
    curl-form-data?			curl-form-data?/filled
    (rename (%make-curl-form-data	make-curl-form-data))
    curl-form-data-string
    curl-form-data-destructor		set-curl-form-data-destructor!

    ;; basic URL string escaping
    curl-escape				curl-escape/string
    curl-unescape			curl-unescape/string

    ;; shared configuration option sets
    curl-share-init			curl-share-cleanup
    curl-share-setopt
    curl-share-strerror			curl-share-strerror/string
    make-curl-lock-function		make-curl-unlock-function

    curl-share
    curl-share?				curl-share?/alive
    curl-share-destructor		set-curl-share-destructor!

    ;; miscellaneous functions
    curl-free				curl-getdate

    ;; easy API
    curl-easy-init			curl-easy-cleanup
    curl-easy-reset
    curl-easy-setopt			curl-easy-getinfo
    curl-easy-perform			curl-easy-duphandle
    curl-easy-recv			curl-easy-send
    curl-easy-strerror			curl-easy-pause
    curl-easy-escape			curl-easy-unescape

    curl-easy
    curl-easy?				curl-easy?/alive
    curl-easy-destructor		set-curl-easy-destructor!

    ;; multi API
    curl-multi-init
    curl-multi-add-handle
    curl-multi-remove-handle
    curl-multi-fdset
    curl-multi-perform
    curl-multi-cleanup
    curl-multi-info-read
    curl-multi-strerror
    curl-multi-socket
    curl-multi-socket-action
    curl-multi-socket-all
    curl-multi-timeout
    curl-multi-setopt
    curl-multi-assign

    ;; debugging
    curl-easy-garbage-collection-log
    curl-form-data-garbage-collection-log
    curl-share-garbage-collection-log)
  (import (vicare)
    (vicare net curl constants)
    (prefix (vicare net curl unsafe-capi)
	    capi.)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.)
    (prefix (vicare ffi)
	    ffi.)
    (prefix (vicare words)
	    words.))


;;;; arguments validation

(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))

(define-argument-validation (non-negative-fixnum who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as argument" obj))

(define-argument-validation (pointer who obj)
  (pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

(define-argument-validation (callback who obj)
  (or (not obj) (pointer? obj))
  (assertion-violation who "expected callback as argument" obj))

#;(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))

(define-argument-validation (vector who obj)
  (vector? obj)
  (assertion-violation who "expected vector as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (pointer/false who obj)
  (or (not obj) (pointer? obj))
  (assertion-violation who "expected false or pointer as argument" obj))

(define-argument-validation (general-string who obj)
  (or (string? obj)
      (bytevector? obj)
      (pointer? obj)
      (memory-block? obj))
  (assertion-violation who
    "expected string or bytevector or pointer or memory-block as argument" obj))

(define-argument-validation (general-output-buffer who obj)
  (or (bytevector? obj)
      (pointer? obj)
      (memory-block? obj))
  (assertion-violation who
    "expected bytevector or pointer or memory-block as argument" obj))

(define-argument-validation (list-of-strings who obj)
  (and (list? obj)
       (for-all string? obj))
  (assertion-violation who "expected list of strings as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-int who obj)
  (words.signed-int? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"int\" as argument"
    obj))

(define-argument-validation (signed-int/false who obj)
  (or (not obj) (words.signed-int? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"int\" as argument"
    obj))

(define-argument-validation (signed-long who obj)
  (words.signed-long? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"long\" as argument"
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (curl-version-info-data who obj)
  (curl-version-info-data? obj)
  (assertion-violation who
    "expected instance of \"curl-version-info-data\" as argument"
    obj))

(define-argument-validation (curl-form-data who obj)
  (curl-form-data? obj)
  (assertion-violation who
    "expected instance of \"curl-form-data\" as argument"
    obj))

(define-argument-validation (curl-form-data/filled who obj)
  (curl-form-data?/filled obj)
  (assertion-violation who
    "expected instance of \"curl-form-data\" as argument holding contents"
    obj))

(define-argument-validation (curl-share who obj)
  (curl-share? obj)
  (assertion-violation who "expected instance of \"curl-share\" as argument" obj))

(define-argument-validation (curl-share/alive who obj)
  (curl-share?/alive obj)
  (assertion-violation who "expected alive instance of \"curl-share\" as argument" obj))

(define-argument-validation (curl-share-parameter who parameter option)
  (cond ((or (= option CURLSHOPT_SHARE)
	     (= option CURLSHOPT_UNSHARE))
	 (words.signed-int? parameter))
	(else
	 (%callback? parameter)))
  (assertion-violation who
    "invalid matching between \"curl-share\" option and parameter"
    parameter option))

(define-argument-validation (curl-easy who obj)
  (curl-easy? obj)
  (assertion-violation who "expected instance of \"curl-easy\" as argument" obj))

(define-argument-validation (curl-easy/alive who obj)
  (curl-easy?/alive obj)
  (assertion-violation who "expected alive instance of \"curl-easy\" as argument" obj))


;;;; helpers

(define (%guardian-destructor-debugging-log struct-instance log-param)
  (let ((obj (log-param)))
    (define (%print port)
      (fprintf port "Vicare/cURL: guardian finalising ~a\n" struct-instance))
    (cond ((output-port? obj)
	   (%print obj))
	  ((procedure? obj)
	   (guard (E (else (void)))
	     (obj struct-instance)))
	  (obj
	   (%print (current-error-port))))))

(define (%garbage-collector-debugging-log-validator obj)
  (cond ((or (output-port? obj)
	     (procedure? obj))
	 obj)
	(obj	#t)
	(else	#f)))

;;; --------------------------------------------------------------------

(define-inline (%callback? ?obj)
  (or (not ?obj) (pointer? ?obj)))

(define-syntax with-general-strings/utf8
  (syntax-rules ()
    ((_ ((?var ?arg) ...) ?body0 . ?body)
     (let ((?var (let ((arg ?arg))
		   (cond ((string? arg)
			  (string->utf8 arg))
			 ((or (bytevector? arg)
			      (pointer?    arg)
			      (memory-block? arg))
			  arg)
			 (else
			  (assertion-violation #f "unexpected object" arg)))))
	   ...)
       ?body0 . ?body))))

(define-syntax with-general-strings/ascii
  (syntax-rules ()
    ((_ ((?var ?arg) ...) ?body0 . ?body)
     (let ((?var (let ((arg ?arg))
		   (cond ((string? arg)
			  (string->ascii arg))
			 ((or (bytevector? arg)
			      (pointer?    arg)
			      (memory-block? arg))
			  arg)
			 (else
			  (assertion-violation #f "unexpected object" arg)))))
	   ...)
       ?body0 . ?body))))


;;;; version functions

(define (vicare-curl-version-interface-current)
  (capi.vicare-curl-version-interface-current))

(define (vicare-curl-version-interface-revision)
  (capi.vicare-curl-version-interface-revision))

(define (vicare-curl-version-interface-age)
  (capi.vicare-curl-version-interface-age))

(define (vicare-curl-version)
  (ascii->string (capi.vicare-curl-version)))

;;; --------------------------------------------------------------------

(define (curl-version)
  (ascii->string (capi.curl-version)))

;;; --------------------------------------------------------------------

(define-struct curl-version-info-data
  (age
		;Age of the returned struct.
   version
		;A LIBCURL_VERSION constant
   version-num
		;The LIBCURL_VERSION_NUM constant.
   host
		;OS/host/cpu/machine when configured.
   features
		;Bitmask of CURL_VERSION_ constants.
   ssl-version
		;Human readable string.
   ssl-version-num
		;Not used anymore, always 0.
   libz-version
		;Human readable string.
   protocols
		;List of protocols is terminated by an entry with a NULL
		;protoname.
   ares
		;
   ares-num
		;
   libidn
		;
   iconv-ver-num
		;Same as '_libiconv_version' if built with HAVE_ICONV.
   libssh-version
		;Human readable string.
   ))

(define (%struct-curl-version-info-data-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[curl-version-info-data")
  (%display " age=")		(%display (curl-version-info-data-age S))
  (%display " version=")	(%write   (curl-version-info-data-version S))
  (%display " version-num=")	(%display (curl-version-info-data-version-num S))
  (%display " host=")		(%write   (curl-version-info-data-host S))
  (%display " features=")	(%display (curl-version-info-features->symbols S))
  (%display " ssl-version=")	(%write	  (curl-version-info-data-ssl-version S))
  (%display " ssl-version-num=")(%display (curl-version-info-data-ssl-version-num S))
  (%display " libz-version=")	(%write   (curl-version-info-data-libz-version S))
  (%display " protocols=")	(%write   (curl-version-info-data-protocols S))
  (%display " ares=")		(%write   (curl-version-info-data-ares S))
  (%display " ares-num=")	(%display (curl-version-info-data-ares-num S))
  (%display " libidn=")		(%write   (curl-version-info-data-libidn S))
  (%display " iconv-ver-num=")	(%display (curl-version-info-data-iconv-ver-num S))
  (%display " libssh-version=")	(%write   (curl-version-info-data-libssh-version S))
  (%display "]"))

(define (curl-version-info version-code)
  (define who 'curl-version-info)
  (with-arguments-validation (who)
      ((non-negative-fixnum	version-code))
    (let ((rv (capi.curl-version-info (type-descriptor curl-version-info-data) version-code)))
      (define-inline (b->s S G)
	(S rv (let ((b (G rv)))
		(and b (ascii->string b)))))
      (b->s set-curl-version-info-data-version!
	    curl-version-info-data-version)
      (b->s set-curl-version-info-data-host!
	    curl-version-info-data-host)
      (b->s set-curl-version-info-data-ssl-version!
	    curl-version-info-data-ssl-version)
      (b->s set-curl-version-info-data-libz-version!
	    curl-version-info-data-libz-version)
      (b->s set-curl-version-info-data-ares!
	    curl-version-info-data-ares)
      (b->s set-curl-version-info-data-libidn!
	    curl-version-info-data-libidn)
      (b->s set-curl-version-info-data-libssh-version!
	    curl-version-info-data-libssh-version)
      (set-curl-version-info-data-protocols!
       rv (map ascii->string (curl-version-info-data-protocols rv)))
      rv)))

(define (curl-version-info-features->symbols S)
  (define who 'curl-version-info-features->symbols)
  (with-arguments-validation (who)
      ((curl-version-info-data	S))
    (let loop ((result   '())
	       (features (curl-version-info-data-features S))
	       (flags	`(,CURL_VERSION_IPV6		,CURL_VERSION_KERBEROS4
			  ,CURL_VERSION_SSL		,CURL_VERSION_LIBZ
			  ,CURL_VERSION_NTLM		,CURL_VERSION_GSSNEGOTIATE
			  ,CURL_VERSION_DEBUG		,CURL_VERSION_ASYNCHDNS
			  ,CURL_VERSION_SPNEGO		,CURL_VERSION_LARGEFILE
			  ,CURL_VERSION_IDN		,CURL_VERSION_SSPI
			  ,CURL_VERSION_CONV		,CURL_VERSION_CURLDEBUG
			  ,CURL_VERSION_TLSAUTH_SRP	,CURL_VERSION_NTLM_WB)))
      (if (null? flags)
	  result
	(loop (let ((flag (unsafe.car flags)))
		(if (zero? (bitwise-and flag features))
		    result
		  (cons (%curl-version-info-features->symbols flag)
			result)))
	      features
	      (unsafe.cdr flags))))))

(define-exact-integer->symbol-function %curl-version-info-features->symbols
  (CURL_VERSION_IPV6
   CURL_VERSION_KERBEROS4
   CURL_VERSION_SSL
   CURL_VERSION_LIBZ
   CURL_VERSION_NTLM
   CURL_VERSION_GSSNEGOTIATE
   CURL_VERSION_DEBUG
   CURL_VERSION_ASYNCHDNS
   CURL_VERSION_SPNEGO
   CURL_VERSION_LARGEFILE
   CURL_VERSION_IDN
   CURL_VERSION_SSPI
   CURL_VERSION_CONV
   CURL_VERSION_CURLDEBUG
   CURL_VERSION_TLSAUTH_SRP
   CURL_VERSION_NTLM_WB))

(define (curl-version-feature? S feature)
  (define who 'curl-version-feature?)
  (with-arguments-validation (who)
      ((curl-version-info-data	S)
       (non-negative-fixnum	feature))
    (not (zero? (bitwise-and feature (curl-version-info-data-features S))))))


;;;; global initialisation and finalisation functions

(define (curl-global-init flags)
  (define who 'curl-global-init)
  (with-arguments-validation (who)
      ((signed-long	flags))
    (capi.curl-global-init flags)))

(define (curl-global-init-mem flags
			      malloc-callback free-callback realloc-callback
			      strdup-callback calloc-callback)
  (define who 'curl-global-init-mem)
  (with-arguments-validation (who)
      ((signed-long	flags)
       (callback	malloc-callback)
       (callback	free-callback)
       (callback	realloc-callback)
       (callback	strdup-callback)
       (callback	calloc-callback))
    (capi.curl-global-init-mem flags
			       malloc-callback free-callback realloc-callback
			       strdup-callback calloc-callback)))

(define (curl-global-cleanup)
  (capi.curl-global-cleanup))

;;; --------------------------------------------------------------------

(define make-curl-malloc-callback
  ;; void *(*curl_malloc_callback)(size_t size)
  (let ((maker (ffi.make-c-callback-maker 'pointer '(size_t))))
    (lambda (user-scheme-callback)
      (maker (lambda (number-of-bytes)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (null-pointer)))
		 (user-scheme-callback number-of-bytes)))))))

(define make-curl-free-callback
  ;; void (*curl_free_callback)(void *ptr)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (ptr)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (void)))
		 (user-scheme-callback ptr)))))))

(define make-curl-realloc-callback
  ;; void *(*curl_realloc_callback)(void *ptr, size_t size)
  (let ((maker (ffi.make-c-callback-maker 'pointer '(pointer size_t))))
    (lambda (user-scheme-callback)
      (maker (lambda (ptr number-of-bytes)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (null-pointer)))
		 (user-scheme-callback ptr number-of-bytes)))))))

(define make-curl-strdup-callback
  ;; char *(*curl_strdup_callback)(const char *str)
  (let ((maker (ffi.make-c-callback-maker 'pointer '(pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (ptr)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (null-pointer)))
		 (user-scheme-callback ptr)))))))

(define make-curl-calloc-callback
  ;; void *(*curl_calloc_callback)(size_t nmemb, size_t size)
  (let ((maker (ffi.make-c-callback-maker 'pointer '(size_t size_t))))
    (lambda (user-scheme-callback)
      (maker (lambda (number-of-items item-size)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (null-pointer)))
		 (user-scheme-callback number-of-items item-size)))))))


;;;; string lists

(define curl-slist-append
  (case-lambda
   ((string)
    (curl-slist-append #f string))
   ((slist string)
    (define who 'curl-slist-append)
    (with-arguments-validation (who)
	((pointer/false		slist)
	 (general-string	string))
      (with-general-strings/utf8 ((string^ string))
	(capi.curl-slist-append slist string^))))))

(define (curl-slist-free-all slist)
  (define who 'curl-slist-free-all)
  (with-arguments-validation (who)
      ((pointer/false	slist))
    (capi.curl-slist-free-all slist)))

(define (curl-slist->list slist)
  (define who 'curl-slist->list)
  (with-arguments-validation (who)
      ((pointer/false	slist))
    (map ascii->string (reverse (capi.curl-slist->list slist)))))

(define (list->curl-slist list-of-strings)
  (define who 'list->curl-slist)
  (with-arguments-validation (who)
      ((list-of-strings	list-of-strings))
    (fold-left (lambda (slist str)
		 (curl-slist-append slist str))
      #f list-of-strings)))


;;;; multipart/formdata composition

(define-struct curl-form-data
  (pointer destructor))

(define (%struct-curl-form-data-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[curl-form-data")
  (%display " pointer=")	(%display (curl-form-data-pointer S))
  (%display " data=")		(%write   (curl-form-data-string  S))
  (%display "]"))

(define (%unsafe.curl-formfree post)
  (let ((destructor (curl-form-data-destructor post)))
    (when destructor
      (guard (E (else (void)))
	(destructor post)))
    (set-curl-form-data-destructor! post #f))
  (capi.curl-formfree post))

;;; --------------------------------------------------------------------

(define %curl-form-data-guardian
  (make-guardian))

(define (%curl-form-data-guardian-destructor)
  (do ((P (%curl-form-data-guardian) (%curl-form-data-guardian)))
      ((not P))
    (%guardian-destructor-debugging-log P curl-form-data-garbage-collection-log)
    (%unsafe.curl-formfree P)
    (struct-reset P)))

(define curl-form-data-garbage-collection-log
  (make-parameter #f
    %garbage-collector-debugging-log-validator))

;;; --------------------------------------------------------------------

(define (curl-form-data?/filled obj)
  (and (curl-form-data? obj)
       (not (pointer-null? (curl-form-data-pointer obj)))))

(define (%make-curl-form-data)
  (%curl-form-data-guardian (make-curl-form-data (null-pointer) #f)))

(define (curl-form-data-string post)
  (define who 'curl-form-data-string)
  (with-arguments-validation (who)
      ((curl-form-data	post))
    (if (pointer-null? (curl-form-data-pointer post))
	""
      (let* ((data "")
	     (cb   (make-curl-formget-callback
		    (lambda (custom-data cstring.ptr cstring.len)
		      (set! data (string-append data (cstring->string cstring.ptr cstring.len)))
		      cstring.len))))
	(unwind-protect
	    (and (not (curl-formget post #f cb))
		 data)
	  (ffi.free-c-callback cb))))))

;;; --------------------------------------------------------------------

(module (curl-formadd)

  (define-inline (%normalise-val val)
    (cond ((words.signed-long? val)
	   val)
	  ((string? val)
	   (string->ascii val))
	  (else
	   val)))

  (define-argument-validation (a-value who obj)
    (or (words.signed-long? obj)
	(string? obj)
	(bytevector? obj)
	(pointer? obj)
	(memory-block? obj))
    (assertion-violation who
      "invalid HTTP form option's value" obj))

  (define curl-formadd
    (case-lambda
     ((post last opt1 val1 optend)
      (arguments-validation-forms
       (assert (eqv? optend CURLFORM_END)))
      (curl-formadd post last
		    opt1 val1))

     ((post last opt1 val1)
      (define who 'curl-formadd)
      (with-arguments-validation (who)
	  ((curl-form-data	post)
	   (pointer		last)
	   (signed-int		opt1)
	   (a-value		val1))
	(capi.curl-formadd-1 post last
			     opt1 (%normalise-val val1))))

     ((post last opt1 val1 opt2 val2 optend)
      (arguments-validation-forms
       (assert (eqv? optend CURLFORM_END)))
      (curl-formadd post last
		    opt1 val1
		    opt2 val2))

     ((post last opt1 val1 opt2 val2)
      (define who 'curl-formadd)
      (with-arguments-validation (who)
	  ((curl-form-data	post)
	   (pointer		last)
	   (signed-int		opt1)
	   (a-value		val1)
	   (signed-int		opt2)
	   (a-value		val2))
	(capi.curl-formadd-2 post last
			     opt1 (%normalise-val val1)
			     opt2 (%normalise-val val2))))

     ((post last opt1 val1 opt2 val2 opt3 val3 optend)
      (arguments-validation-forms
       (assert (eqv? optend CURLFORM_END)))
      (curl-formadd post last
		    opt1 val1
		    opt2 val2
		    opt3 val3))

     ((post last opt1 val1 opt2 val2 opt3 val3)
      (define who 'curl-formadd)
      (with-arguments-validation (who)
	  ((curl-form-data	post)
	   (pointer		last)
	   (signed-int		opt1)
	   (a-value		val1)
	   (signed-int		opt2)
	   (a-value		val2)
	   (signed-int		opt3)
	   (a-value		val3))
	(capi.curl-formadd-3 post last
			     opt1 (%normalise-val val1)
			     opt2 (%normalise-val val2)
			     opt3 (%normalise-val val3))))

     ((post last opt1 val1 opt2 val2 opt3 val3 opt4 val4 optend)
      (arguments-validation-forms
       (assert (eqv? optend CURLFORM_END)))
      (curl-formadd post last
		    opt1 val1
		    opt2 val2
		    opt3 val3
		    opt4 val4))

     ((post last opt1 val1 opt2 val2 opt3 val3 opt4 val4)
      (define who 'curl-formadd)
      (with-arguments-validation (who)
	  ((curl-form-data	post)
	   (pointer		last)
	   (signed-int		opt1)
	   (a-value		val1)
	   (signed-int		opt2)
	   (a-value		val2)
	   (signed-int		opt3)
	   (a-value		val3)
	   (signed-int		opt4)
	   (a-value		val4))
	(capi.curl-formadd-4 post last
			     opt1 (%normalise-val val1)
			     opt2 (%normalise-val val2)
			     opt3 (%normalise-val val3)
			     opt4 (%normalise-val val4))))
     ))

  #| end of module |# )

(define (curl-formget post custom-data callback)
  (define who 'curl-formget)
  (with-arguments-validation (who)
      ((curl-form-data	post)
       (pointer/false	custom-data)
       (callback	callback))
    (capi.curl-formget post custom-data callback)))

(define (curl-formfree post)
  (define who 'curl-formfree)
  (with-arguments-validation (who)
      ((curl-form-data	post))
    (%unsafe.curl-formfree post)))

;;; --------------------------------------------------------------------

(define make-curl-formget-callback
  ;; size_t curl_formget_callback (void *arg, const char *buf, size_t len)
  (let ((maker (ffi.make-c-callback-maker 'size_t '(pointer pointer size_t))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data cstring.ptr cstring.len)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback custom-data cstring.ptr cstring.len)))))))


;;;; basic URL string escaping

(define curl-escape
  (case-lambda
   ((str.data)
    (curl-escape str.data #f))
   ((str.data str.len)
    (define who 'curl-escape)
    (with-arguments-validation (who)
	((general-string	str.data)
	 (signed-int/false	str.len))
      (with-general-strings/ascii ((str.data^ str.data))
	(capi.curl-escape str.data^ str.len))))))

(define curl-unescape
  (case-lambda
   ((str.data)
    (curl-unescape str.data #f))
   ((str.data str.len)
    (define who 'curl-unescape)
    (with-arguments-validation (who)
	((general-string	str.data)
	 (signed-int/false	str.len))
      (with-general-strings/ascii ((str.data^ str.data))
	(capi.curl-unescape str.data^ str.len))))))

;;; --------------------------------------------------------------------

(define curl-escape/string
  (case-lambda
   ((str.data)
    (curl-escape/string str.data #f))
   ((str.data str.len)
    (let ((rv (curl-escape str.data str.len)))
      (and rv (ascii->string rv))))))

(define curl-unescape/string
  (case-lambda
   ((str.data)
    (curl-unescape/string str.data #f))
   ((str.data str.len)
    (let ((rv (curl-unescape str.data str.len)))
      (and rv (ascii->string rv))))))


;;;; shared configuration option sets

(define-struct curl-share
  (pointer destructor))

(define (%struct-curl-share-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[curl-share")
  (%display " pointer=")	(%display (curl-share-pointer S))
  (%display "]"))

(define (%unsafe.curl-share-cleanup share)
  (let ((destructor (curl-share-destructor share)))
    (when destructor
      (guard (E (else (void)))
	(destructor share)))
    (set-curl-share-destructor! share #f))
  (capi.curl-share-cleanup share))

;;; --------------------------------------------------------------------

(define %curl-share-guardian
  (make-guardian))

(define (%curl-share-guardian-destructor)
  (do ((P (%curl-share-guardian) (%curl-share-guardian)))
      ((not P))
    (%guardian-destructor-debugging-log P curl-share-garbage-collection-log)
    (%unsafe.curl-share-cleanup P)
    (struct-reset P)))

(define curl-share-garbage-collection-log
  (make-parameter #f
    %garbage-collector-debugging-log-validator))

;;; --------------------------------------------------------------------

(define (curl-share?/alive obj)
  (and (curl-share? obj)
       (not (pointer-null? (curl-share-pointer obj)))))

;;; --------------------------------------------------------------------

(define (curl-share-init)
  (%curl-share-guardian (make-curl-share (capi.curl-share-init) #f)))

(define (curl-share-setopt share option parameter)
  (define who 'curl-share-setopt)
  (with-arguments-validation (who)
      ((curl-share/alive	share)
       (signed-int		option)
       (curl-share-parameter	parameter option))
    (capi.curl-share-setopt share option parameter)))

(define (curl-share-cleanup share)
  (define who 'curl-share-cleanup)
  (with-arguments-validation (who)
      ((curl-share	share))
    (%unsafe.curl-share-cleanup share)))

(define (curl-share-strerror errcode)
  (define who 'curl-share-strerror)
  (with-arguments-validation (who)
      ((signed-int	errcode))
    (capi.curl-share-strerror errcode)))

(define (curl-share-strerror/string errcode)
  (ascii->string (curl-share-strerror errcode)))

;;; --------------------------------------------------------------------

(define make-curl-lock-function
  ;; void curl_lock_function (CURL *handle, curl_lock_data data,
  ;;                          curl_lock_access locktype, void *userptr)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer signed-int signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle data locktype userptr)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (void)))
		 (user-scheme-callback (%make-curl-easy handle #f)
				       data locktype
				       (if (pointer-null? userptr)
					   #f
					 userptr))
		 (void)))))))

(define make-curl-unlock-function
  ;; void curl_unlock_function (CURL *handle, curl_lock_data data, void *userptr)
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle data userptr)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (void)))
		 (user-scheme-callback (%make-curl-easy handle #f)
				       data
				       (if (pointer-null? userptr)
					   #f
					 userptr))
		 (void)))))))


;;;; easy API

(define-struct curl-easy
  (pointer
		;Pointer object referencing an instance of "CURL".
   owner?
		;Boolean,  true   if  this   data  structure   owns  the
		;referenced "CURL" instance.
   destructor
   ))

(define (curl-easy?/alive obj)
  (and (curl-easy? obj)
       (not (pointer-null? (curl-easy-pointer obj)))))

(define (%struct-curl-easy-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[curl-easy")
  (%display " pointer=")	(%display (curl-easy-pointer S))
  (%display " owner?=")		(%display (curl-easy-owner?  S))
  (%display "]"))

(define (%unsafe.curl-easy-cleanup easy)
  (let ((destructor (curl-easy-destructor easy)))
    (when destructor
      (guard (E (else (void)))
	(destructor easy)))
    (set-curl-easy-destructor! easy #f))
  (capi.curl-easy-cleanup easy))

;;; --------------------------------------------------------------------

(define %curl-easy-guardian
  (make-guardian))

(define (%curl-easy-guardian-destructor)
  (do ((P (%curl-easy-guardian) (%curl-easy-guardian)))
      ((not P))
    (%guardian-destructor-debugging-log P curl-easy-garbage-collection-log)
    (capi.curl-easy-cleanup P)
    (struct-reset P)))

(define curl-easy-garbage-collection-log
  (make-parameter #f
    %garbage-collector-debugging-log-validator))

(define (%make-curl-easy pointer owner?)
  (%curl-easy-guardian (make-curl-easy pointer owner? #f)))

;;; --------------------------------------------------------------------

(define (curl-easy-init)
  (let ((rv (capi.curl-easy-init)))
    (and rv (%make-curl-easy rv #t))))

(define (curl-easy-cleanup easy)
  (define who 'curl-easy-cleanup)
  (with-arguments-validation (who)
      ((curl-easy	easy))
    (capi.curl-easy-cleanup easy)))

(define (curl-easy-reset easy)
  (define who 'curl-easy-reset)
  (with-arguments-validation (who)
      ((curl-easy/alive	easy))
    (capi.curl-easy-reset easy)))

;;; --------------------------------------------------------------------

(define (curl-easy-setopt easy option parameter)
  (define who 'curl-easy-setopt)
  (with-arguments-validation (who)
      ((curl-easy/alive	easy)
       (signed-int	option))
    (capi.curl-easy-setopt easy option parameter)))

(define (curl-easy-getinfo easy info)
  (define who 'curl-easy-getinfo)
  (with-arguments-validation (who)
      ((curl-easy/alive	easy)
       (signed-int	info))
    (capi.curl-easy-getinfo easy info)))

;;; --------------------------------------------------------------------

(define (curl-easy-perform easy)
  (define who 'curl-easy-perform)
  (with-arguments-validation (who)
      ((curl-easy/alive	easy))
    (capi.curl-easy-perform easy)))

(define (curl-easy-duphandle easy)
  (define who 'curl-easy-duphandle)
  (with-arguments-validation (who)
      ((curl-easy/alive	easy))
    (let ((rv (capi.curl-easy-duphandle easy)))
      (and rv (%make-curl-easy rv #t)))))

(define (curl-easy-pause easy bitmask)
  (define who 'curl-easy-pause)
  (with-arguments-validation (who)
      ((curl-easy/alive	easy)
       (signed-int	bitmask))
    (capi.curl-easy-pause easy bitmask)))

;;; --------------------------------------------------------------------

(define (curl-easy-recv easy buffer.data buffer.len)
  (define who 'curl-easy-recv)
  (with-arguments-validation (who)
      ((curl-easy/alive		easy)
       (general-output-buffer	buffer.data)
       (signed-int/false	buffer.len))
    (capi.curl-easy-recv easy buffer.data buffer.len)))

(define (curl-easy-send easy buffer.data buffer.len)
  (define who 'curl-easy-send)
  (with-arguments-validation (who)
      ((curl-easy/alive		easy)
       (general-string		buffer.data)
       (signed-int/false	buffer.len))
    (with-general-strings/utf8 ((buffer.data^ buffer.data))
      (capi.curl-easy-send easy buffer.data^ buffer.len))))

;;; --------------------------------------------------------------------

(define (curl-easy-escape easy chars.data chars.len)
  (define who 'curl-easy-escape)
  (with-arguments-validation (who)
      ((curl-easy/alive		easy)
       (general-string		chars.data)
       (signed-int/false	chars.len))
    (with-general-strings/utf8 ((chars.data^ chars.data))
      (capi.curl-easy-escape easy chars.data chars.len))))

(define (curl-easy-unescape easy chars.data chars.len)
  (define who 'curl-easy-unescape)
  (with-arguments-validation (who)
      ((curl-easy/alive		easy)
       (general-string		chars.data)
       (signed-int/false	chars.len))
    (with-general-strings/utf8 ((chars.data^ chars.data))
      (capi.curl-easy-unescape easy chars.data chars.len))))

;;; --------------------------------------------------------------------

(define (curl-easy-strerror code)
  (define who 'curl-easy-strerror)
  (with-arguments-validation (who)
      ((signed-int	code))
    (capi.curl-easy-strerror code)))


;;;; miscellaneous functions

(define (curl-free pointer)
  (define who 'curl-free)
  (with-arguments-validation (who)
      ((pointer/false	pointer))
    (capi.curl-free pointer)))

(define (curl-getdate date)
  (define who 'curl-getdate)
  (with-arguments-validation (who)
      ((general-string	date))
    (with-general-strings/ascii ((date^ date))
      (capi.curl-getdate date^))))


;;;; callback makers

 ;; int (*curl_progress_callback)(void *clientp,
 ;;                                      double dltotal,
 ;;                                      double dlnow,
 ;;                                      double ultotal,
 ;;                                      double ulnow);

 ;; size_t (*curl_write_callback)(char *buffer,
 ;;                                      size_t size,
 ;;                                      size_t nitems,
 ;;                                      void *outstream);

 ;; long (*curl_chunk_bgn_callback)(const void *transfer_info,
 ;;                                        void *ptr,
 ;;                                        int remains);

;; long (*curl_chunk_end_callback)(void *ptr);

 ;; int (*curl_fnmatch_callback)(void *ptr,
 ;;                                     const char *pattern,
 ;;                                     const char *string);

 ;; int (*curl_seek_callback)(void *instream,
 ;;                                  curl_off_t offset,
 ;;                                  int origin);

 ;; size_t (*curl_read_callback)(char *buffer,
 ;;                                      size_t size,
 ;;                                      size_t nitems,
 ;;                                      void *instream);

 ;; int (*curl_sockopt_callback)(void *clientp,
 ;;                                     curl_socket_t curlfd,
 ;;                                     curlsocktype purpose);

;;  curl_socket_t
;; (*curl_opensocket_callback)(void *clientp,
;;                             curlsocktype purpose,
;;                             struct curl_sockaddr *address);

;; int
;; (*curl_closesocket_callback)(void *clientp, curl_socket_t item);

 ;; curlioerr (*curl_ioctl_callback)(CURL *handle,
 ;;                                         int cmd,
 ;;                                         void *clientp);

 ;; int (*curl_debug_callback)
 ;;       (CURL *handle,      /* the handle/transfer this concerns */
 ;;        curl_infotype type, /* what kind of data */
 ;;        char *data,        /* points to the data */
 ;;        size_t size,       /* size of the data pointed to */
 ;;        void *userptr);    /* whatever the user please */

 ;; CURLcode (*curl_conv_callback)(char *buffer, size_t length);

 ;; CURLcode (*curl_ssl_ctx_callback)(CURL *curl,    /* easy handle */
 ;;                                          void *ssl_ctx, /* actually an
 ;;                                                            OpenSSL SSL_CTX */
 ;;                                          void *userptr);

 ;; int
 ;;  (*curl_sshkeycallback) (CURL *easy,     /* easy handle */
 ;;                          const struct curl_khkey *knownkey, /* known */
 ;;                          const struct curl_khkey *foundkey, /* found */
 ;;                          enum curl_khmatch, /* libcurl's view on the keys */
 ;;                          void *clientp); /* custom pointer passed from app */

 ;; int (*curl_socket_callback)(CURL *easy,
 ;;                                    curl_socket_t s,
 ;;                                    int what,
 ;;                                    void *userp,
 ;;                                    void *socketp);

 ;; int (*curl_multi_timer_callback)(CURLM *multi, long timeout_ms, void *userp);



;;;; still to be implemented

(define-inline (unimplemented who)
  (assertion-violation who "unimplemented function"))

(define (curl-multi-init . args)
  (define who 'curl-multi-init)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-add-handle . args)
  (define who 'curl-multi-add-handle)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-remove-handle . args)
  (define who 'curl-multi-remove-handle)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-fdset . args)
  (define who 'curl-multi-fdset)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-perform . args)
  (define who 'curl-multi-perform)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-cleanup . args)
  (define who 'curl-multi-cleanup)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-info-read . args)
  (define who 'curl-multi-info-read)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-strerror . args)
  (define who 'curl-multi-strerror)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-socket . args)
  (define who 'curl-multi-socket)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-socket-action . args)
  (define who 'curl-multi-socket-action)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-socket-all . args)
  (define who 'curl-multi-socket-all)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-timeout . args)
  (define who 'curl-multi-timeout)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-setopt . args)
  (define who 'curl-multi-setopt)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-assign . args)
  (define who 'curl-multi-assign)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))


;;;; done

(set-rtd-printer! (type-descriptor curl-version-info-data)
		  %struct-curl-version-info-data-printer)
(set-rtd-printer! (type-descriptor curl-form-data)	%struct-curl-form-data-printer)
(set-rtd-printer! (type-descriptor curl-share)		%struct-curl-share-printer)
(set-rtd-printer! (type-descriptor curl-easy)		%struct-curl-easy-printer)

(post-gc-hooks (cons* %curl-form-data-guardian-destructor
		      %curl-share-guardian-destructor
		      %curl-easy-guardian-destructor
		      (post-gc-hooks)))

)

;;; end of file
;;Local Variables:
;;eval: (put 'with-general-strings/utf8 'scheme-indent-function 1)
;;eval: (put 'with-general-strings/ascii 'scheme-indent-function 1)
;;End:
