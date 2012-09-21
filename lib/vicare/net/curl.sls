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
    curl-version				curl-version-info
    curl-version-info-features->symbols		curl-version-feature?

    ;; version info data structure
    curl-version-info-data			curl-version-info-data?
    curl-version-info-data-age			curl-version-info-data-version
    curl-version-info-data-version-num		curl-version-info-data-host
    curl-version-info-data-features		curl-version-info-data-ssl-version
    curl-version-info-data-ssl-version-num
    curl-version-info-data-libz-version		curl-version-info-data-protocols
    curl-version-info-data-ares			curl-version-info-data-ares-num
    curl-version-info-data-libidn		curl-version-info-data-iconv-ver-num
    curl-version-info-data-libssh-version

    ;; initialisation and finalisation functions
    curl-global-init				curl-global-init-mem
    curl-global-cleanup
    make-curl-malloc-callback			make-curl-free-callback
    make-curl-realloc-callback			make-curl-strdup-callback
    make-curl-calloc-callback

    ;; string lists
    curl-slist-append				curl-slist-free-all
    curl-slist->list				list->curl-slist

    ;; multipart/formdata composition
    curl-formadd				curl-formfree
    curl-formget				make-curl-formget-callback

    curl-form-data
    curl-form-data?				curl-form-data?/filled
    (rename (%make-curl-form-data		make-curl-form-data))
    curl-form-data-string
    curl-form-data-destructor
    (rename (%set-curl-form-data-destructor!	set-curl-form-data-destructor!))

    ;; basic URL string escaping
    curl-escape					curl-escape/string
    curl-unescape				curl-unescape/string

    ;; shared configuration option sets
    curl-share-init				curl-share-cleanup
    curl-share-setopt
    curl-share-strerror				curl-share-strerror/string
    make-curl-lock-function			make-curl-unlock-function

    curl-share
    curl-share?					curl-share?/alive
    curl-share-destructor
    (rename (%set-curl-share-destructor!	set-curl-share-destructor!))

    ;; easy API
    curl-easy-init				curl-easy-cleanup
    curl-easy-reset
    curl-easy-setopt				curl-easy-getinfo
    curl-easy-perform				curl-easy-duphandle
    curl-easy-recv				curl-easy-send
    curl-easy-strerror				curl-easy-pause
    curl-easy-escape				curl-easy-escape/string
    curl-easy-unescape				curl-easy-unescape/string

    curl-easy
    curl-easy?					curl-easy?/alive
    curl-easy-destructor
    (rename (%set-curl-easy-destructor!		set-curl-easy-destructor!))

    ;; multi API
    curl-multi-init				curl-multi-cleanup
    curl-multi-add-handle			curl-multi-remove-handle
    (rename (%curl-multi-easies curl-multi-easies))
    curl-multi-setopt				curl-multi-fdset
    curl-multi-perform				curl-multi-info-read
    curl-multi-socket				curl-multi-socket-action
    curl-multi-socket-all
    curl-multi-timeout				curl-multi-assign
    curl-multi-strerror

    curl-multi
    curl-multi?					curl-multi?/alive
    curl-multi-destructor
    (rename (%set-curl-multi-destructor!	set-curl-multi-destructor!))

    ;; callback makers
    make-curl-write-callback			make-curl-read-callback
    make-curl-ioctl-callback			make-curl-seek-callback
    make-curl-socket-option-callback		make-curl-open-socket-callback
    make-curl-close-socket-callback		make-curl-progress-callback
    make-curl-header-callback			make-curl-debug-callback
    make-curl-ssl-ctx-callback			make-curl-conv-to-network-callback
    make-curl-conv-from-network-callback	make-curl-conv-from-utf8-callback
    make-curl-interleave-callback		make-curl-chunk-begin-callback
    make-curl-chunk-end-callback		make-curl-fnmatch-callback
    make-curl-sshkey-callback			make-curl-socket-callback
    make-curl-multi-timer-callback

    ;; miscellaneous functions
    curl-free					curl-getdate

    ;; accessors for "struct curl_sockaddr"
    curl-sockaddr.family			curl-sockaddr.socktype
    curl-sockaddr.protocol			curl-sockaddr.addrlen
    curl-sockaddr.addr

    ;; accessors for "struct curl_fileinfo"
    curl-fileinfo.filename			curl-fileinfo.filetype
    curl-fileinfo.time				curl-fileinfo.perm
    curl-fileinfo.uid				curl-fileinfo.gid
    curl-fileinfo.size				curl-fileinfo.hardlinks
    curl-fileinfo.strings.time			curl-fileinfo.strings.perm
    curl-fileinfo.strings.user			curl-fileinfo.strings.group
    curl-fileinfo.strings.target		curl-fileinfo.flags

    curl-fileinfo?				curl-fileinfo->struct
    curl-fileinfo-filename			curl-fileinfo-filetype
    curl-fileinfo-time				curl-fileinfo-perm
    curl-fileinfo-uid				curl-fileinfo-gid
    curl-fileinfo-size				curl-fileinfo-hardlinks
    curl-fileinfo-strings.time			curl-fileinfo-strings.perm
    curl-fileinfo-strings.user			curl-fileinfo-strings.group
    curl-fileinfo-strings.target		curl-fileinfo-flags

    ;; accessors for "struct curl_khkey"
    curl-khkey.key

    ;; accessors and mutators for "struct curl_forms" arrays
    curl-forms-sizeof-array
    curl-forms.option				curl-forms.option-set!
    curl-forms.value				curl-forms.value-set!

    ;; accessors for "struct curl_certinfo"
    curl-certinfo.certinfo

    ;; accessors for "struct CURLMsg"
    curl-msg.msg				curl-msg.easy_handle
    curl-msg.data.whatever			curl-msg.data.result

    ;; constants to symbols
    curlmsg->symbol
    )
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

(define-argument-validation (procedure/false who obj)
  (or (not obj) (procedure? obj))
  (assertion-violation who "expected false or procedure as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (pointer/false who obj)
  (or (not obj) (pointer? obj))
  (assertion-violation who "expected false or pointer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (general-string who obj)
  (or (string? obj)
      (bytevector? obj)
      (pointer? obj)
      (memory-block? obj))
  (assertion-violation who
    "expected string or bytevector or pointer or memory-block as argument" obj))

(define-argument-validation (general-string/false who obj)
  (or (not obj)
      (string? obj)
      (bytevector? obj)
      (pointer? obj)
      (memory-block? obj))
  (assertion-violation who
    "expected false or string or bytevector or pointer or memory-block as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (general-buffer who obj)
  (or (bytevector? obj)
      (pointer? obj)
      (memory-block? obj))
  (assertion-violation who
    "expected bytevector or pointer or memory-block as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (general-data who obj)
  (or (bytevector? obj)
      (pointer? obj)
      (memory-block? obj))
  (assertion-violation who
    "expected bytevector or pointer or memory-block as argument" obj))

(define-argument-validation (list-of-strings who obj)
  (and (list? obj)
       (for-all string? obj))
  (assertion-violation who "expected list of strings as argument" obj))

(define-argument-validation (pointer/memory-block who obj)
  (or (pointer? obj)
      (memory-block? obj))
  (assertion-violation who "expected pointer or memory block as argument" obj))

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

(define-argument-validation (off_t who obj)
  (words.off_t? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"off_t\" as argument"
    obj))

(define-argument-validation (optional-buffer-length who obj buffer)
  (if (pointer? buffer)
      (words.size_t? obj)
    #t)
  (assertion-violation who
    "expected exact integer representing \"size_t\" as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (curl-version-info-data who obj)
  (curl-version-info-data? obj)
  (assertion-violation who
    "expected instance of \"curl-version-info-data\" as argument"
    obj))

;;; --------------------------------------------------------------------

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

;;; --------------------------------------------------------------------

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

;;; --------------------------------------------------------------------

(define-argument-validation (curl-easy who obj)
  (curl-easy? obj)
  (assertion-violation who "expected instance of \"curl-easy\" as argument" obj))

(define-argument-validation (curl-easy/alive who obj)
  (curl-easy?/alive obj)
  (assertion-violation who "expected alive instance of \"curl-easy\" as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (curl-multi who obj)
  (curl-multi? obj)
  (assertion-violation who "expected instance of \"curl-multi\" as argument" obj))

(define-argument-validation (curl-multi/alive who obj)
  (curl-multi?/alive obj)
  (assertion-violation who "expected alive instance of \"curl-multi\" as argument" obj))


;;;; helpers

(define-inline (unimplemented who)
  (assertion-violation who "unimplemented function"))

(define-auxiliary-syntaxes pointer)
(define-auxiliary-syntaxes owner?)

(define-syntax struct-destructor-application
  ;;Data structures are might have  a field called DESTRUCTOR holding #f
  ;;or a function to be applied to the struct instance upon finalisation
  ;;(either when the finaliser is  explicitly called by the application,
  ;;or when  the garbage collector  performs the finalisation  through a
  ;;guardian).
  ;;
  ;;This macro should  be used in the finalisation  function to properly
  ;;apply the destructor to the structure.
  ;;
  ;;For example, given the definition:
  ;;
  ;;  (define-struct the-type (the-field destructor))
  ;;
  ;;the code:
  ;;
  ;;  (define (%unsafe.the-type-final struct)
  ;;    (struct-destructor-application struct
  ;;      the-type-destructor set-the-type-destructor!))
  ;;
  ;;expands to:
  ;;
  ;;  (define (%unsafe.the-type-final struct)
  ;;    (let ((destructor (the-type-destructor struct)))
  ;;      (when destructor
  ;;        (guard (E (else (void)))
  ;;          (destructor struct))
  ;;        (?mutator ?struct #f))))
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?struct ?accessor ?mutator)
       (and (identifier? #'?struct)
	    (identifier? #'?accessor))
       #'(let ((destructor (?accessor ?struct)))
	   (when destructor
	     (guard (E (else (void)))
	       (destructor ?struct))
	     (?mutator ?struct #f)))))))

;;; --------------------------------------------------------------------

(define-inline (%callback? ?obj)
  (or (not ?obj) (pointer? ?obj)))

(define-inline (%define-raw-struct-accessor ?who ?accessor)
  ;;Used  to define  a  function to  access a  C  language struct  field
  ;;through a pointer to it.
  ;;
  (define (?who stru)
    (define who '?who)
    (with-arguments-validation (who)
	((pointer	stru))
      (?accessor stru))))

;;; --------------------------------------------------------------------

(define-auxiliary-syntaxes string-to-bytevector)

(define-syntax* (with-general-strings stx)
  (syntax-case stx (string-to-bytevector)
    ((_ ((?str^ ?str) ...) ?string->bytevector ?body0 . ?body)
     (identifier? #'?string->bytevector)
     #'(with-general-strings ((?str^ ?str) ...)
	   (string-to-bytevector ?string->bytevector)
	 ?body0 . ?body))
    ((_ ((?str^ ?str) ...)
	(string-to-bytevector ?string->bytevector)
	?body0 . ?body)
     (identifier? #'?string->bytevector)
     #'(let ((?str^ (let ((str ?str))
		      (cond ((string? str)
			     (?string->bytevector str))
			    ((or (bytevector?   str)
				 (pointer?      str)
				 (memory-block? str))
			     str)
			    (else
			     (assertion-violation #f "invalid general string" str)))))
	     ...)
	 ?body0 . ?body))))

(define-syntax with-general-strings/false
  (lambda (stx)
    (syntax-case stx (string-to-bytevector)
      ((?key ((?str^ ?str) ...) ?string->bytevector ?body0 . ?body)
       (identifier? #'?string->bytevector)
       #'(with-general-strings/false ((?str^ ?str) ...)
	     (string-to-bytevector ?string->bytevector)
	   ?body0 . ?body))
      ((_ ((?str^ ?str) ...)
	  (string-to-bytevector ?string->bytevector)
	  ?body0 . ?body)
       (identifier? #'?string->bytevector)
       #'(let ((?str^ (let ((str ?str))
			(cond ((string? str)
			       (?string->bytevector str))
			      ((or (bytevector?   str)
				   (pointer?      str)
				   (memory-block? str))
			       str)
			      ((not str)
			       str)
			      (else
			       (assertion-violation #f "invalid general string" str)))))
	       ...)
	   ?body0 . ?body)))))


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
      (with-general-strings ((string^ string))
	  (string-to-bytevector string->utf8)
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
  (pointer
		;Pointer  object  referencing  an  instance  of  "struct
		;curl_httppost".
   destructor
		;False or a user-supplied function to be called whenever
		;this instance  is finalised.  The function  must accept
		;at least one argument being the data structure itself.
   ))

(define (%struct-curl-form-data-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[curl-form-data")
  (%display " pointer=")	(%display ($curl-form-data-pointer S))
  (%display " data=")		(%write   (curl-form-data-string   S))
  (%display "]"))

(define (%unsafe.curl-formfree post)
  (struct-destructor-application post
				 $curl-form-data-destructor
				 $set-curl-form-data-destructor!)
  (capi.curl-formfree post))

;;; --------------------------------------------------------------------

(define (curl-form-data?/filled obj)
  (and (curl-form-data? obj)
       (not (pointer-null? ($curl-form-data-pointer obj)))))

(define (%make-curl-form-data)
  (make-curl-form-data (null-pointer) #f))

(define (%set-curl-form-data-destructor! struct destructor-func)
  (define who 'set-curl-form-data-destructor!)
  (with-arguments-validation (who)
      ((curl-form-data	struct)
       (procedure/false	destructor-func))
    ($set-curl-form-data-destructor! struct destructor-func)))

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

;;; --------------------------------------------------------------------

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
      (with-general-strings ((str.data^ str.data))
	  string->ascii
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
      (with-general-strings ((str.data^ str.data))
	  string->ascii
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
  (pointer
		;Pointer object referencing an instance of "CURLSH".
   destructor
		;False or a user-supplied function to be called whenever
		;this instance  is finalised.  The function  must accept
		;at least one argument being the data structure itself.
   ))

(define (%struct-curl-share-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[curl-share")
  (%display " pointer=")	(%display ($curl-share-pointer S))
  (%display "]"))

(define (%unsafe.curl-share-cleanup share)
  (struct-destructor-application share $curl-share-destructor $set-curl-share-destructor!)
  (capi.curl-share-cleanup share))

;;; --------------------------------------------------------------------

(define-inline (%make-curl-share pointer)
  (make-curl-share pointer #f))

(define (curl-share?/alive obj)
  (and (curl-share? obj)
       (not (pointer-null? (curl-share-pointer obj)))))

(define (%set-curl-share-destructor! struct destructor-func)
  (define who 'set-curl-share-destructor!)
  (with-arguments-validation (who)
      ((curl-share	struct)
       (procedure/false	destructor-func))
    ($set-curl-share-destructor! struct destructor-func)))

;;; --------------------------------------------------------------------

(define (curl-share-init)
  (let ((rv (capi.curl-share-init)))
    (and rv (%make-curl-share rv))))

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
		 (user-scheme-callback (%make-curl-easy
					   (pointer handle)
					 (owner? #f))
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
		 (user-scheme-callback (%make-curl-easy
					   (pointer handle)
					 (owner? #f))
				       data
				       (if (pointer-null? userptr)
					   #f
					 userptr))
		 (void)))))))


;;;; easy API

(define-struct curl-easy
  (pointer
		;Pointer  object  referencing  an   instance  of  the  C
		;language type "CURL".
   owner?
		;Boolean,  true   if  this   data  structure   owns  the
		;referenced "CURL" instance.
   destructor
		;False or a user-supplied function to be called whenever
		;this instance  is finalised.  The function  must accept
		;at least one argument being the data structure itself.
   ))

(define (%struct-curl-easy-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[curl-easy")
  (%display " pointer=")	(%display ($curl-easy-pointer S))
  (%display " owner?=")		(%display ($curl-easy-owner?  S))
  (%display "]"))

;;; --------------------------------------------------------------------

(define-syntax %make-curl-easy
  (syntax-rules (pointer owner?)
    ((_ (pointer ?pointer) (owner? ?owner?))
     (make-curl-easy ?pointer ?owner? #f))
    ((_ ?pointer ?owner?)
     (make-curl-easy ?pointer ?owner? #f))))

(define (curl-easy?/alive obj)
  (and (curl-easy? obj)
       (not (pointer-null? (curl-easy-pointer obj)))))

(define (%unsafe.curl-easy-cleanup easy)
  (struct-destructor-application easy $curl-easy-destructor $set-curl-easy-destructor!)
  (capi.curl-easy-cleanup easy))

(define (%set-curl-easy-destructor! struct destructor-func)
  (define who 'set-curl-easy-destructor!)
  (with-arguments-validation (who)
      ((curl-easy	struct)
       (procedure/false	destructor-func))
    ($set-curl-easy-destructor! struct destructor-func)))

;;; --------------------------------------------------------------------

(define (curl-easy-init)
  (let ((rv (capi.curl-easy-init)))
    (and rv (%make-curl-easy
		(pointer rv)
	      (owner? #t)))))

(define (curl-easy-cleanup easy)
  (define who 'curl-easy-cleanup)
  (with-arguments-validation (who)
      ((curl-easy	easy))
    (%unsafe.curl-easy-cleanup easy)))

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
    (cond ((<= CURLOPTTYPE_OFF_T option)
	   (with-arguments-validation (who)
	       ((off_t	parameter))
	     (capi.curl-easy-setopt easy option parameter)))
	  ((<= CURLOPTTYPE_FUNCTIONPOINT option)
	   (with-arguments-validation (who)
	       ((callback	parameter))
	     (capi.curl-easy-setopt easy option parameter)))
	  ((<= CURLOPTTYPE_OBJECTPOINT option)
	   (with-arguments-validation (who)
	       ((general-string/false	parameter))
	     (with-general-strings/false ((parameter^ parameter))
		 string->utf8
	       (capi.curl-easy-setopt easy option parameter^))))
	  ((<= CURLOPTTYPE_LONG option)
	   (if (boolean? parameter)
	       (capi.curl-easy-setopt easy option (if parameter 1 0))
	     (with-arguments-validation (who)
		 ((signed-long	parameter))
	       (capi.curl-easy-setopt easy option parameter))))
	  (else
	   (assertion-violation who
	     "invalid parameter type for selected option"
	     easy option parameter)))))

(define (curl-easy-getinfo easy info)
  (define who 'curl-easy-getinfo)
  (with-arguments-validation (who)
      ((curl-easy/alive	easy)
       (signed-int	info))
    (let ((rv (capi.curl-easy-getinfo easy info)))
      (if (pair? rv)
	  (let ((retval.type	(unsafe.car rv))
		(retval.value	(unsafe.cdr rv)))
	    (case-integers info
	      ((CURLINFO_CERTINFO)
	       (values CURLE_OK (and retval.value
				     (curl-certinfo.certinfo retval.value))))
	      ((CURLINFO_PRIVATE)
	       (values CURLE_OK retval.value))
	      (else
	       (case-integers retval.type
		 ((CURLINFO_SLIST)
		  (values CURLE_OK (and retval.value
					(let ((rv (curl-slist->list retval.value)))
					  (curl-slist-free-all retval.value)
					  rv))))
		 ((CURLINFO_DOUBLE CURLINFO_LONG)
		  (values CURLE_OK retval.value))
		 ((CURLINFO_STRING)
		  (values CURLE_OK (and retval.value (ascii->string retval.value))))
		 (else
		  (values CURLE_OK retval.value))))))
	(values rv #f)))))

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
      (and rv (%make-curl-easy
		  (pointer rv)
		(owner? #t))))))

(define (curl-easy-pause easy bitmask)
  (define who 'curl-easy-pause)
  (with-arguments-validation (who)
      ((curl-easy/alive	easy)
       (signed-int	bitmask))
    (capi.curl-easy-pause easy bitmask)))

;;; --------------------------------------------------------------------

(define curl-easy-recv
  (case-lambda
   ((easy buffer.data)
    (curl-easy-recv easy buffer.data #f))
   ((easy buffer.data buffer.len)
    (define who 'curl-easy-recv)
    (with-arguments-validation (who)
	((curl-easy/alive		easy)
	 (general-buffer		buffer.data)
	 (optional-buffer-length	buffer.len buffer.data))
      (let ((rv (capi.curl-easy-recv easy buffer.data buffer.len)))
	(if (pair? rv)
	    (values (unsafe.car rv) (unsafe.cdr rv))
	  (values rv #f)))))))

(define curl-easy-send
  (case-lambda
   ((easy buffer.data)
    (curl-easy-send easy buffer.data #f))
   ((easy buffer.data buffer.len)
    (define who 'curl-easy-send)
    (with-arguments-validation (who)
	((curl-easy/alive		easy)
	 (general-string		buffer.data)
	 (optional-buffer-length	buffer.len buffer.data))
      (with-general-strings ((buffer.data^ buffer.data))
	  string->utf8
	(let ((rv (capi.curl-easy-send easy buffer.data^ buffer.len)))
	  (if (pair? rv)
	      (values (unsafe.car rv) (unsafe.cdr rv))
	    (values rv #f))))))))

;;; --------------------------------------------------------------------

(define curl-easy-escape
  (case-lambda
   ((easy chars.data)
    (curl-easy-escape easy chars.data #f))
   ((easy chars.data chars.len)
    (define who 'curl-easy-escape)
    (with-arguments-validation (who)
	((curl-easy/alive		easy)
	 (general-string		chars.data)
	 (optional-buffer-length	chars.len chars.data))
      (with-general-strings ((chars.data^ chars.data))
	  string->utf8
	(capi.curl-easy-escape easy chars.data^ chars.len))))))

(define curl-easy-unescape
  (case-lambda
   ((easy chars.data)
    (curl-easy-unescape easy chars.data #f))
   ((easy chars.data chars.len)
    (define who 'curl-easy-unescape)
    (with-arguments-validation (who)
	((curl-easy/alive		easy)
	 (general-string		chars.data)
	 (optional-buffer-length	chars.len chars.data))
      (with-general-strings ((chars.data^ chars.data))
	  string->utf8
	(capi.curl-easy-unescape easy chars.data^ chars.len))))))

(define curl-easy-escape/string
  (case-lambda
   ((easy chars.data)
    (let ((rv (curl-easy-escape easy chars.data #f)))
      (and rv (ascii->string rv))))
   ((easy chars.data chars.len)
    (let ((rv (curl-easy-escape easy chars.data chars.len)))
      (and rv (ascii->string rv))))))

(define curl-easy-unescape/string
  (case-lambda
   ((easy chars.data)
    (let ((rv (curl-easy-unescape easy chars.data #f)))
      (and rv (ascii->string rv))))
   ((easy chars.data chars.len)
    (let ((rv (curl-easy-unescape easy chars.data chars.len)))
      (and rv (ascii->string rv))))))

;;; --------------------------------------------------------------------

(define (curl-easy-strerror code)
  (define who 'curl-easy-strerror)
  (with-arguments-validation (who)
      ((signed-int	code))
    (let ((rv (capi.curl-easy-strerror code)))
      (and rv (ascii->string rv)))))


;;;; multi API

(define-struct curl-multi
  (pointer
		;Pointer  object  referencing  an   instance  of  the  C
		;language type "CURLM".
   owner?
		;Boolean,  true   if  this   data  structure   owns  the
		;referenced "CURLM" instance.
   destructor
		;False or a user-supplied function to be called whenever
		;this instance  is finalised.  The function  must accept
		;at least one argument being the data structure itself.
   easies
		;Hash table of "curl-easy" instances.
   ))

(define (%struct-curl-multi-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[curl-multi")
  (%display " pointer=")	(%display ($curl-multi-pointer S))
  (%display " owner?=")		(%display ($curl-multi-owner?  S))
  (%display "]"))

;;; --------------------------------------------------------------------

(define-syntax %make-curl-multi
  (syntax-rules (pointer owner?)
    ((_ (pointer ?pointer) (owner? ?owner?))
     (make-curl-multi ?pointer ?owner? #f (make-eq-hashtable)))
    ((_ ?pointer ?owner?)
     (make-curl-multi ?pointer ?owner? #f (make-eq-hashtable)))))

(define (curl-multi?/alive obj)
  (and (curl-multi? obj)
       (not (pointer-null? ($curl-multi-pointer obj)))))

(define (%unsafe.curl-multi-cleanup multi)
  (struct-destructor-application multi $curl-multi-destructor $set-curl-multi-destructor!)
  (let ((easies ($curl-multi-easies multi)))
    (vector-for-each (lambda (easy)
		       (capi.curl-multi-remove-handle multi easy)
		       (hashtable-delete! easies easy))
      (hashtable-keys easies)))
  (capi.curl-multi-cleanup multi))

(define (%set-curl-multi-destructor! struct destructor-func)
  (define who 'set-curl-multi-destructor!)
  (with-arguments-validation (who)
      ((curl-multi	struct)
       (procedure/false	destructor-func))
    ($set-curl-multi-destructor! struct destructor-func)))

;;; --------------------------------------------------------------------

(define (curl-multi-init)
  (let ((rv (capi.curl-multi-init)))
    (and rv (%make-curl-multi
		(pointer rv)
	      (owner? #t)))))

(define (curl-multi-cleanup multi)
  (define who 'curl-multi-cleanup)
  (with-arguments-validation (who)
      ((curl-multi	multi))
    (%unsafe.curl-multi-cleanup multi)))

;;; --------------------------------------------------------------------

(define (curl-multi-add-handle multi easy)
  (define who 'curl-multi-add-handle)
  (with-arguments-validation (who)
      ((curl-multi/alive	multi)
       (curl-easy		easy))
    (let ((easies ($curl-multi-easies multi)))
      (if (hashtable-contains? easies easy)
	  CURLM_OK
	(let ((rv (capi.curl-multi-add-handle multi easy)))
	  (when (= rv CURLM_OK)
	    (hashtable-set! easies easy easy))
	  rv)))))

(define (curl-multi-remove-handle multi easy)
  (define who 'curl-multi-remove-handle)
  (with-arguments-validation (who)
      ((curl-multi/alive	multi)
       (curl-easy		easy))
    (let ((easies ($curl-multi-easies multi)))
      (if (hashtable-contains? easies easy)
	  (let ((rv (capi.curl-multi-remove-handle multi easy)))
	    (when (= rv CURLM_OK)
	      (hashtable-delete! easies easy)
	      rv))
	CURLM_OK))))

(define (%curl-multi-easies multi)
  ;;This function  name is prefixed  with %  to avoid conflict  with the
  ;;accessor for the "easies" field of the CURL-MULTI data structure.
  ;;
  (define who 'curl-multi-easies)
  (with-arguments-validation (who)
      ((curl-multi/alive	multi))
    (hashtable-keys ($curl-multi-easies multi))))

;;; --------------------------------------------------------------------

(define (curl-multi-setopt multi option parameter)
  (define who 'curl-multi-setopt)
  (with-arguments-validation (who)
      ((curl-multi/alive	multi)
       (signed-int		option))
    (cond ((<= CURLOPTTYPE_OFF_T option)
	   (with-arguments-validation (who)
	       ((off_t	parameter))
	     (capi.curl-multi-setopt multi option parameter)))
	  ((<= CURLOPTTYPE_FUNCTIONPOINT option)
	   (with-arguments-validation (who)
	       ((callback	parameter))
	     (capi.curl-multi-setopt multi option parameter)))
	  ((<= CURLOPTTYPE_OBJECTPOINT option)
	   (with-arguments-validation (who)
	       ((general-string/false	parameter))
	     (with-general-strings/false ((parameter^ parameter))
		 string->utf8
	       (capi.curl-multi-setopt multi option parameter^))))
	  ((<= CURLOPTTYPE_LONG option)
	   (if (boolean? parameter)
	       (capi.curl-multi-setopt multi option (if parameter 1 0))
	     (with-arguments-validation (who)
		 ((signed-long	parameter))
	       (capi.curl-multi-setopt multi option parameter))))
	  (else
	   (assertion-violation who
	     "invalid parameter type for selected option"
	     multi option parameter)))))

;;; --------------------------------------------------------------------

(define (curl-multi-fdset . args)
  (define who 'curl-multi-fdset)
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

;;; --------------------------------------------------------------------

(define (curl-multi-perform multi)
  (define who 'curl-multi-perform)
  (with-arguments-validation (who)
      ((curl-multi/alive	multi))
    (let ((rv (capi.curl-multi-perform multi)))
      (values (unsafe.car rv)
	      (unsafe.cdr rv)))))

;;; --------------------------------------------------------------------

(define (curl-multi-timeout . args)
  (define who 'curl-multi-timeout)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

(define (curl-multi-assign . args)
  (define who 'curl-multi-assign)
  (with-arguments-validation (who)
      ()
    (unimplemented who)))

;;; --------------------------------------------------------------------

(define (curl-multi-info-read multi)
  (define who 'curl-multi-info-read)
  (with-arguments-validation (who)
      ((curl-multi/alive	multi))
    (let ((rv (capi.curl-multi-info-read multi)))
      (values (unsafe.car rv) (unsafe.cdr rv)))))

(define (curl-multi-strerror code)
  (define who 'curl-multi-strerror)
  (with-arguments-validation (who)
      ((signed-int	code))
    (let ((rv (capi.curl-multi-strerror code)))
      (and rv (ascii->string rv)))))


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
    (with-general-strings ((date^ date))
	string->ascii
      (capi.curl-getdate date^))))

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_sockaddr"

(%define-raw-struct-accessor curl-sockaddr.family	capi.curl-sockaddr.family)
(%define-raw-struct-accessor curl-sockaddr.socktype	capi.curl-sockaddr.socktype)
(%define-raw-struct-accessor curl-sockaddr.protocol	capi.curl-sockaddr.protocol)
(%define-raw-struct-accessor curl-sockaddr.addrlen	capi.curl-sockaddr.addrlen)
(%define-raw-struct-accessor curl-sockaddr.addr		capi.curl-sockaddr.addr)

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_fileinfo"

(define-struct curl-fileinfo
  (filename
   filetype
   time
   perm
   uid
   gid
   size
   hardlinks
   strings.time
   strings.perm
   strings.user
   strings.group
   strings.target
   flags))

(define (curl-fileinfo->struct stru)
  (define-inline (%accessor->string ?accessor)
    (let ((o (?accessor stru)))
      (and o (cstring->string o))))
  (make-curl-fileinfo
   (%accessor->string curl-fileinfo.filename)
   (curl-fileinfo.filetype	stru)
   (curl-fileinfo.time		stru)
   (curl-fileinfo.perm		stru)
   (curl-fileinfo.uid		stru)
   (curl-fileinfo.gid		stru)
   (curl-fileinfo.size		stru)
   (curl-fileinfo.hardlinks	stru)
   (%accessor->string curl-fileinfo.strings.time)
   (%accessor->string curl-fileinfo.strings.perm)
   (%accessor->string curl-fileinfo.strings.user)
   (%accessor->string curl-fileinfo.strings.group)
   (%accessor->string curl-fileinfo.strings.target)
   (curl-fileinfo.flags		stru)))

(%define-raw-struct-accessor curl-fileinfo.filename	capi.curl-fileinfo.filename)
(%define-raw-struct-accessor curl-fileinfo.filetype	capi.curl-fileinfo.filetype)
(%define-raw-struct-accessor curl-fileinfo.time		capi.curl-fileinfo.time)
(%define-raw-struct-accessor curl-fileinfo.perm		capi.curl-fileinfo.perm)
(%define-raw-struct-accessor curl-fileinfo.uid		capi.curl-fileinfo.uid)
(%define-raw-struct-accessor curl-fileinfo.gid		capi.curl-fileinfo.gid)
(%define-raw-struct-accessor curl-fileinfo.size		capi.curl-fileinfo.size)
(%define-raw-struct-accessor curl-fileinfo.hardlinks	capi.curl-fileinfo.hardlinks)
(%define-raw-struct-accessor curl-fileinfo.strings.time	capi.curl-fileinfo.strings.time)
(%define-raw-struct-accessor curl-fileinfo.strings.perm	capi.curl-fileinfo.strings.perm)
(%define-raw-struct-accessor curl-fileinfo.strings.user	capi.curl-fileinfo.strings.user)
(%define-raw-struct-accessor curl-fileinfo.strings.group capi.curl-fileinfo.strings.group)
(%define-raw-struct-accessor curl-fileinfo.strings.target capi.curl-fileinfo.strings.target)
(%define-raw-struct-accessor curl-fileinfo.flags	capi.curl-fileinfo.flags)

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_khkey"

(define (curl-khkey.key stru)
  (define who 'curl-khkey.key)
  (with-arguments-validation (who)
      ((pointer		stru))
    (let ((rv (capi.curl-khkey.key stru)))
      (values (unsafe.car rv) (unsafe.cdr rv)))))

;;; --------------------------------------------------------------------
;;; accessors and mutators for "struct curl_forms" arrays

(define (curl-forms-sizeof-array number-of-structs)
  (define who 'curl-forms-sizeof-array)
  (with-arguments-validation (who)
      ((non-negative-fixnum	number-of-structs))
    (capi.curl-forms-sizeof-array number-of-structs)))

(define (curl-forms.option pointer index)
  (define who 'curl-forms.option)
  (with-arguments-validation (who)
      ((general-data		pointer)
       (non-negative-fixnum	index))
    (capi.curl-forms.option pointer index)))

(define (curl-forms.value pointer index)
  (define who 'curl-forms.value)
  (with-arguments-validation (who)
      ((general-data		pointer)
       (non-negative-fixnum	index))
    (capi.curl-forms.value pointer index)))

(define (curl-forms.option-set! pointer index value)
  (define who 'curl-forms.option-set!)
  (with-arguments-validation (who)
      ((general-data		pointer)
       (non-negative-fixnum	index)
       (signed-int		value))
    (let ((rv (capi.curl-forms.option-set! pointer index value)))
      (and rv (ascii->string rv)))))

(define (curl-forms.value-set! pointer index value)
  (define who 'curl-forms.value-set!)
  (with-arguments-validation (who)
      ((general-data		pointer)
       (non-negative-fixnum	index)
       (pointer/memory-block	value))
    (capi.curl-forms.value-set! pointer index value)))

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_certinfo"

(define (curl-certinfo.certinfo pointer)
  (define who 'curl-certinfo.certinfo)
  (with-arguments-validation (who)
      ((pointer		pointer))
    (let ((rv (capi.curl-certinfo.certinfo pointer)))
      (vector-map (lambda (slist)
		    (curl-slist->list slist))
	rv))))

;;; --------------------------------------------------------------------
;;; accessors for "struct CURLMsg"

(%define-raw-struct-accessor curl-msg.msg		capi.curl-msg.msg)
(%define-raw-struct-accessor curl-msg.data.whatever	capi.curl-msg.data.whatever)
(%define-raw-struct-accessor curl-msg.data.result	capi.curl-msg.data.result)

(define (curl-msg.easy_handle stru)
  (define who 'curl-msg.easy_handle)
  (with-arguments-validation (who)
      ((pointer	stru))
    (%make-curl-easy
	(pointer (capi.curl-msg.easy_handle stru))
      (owner? #f))))


;;;; callback makers: easy API

(define-syntax %cdata
  (syntax-rules ()
    ((_ ?p)
     (if (pointer-null? ?p)
	 #f
       ?p))))

;;; --------------------------------------------------------------------

(define make-curl-write-callback
  ;; size_t curl_write_callback (char *buffer, size_t size, size_t nitems, void *outstream)
  (let ((maker (ffi.make-c-callback-maker 'size_t '(pointer size_t size_t pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (buffer size nitems outstream)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback buffer size nitems
				       (%cdata outstream))))))))

(define make-curl-read-callback
  ;; size_t curl_read_callback (char *buffer, size_t size, size_t nitems, void *instream)
  (let ((maker (ffi.make-c-callback-maker 'size_t '(pointer size_t size_t pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (buffer size nitems instream)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_READFUNC_ABORT))
		 (user-scheme-callback buffer size nitems
				       (%cdata instream))))))))

(define make-curl-ioctl-callback
  ;; curlioerr curl_ioctl_callback (CURL *handle, int cmd, void *clientp)
  (let ((maker (ffi.make-c-callback-maker 'signed-int '(pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle cmd custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURLIOE_UNKNOWNCMD))
		 (user-scheme-callback (%make-curl-easy
					   (pointer handle)
					 (owner? #f))
				       cmd
				       (%cdata custom-data))))))))

(define make-curl-seek-callback
  ;; int curl_seek_callback (void *instream, curl_off_t offset, int origin)
  (let ((maker (ffi.make-c-callback-maker 'signed-int '(pointer off_t signed-int))))
    (lambda (user-scheme-callback)
      (maker (lambda (instream offset origin)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_SEEKFUNC_FAIL))
		 (user-scheme-callback (%cdata instream)
				       offset origin)))))))

(define make-curl-socket-option-callback
  ;; int curl_sockopt_callback (void *clientp, curl_socket_t curlfd, curlsocktype purpose)
  (let ((maker (ffi.make-c-callback-maker 'signed-int '(pointer signed-int signed-int))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data curlfd purpose)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_SOCKOPT_ERROR))
		 (user-scheme-callback (%cdata custom-data) curlfd purpose)))))))

(define make-curl-open-socket-callback
  ;; curl_socket_t curl_opensocket_callback (void *clientp, curlsocktype purpose,
  ;;                                         struct curl_sockaddr *address)
  (let ((maker (ffi.make-c-callback-maker 'signed-int '(pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data purpose address)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_SOCKET_BAD))
		 (user-scheme-callback (%cdata custom-data) purpose address)))))))

(define make-curl-close-socket-callback
  ;; int curl_close-socket_callback (void *clientp, curl_socket_t item)
  (let ((maker (ffi.make-c-callback-maker 'signed-int '(pointer signed-int))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data item)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  1))
		 (user-scheme-callback (%cdata custom-data) item)))))))

(define make-curl-progress-callback
  ;; int curl_progress_callback (void *clientp,
  ;;                             double dltotal, double dlnow,
  ;;                             double ultotal, double ulnow)
  (let ((maker (ffi.make-c-callback-maker 'signed-int '(pointer double double double double))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data dltotal dlnow ultotal ulnow)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (if (user-scheme-callback (%cdata custom-data)
					   dltotal dlnow ultotal ulnow)
		     1
		   0)))))))

(define make-curl-header-callback
  ;; size_t (noproto) (void *ptr, size_t size, size_t nmemb, void *userdata)
  (let ((maker (ffi.make-c-callback-maker 'size_t '(pointer size_t size_t pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (header-buffer size nmemb custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_SOCKET_BAD))
		 (user-scheme-callback header-buffer size nmemb
				       (%cdata custom-data))))))))

(define make-curl-debug-callback
  ;; int curl_debug_callback (CURL *handle, curl_infotype type, char *data,
  ;;                          size_t size, void *userptr)
  (let ((maker (ffi.make-c-callback-maker 'signed-int
					  '(pointer signed-int pointer size_t pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle type data size custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback (%make-curl-easy
					   (pointer handle)
					 (owner? #f))
				       type data size
				       (%cdata custom-data))))))))

(define make-curl-ssl-ctx-callback
  ;; CURLcode curl_ssl_ctx_callback (CURL *curl, void *ssl_ctx, void *userptr)
  (let ((maker (ffi.make-c-callback-maker 'signed-int '(pointer pointer pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle ssl-ctx custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURLE_ABORTED_BY_CALLBACK))
		 (user-scheme-callback (%make-curl-easy
					   (pointer handle)
					 (owner? #f))
				       ssl-ctx (%cdata custom-data))))))))

;;; --------------------------------------------------------------------

(define make-curl-conv-to-network-callback
  ;; CURLcode callback (void * buffer, size_t size)
  (let ((maker (ffi.make-c-callback-maker 'signed-int '(pointer size_t))))
    (lambda (user-scheme-callback)
      (maker (lambda (buffer size)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURLE_ABORTED_BY_CALLBACK))
		 (user-scheme-callback buffer size)))))))

(define make-curl-conv-from-network-callback
  make-curl-conv-to-network-callback)

(define make-curl-conv-from-utf8-callback
  make-curl-conv-to-network-callback)

;;; --------------------------------------------------------------------

(define make-curl-interleave-callback
  ;; size_t callback (void *ptr, size_t size, size_t nmemb, void * userdata)
  (let ((maker (ffi.make-c-callback-maker 'size_t '(pointer size_t size_t pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (buffer size nmemb custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback buffer size nmemb (%cdata custom-data))))))))

(define make-curl-chunk-begin-callback
  ;; long curl_chunk_bgn_callback (const void *transfer_info, void *ptr, int remains)
  (let ((maker (ffi.make-c-callback-maker 'signed-long '(pointer pointer signed-int))))
    (lambda (user-scheme-callback)
      (maker (lambda (transfer-info custom-data remains)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_CHUNK_BGN_FUNC_FAIL))
		 (user-scheme-callback transfer-info (%cdata custom-data)
				       remains)))))))

(define make-curl-chunk-end-callback
  ;; long curl_chunk_end_callback (void *ptr)
  (let ((maker (ffi.make-c-callback-maker 'signed-long '(pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_CHUNK_END_FUNC_FAIL))
		 (user-scheme-callback (%cdata custom-data))))))))

(define make-curl-fnmatch-callback
  ;; int curl_fnmatch_callback (void *ptr, const char *pattern, const char *string)
  (let ((maker (ffi.make-c-callback-maker 'signed-int '(pointer pointer pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data pattern string)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_FNMATCHFUNC_FAIL))
		 (user-scheme-callback (%cdata custom-data) pattern string)))))))

(define make-curl-sshkey-callback
  ;; int curl_sshkeycallback (CURL *easy, const struct curl_khkey *knownkey,
  ;;                          const struct curl_khkey *foundkey, enum curl_khmatch,
  ;;                          void *clientp);
  (let ((maker (ffi.make-c-callback-maker 'signed-int
					  '(pointer pointer pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle knownkey foundkey khmatch custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURLKHSTAT_REJECT))
		 (user-scheme-callback (%make-curl-easy
					   (pointer handle)
					 (owner? #f))
				       (%cdata knownkey) foundkey khmatch
				       (%cdata custom-data))))))))


;;;; callback makers: multi API

(define make-curl-socket-callback
  ;; int curl_socket_callback (CURL *easy, curl_socket_t s, int what, void *userp,
  ;;                           void *socketp)
  (let ((maker (ffi.make-c-callback-maker 'signed-int
					  '(pointer signed-int signed-int pointer pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle sock what callback-custom-data socket-custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback (%make-curl-easy
					   (pointer handle)
					 (owner? #f))
				       sock what
				       (%cdata callback-custom-data)
				       (%cdata socket-custom-data))
		 0))))))

(define make-curl-multi-timer-callback
  ;; int curl_multi_timer_callback (CURLM *multi, long timeout_ms, void *userp)
  (let ((maker (ffi.make-c-callback-maker 'signed-int '(pointer signed-long pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle timeout-ms custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback (%make-curl-multi
					   (pointer handle)
					 (owner? #f))
				       timeout-ms (%cdata custom-data))))))))


;;;; constants to symbols

(define-exact-integer->symbol-function curlmsg->symbol
  (CURLMSG_NONE
   CURLMSG_DONE
   CURLMSG_LAST))


;;;; done

(set-rtd-printer! (type-descriptor curl-version-info-data)
		  %struct-curl-version-info-data-printer)
(set-rtd-printer! (type-descriptor curl-form-data)	%struct-curl-form-data-printer)
(set-rtd-printer! (type-descriptor curl-share)		%struct-curl-share-printer)
(set-rtd-printer! (type-descriptor curl-easy)		%struct-curl-easy-printer)
(set-rtd-printer! (type-descriptor curl-multi)		%struct-curl-multi-printer)

(set-rtd-destructor! (type-descriptor curl-form-data)	%unsafe.curl-formfree)
(set-rtd-destructor! (type-descriptor curl-share)	%unsafe.curl-share-cleanup)
(set-rtd-destructor! (type-descriptor curl-easy)	%unsafe.curl-easy-cleanup)
(set-rtd-destructor! (type-descriptor curl-multi)	%unsafe.curl-multi-cleanup)

)

;;; end of file
;;Local Variables:
;;eval: (put '%make-curl-easy 'scheme-indent-function 1)
;;eval: (put '%make-curl-multi 'scheme-indent-function 1)
;;eval: (put 'with-general-strings 'scheme-indent-function 2)
;;eval: (put 'with-general-strings/false 'scheme-indent-function 2)
;;eval: (put '%define-raw-struct-accessor 'scheme-indent-function 1)
;;End:
