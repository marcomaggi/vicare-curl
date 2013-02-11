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
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    curl-constant-httppost->symbol
    curl-constant-filetype->symbol
    curl-constant-infoflag->symbol
    curl-constant-chunk-bgn->symbol
    curl-constant-chunk-end->symbol
    curl-constant-fnmatch->symbol
    curl-constant-seekfunc->symbol
    curl-constant-readfunc->symbol
    curl-constant-socktype->symbol
    curl-constant-sockopt->symbol
    curl-constant-ioe->symbol
    curl-constant-iocmd->symbol
    curl-constant-info-debug->symbol
    curl-constant-e->symbol
    curl-constant-proxy->symbol
    curl-constant-auth->symbol
    curl-constant-ssh->symbol
    curl-constant-gssapi-delegation->symbol
    curl-constant-khtype->symbol
    curl-constant-khstat->symbol
    curl-constant-khmatch->symbol
    curl-constant-use-ssl->symbol
    curl-constant-ftp-ssl-ccc->symbol
    curl-constant-ftp-auth->symbol
    curl-constant-ftp-create-dir->symbol
    curl-constant-ftp-method->symbol
    curl-constant-proto->symbol
    curl-constant-opt-type->symbol
    curl-constant-opt->symbol
    curl-constant-ip-resolve->symbol
    curl-constant-http-version->symbol
    curl-constant-rts-preq->symbol
    curl-constant-netrc->symbol
    curl-constant-ssl-version->symbol
    curl-constant-tls-auth->symbol
    curl-constant-redir->symbol
    curl-constant-time-cond->symbol
    curl-constant-form->symbol
    curl-constant-form-add->symbol
    curl-constant-info-type->symbol
    curl-constant-info->symbol
    curl-constant-close-policy->symbol
    curl-constant-global->symbol
    curl-constant-lock-data->symbol
    curl-constant-lock-access->symbol
    curl-constant-she->symbol
    curl-constant-shopt->symbol
    curl-constant-version-num->symbol
    curl-constant-version->symbol
    curl-constant-pause->symbol
    curl-constant-m->symbol
    curl-constant-msg->symbol
    curl-constant-poll->symbol
    curl-constant-cselect->symbol
    curl-constant-mopt->symbol)
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

(define-argument-validation (general-buffer/false who obj)
  (or (not obj)
      (bytevector? obj)
      (pointer? obj)
      (memory-block? obj))
  (assertion-violation who
    "expected false or bytevector or pointer or memory-block as general buffer argument"
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (general-sticky-buffer who obj)
  ;;A general  "sticky" buffer is  a block of  memory that is  NOT moved
  ;;around by the garbage collector.
  ;;
  (or (pointer? obj)
      (memory-block? obj))
  (assertion-violation who
    "expected pointer or memory-block as general sticky buffer argument" obj))

(define-argument-validation (general-sticky-buffer/false who obj)
  (or (not obj)
      (pointer? obj)
      (memory-block? obj))
  (assertion-violation who
    "expected false or pointer or memory-block as general sticky buffer argument"
    obj))

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

(define-argument-validation (file-descriptor who obj)
  (and (fixnum? obj)
       (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as file descriptor argument" obj))

(define-argument-validation (action-socket-descriptor who obj)
  (and (fixnum? obj)
       (or (unsafe.fx= obj CURL_SOCKET_TIMEOUT)
	   (unsafe.fx<= 0 obj)))
  (assertion-violation who
    "expected CURL_SOCKET_TIMEOUT or non-negative fixnum as socket descriptor argument"
    obj))

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

(define (curl-multi-fdset multi read-fds write-fds exc-fds)
  (define who 'curl-multi-fdset)
  (with-arguments-validation (who)
      ((curl-multi/alive	multi)
       (general-sticky-buffer/false	read-fds)
       (general-sticky-buffer/false	write-fds)
       (general-sticky-buffer/false	exc-fds))
    (let ((rv (capi.curl-multi-fdset multi read-fds write-fds exc-fds)))
      (values (unsafe.car rv)
	      (unsafe.cdr rv)))))

(define curl-multi-socket-action
  (case-lambda
   ((multi sock-fd)
    (curl-multi-socket-action multi sock-fd 0))
   ((multi sock-fd ev-bitmask)
    (define who 'curl-multi-socket-action)
    (with-arguments-validation (who)
	((curl-multi/alive		multi)
	 (action-socket-descriptor	sock-fd)
	 (signed-int			ev-bitmask))
      (let ((rv (capi.curl-multi-socket-action multi sock-fd ev-bitmask)))
	(values (unsafe.car rv)
		(unsafe.cdr rv)))))))

(define (curl-multi-socket multi sock-fd)
  ;;This is deprecated.
  ;;
  (define who 'curl-multi-socket)
  (with-arguments-validation (who)
      ((curl-multi/alive	multi)
       (file-descriptor		sock-fd))
    (let ((rv (capi.curl-multi-socket multi sock-fd)))
      (values (unsafe.car rv)
	      (unsafe.cdr rv)))))

(define (curl-multi-socket-all multi)
  ;;This is deprecated.
  ;;
  (define who 'curl-multi-socket-all)
  (with-arguments-validation (who)
      ((curl-multi/alive	multi))
    (capi.curl-multi-socket-all multi)))

;;; --------------------------------------------------------------------

(define (curl-multi-perform multi)
  (define who 'curl-multi-perform)
  (with-arguments-validation (who)
      ((curl-multi/alive	multi))
    (let ((rv (capi.curl-multi-perform multi)))
      (values (unsafe.car rv)
	      (unsafe.cdr rv)))))

;;; --------------------------------------------------------------------

(define (curl-multi-timeout multi)
  (define who 'curl-multi-timeout)
  (with-arguments-validation (who)
      ((curl-multi/alive	multi))
    (let ((rv (capi.curl-multi-timeout multi)))
      (if (pair? rv)
	  (values (unsafe.car rv)
		  (unsafe.cdr rv))
	(values rv #f)))))

(define (curl-multi-assign multi sock custom-data)
  (define who 'curl-multi-assign)
  (with-arguments-validation (who)
      ((curl-multi/alive	multi)
       (file-descriptor		sock)
       (pointer/false		custom-data))
    (capi.curl-multi-assign multi sock custom-data)))

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

(define-exact-integer->symbol-function curl-constant-httppost->symbol
  (HTTPPOST_FILENAME
   HTTPPOST_READFILE
   HTTPPOST_PTRNAME
   HTTPPOST_PTRCONTENTS
   HTTPPOST_BUFFER
   HTTPPOST_PTRBUFFER
   HTTPPOST_CALLBACK))

(define-exact-integer->symbol-function curl-constant-filetype->symbol
  (CURLFILETYPE_FILE
   CURLFILETYPE_DIRECTORY
   CURLFILETYPE_SYMLINK
   CURLFILETYPE_DEVICE_BLOCK
   CURLFILETYPE_DEVICE_CHAR
   CURLFILETYPE_NAMEDPIPE
   CURLFILETYPE_SOCKET
   CURLFILETYPE_DOOR
   CURLFILETYPE_UNKNOWN))

(define-exact-integer->symbol-function curl-constant-infoflag->symbol
  (CURLFINFOFLAG_KNOWN_FILENAME
   CURLFINFOFLAG_KNOWN_FILETYPE
   CURLFINFOFLAG_KNOWN_TIME
   CURLFINFOFLAG_KNOWN_PERM
   CURLFINFOFLAG_KNOWN_UID
   CURLFINFOFLAG_KNOWN_GID
   CURLFINFOFLAG_KNOWN_SIZE
   CURLFINFOFLAG_KNOWN_HLINKCOUNT))

(define-exact-integer->symbol-function curl-constant-chunk-bgn->symbol
  (CURL_CHUNK_BGN_FUNC_OK
   CURL_CHUNK_BGN_FUNC_FAIL
   CURL_CHUNK_BGN_FUNC_SKIP))

(define-exact-integer->symbol-function curl-constant-chunk-end->symbol
  (CURL_CHUNK_END_FUNC_OK
   CURL_CHUNK_END_FUNC_FAIL))

(define-exact-integer->symbol-function curl-constant-fnmatch->symbol
  (CURL_FNMATCHFUNC_MATCH
   CURL_FNMATCHFUNC_NOMATCH
   CURL_FNMATCHFUNC_FAIL))

(define-exact-integer->symbol-function curl-constant-seekfunc->symbol
  (CURL_SEEKFUNC_OK
   CURL_SEEKFUNC_FAIL
   CURL_SEEKFUNC_CANTSEEK))

(define-exact-integer->symbol-function curl-constant-readfunc->symbol
  (CURL_READFUNC_ABORT
   CURL_READFUNC_PAUSE))

(define-exact-integer->symbol-function curl-constant-socktype->symbol
  (CURLSOCKTYPE_IPCXN
   CURLSOCKTYPE_AGENT
   CURLSOCKTYPE_LAST))

(define-exact-integer->symbol-function curl-constant-sockopt->symbol
  (CURL_SOCKOPT_OK
   CURL_SOCKOPT_ERROR
   CURL_SOCKOPT_ALREADY_CONNECTED))

(define-exact-integer->symbol-function curl-constant-ioe->symbol
  (CURLIOE_OK
   CURLIOE_UNKNOWNCMD
   CURLIOE_FAILRESTART
   CURLIOE_LAST))

(define-exact-integer->symbol-function curl-constant-iocmd->symbol
  (CURLIOCMD_NOP
   CURLIOCMD_RESTARTREAD
   CURLIOCMD_LAST))

(define-exact-integer->symbol-function curl-constant-info-debug->symbol
  (CURLINFO_TEXT
   CURLINFO_HEADER_IN
   CURLINFO_HEADER_OUT
   CURLINFO_DATA_IN
   CURLINFO_DATA_OUT
   CURLINFO_SSL_DATA_IN
   CURLINFO_SSL_DATA_OUT
   CURLINFO_END))

(define-exact-integer->symbol-function curl-constant-e->symbol
  (CURLE_OK
   CURLE_UNSUPPORTED_PROTOCOL
   CURLE_FAILED_INIT
   CURLE_URL_MALFORMAT
   CURLE_NOT_BUILT_IN
   CURLE_COULDNT_RESOLVE_PROXY
   CURLE_COULDNT_RESOLVE_HOST
   CURLE_COULDNT_CONNECT
   CURLE_FTP_WEIRD_SERVER_REPLY
   CURLE_REMOTE_ACCESS_DENIED
   CURLE_FTP_ACCEPT_FAILED
   CURLE_FTP_WEIRD_PASS_REPLY
   CURLE_FTP_ACCEPT_TIMEOUT
   CURLE_FTP_WEIRD_PASV_REPLY
   CURLE_FTP_WEIRD_227_FORMAT
   CURLE_FTP_CANT_GET_HOST
   CURLE_OBSOLETE16
   CURLE_FTP_COULDNT_SET_TYPE
   CURLE_PARTIAL_FILE
   CURLE_FTP_COULDNT_RETR_FILE
   CURLE_OBSOLETE20
   CURLE_QUOTE_ERROR
   CURLE_HTTP_RETURNED_ERROR
   CURLE_WRITE_ERROR
   CURLE_OBSOLETE24
   CURLE_UPLOAD_FAILED
   CURLE_READ_ERROR
   CURLE_OUT_OF_MEMORY
   CURLE_OPERATION_TIMEDOUT
   CURLE_OBSOLETE29
   CURLE_FTP_PORT_FAILED
   CURLE_FTP_COULDNT_USE_REST
   CURLE_OBSOLETE32
   CURLE_RANGE_ERROR
   CURLE_HTTP_POST_ERROR
   CURLE_SSL_CONNECT_ERROR
   CURLE_BAD_DOWNLOAD_RESUME
   CURLE_FILE_COULDNT_READ_FILE
   CURLE_LDAP_CANNOT_BIND
   CURLE_LDAP_SEARCH_FAILED
   CURLE_OBSOLETE40
   CURLE_FUNCTION_NOT_FOUND
   CURLE_ABORTED_BY_CALLBACK
   CURLE_BAD_FUNCTION_ARGUMENT
   CURLE_OBSOLETE44
   CURLE_INTERFACE_FAILED
   CURLE_OBSOLETE46
   CURLE_TOO_MANY_REDIRECTS
   CURLE_UNKNOWN_OPTION
   CURLE_TELNET_OPTION_SYNTAX
   CURLE_OBSOLETE50
   CURLE_PEER_FAILED_VERIFICATION
   CURLE_GOT_NOTHING
   CURLE_SSL_ENGINE_NOTFOUND
   CURLE_SSL_ENGINE_SETFAILED
   CURLE_SEND_ERROR
   CURLE_RECV_ERROR
   CURLE_OBSOLETE57
   CURLE_SSL_CERTPROBLEM
   CURLE_SSL_CIPHER
   CURLE_SSL_CACERT
   CURLE_BAD_CONTENT_ENCODING
   CURLE_LDAP_INVALID_URL
   CURLE_FILESIZE_EXCEEDED
   CURLE_USE_SSL_FAILED
   CURLE_SEND_FAIL_REWIND
   CURLE_SSL_ENGINE_INITFAILED
   CURLE_LOGIN_DENIED
   CURLE_TFTP_NOTFOUND
   CURLE_TFTP_PERM
   CURLE_REMOTE_DISK_FULL
   CURLE_TFTP_ILLEGAL
   CURLE_TFTP_UNKNOWNID
   CURLE_REMOTE_FILE_EXISTS
   CURLE_TFTP_NOSUCHUSER
   CURLE_CONV_FAILED
   CURLE_CONV_REQD
   CURLE_SSL_CACERT_BADFILE
   CURLE_REMOTE_FILE_NOT_FOUND
   CURLE_SSH
   CURLE_SSL_SHUTDOWN_FAILED
   CURLE_AGAIN
   CURLE_SSL_CRL_BADFILE
   CURLE_SSL_ISSUER_ERROR
   CURLE_FTP_PRET_FAILED
   CURLE_RTSP_CSEQ_ERROR
   CURLE_RTSP_SESSION_ERROR
   CURLE_FTP_BAD_FILE_LIST
   CURLE_CHUNK_FAILED
   CURL_LAST))

(define-exact-integer->symbol-function curl-constant-proxy->symbol
  (CURLPROXY_HTTP
   CURLPROXY_HTTP_1_0
   CURLPROXY_SOCKS4
   CURLPROXY_SOCKS5
   CURLPROXY_SOCKS4A
   CURLPROXY_SOCKS5_HOSTNAME))

(define-exact-integer->symbol-function curl-constant-auth->symbol
  (CURLAUTH_NONE
   CURLAUTH_BASIC
   CURLAUTH_DIGEST
   CURLAUTH_GSSNEGOTIATE
   CURLAUTH_NTLM
   CURLAUTH_DIGEST_IE
   CURLAUTH_NTLM_WB
   CURLAUTH_ONLY
   CURLAUTH_ANY
   CURLAUTH_ANYSAFE))

(define-exact-integer->symbol-function curl-constant-ssh->symbol
  (CURLSSH_AUTH_ANY
   CURLSSH_AUTH_NONE
   CURLSSH_AUTH_PUBLICKEY
   CURLSSH_AUTH_PASSWORD
   CURLSSH_AUTH_HOST
   CURLSSH_AUTH_KEYBOARD
   CURLSSH_AUTH_AGENT
   CURLSSH_AUTH_DEFAULT))

(define-exact-integer->symbol-function curl-constant-gssapi-delegation->symbol
  (CURLGSSAPI_DELEGATION_NONE
   CURLGSSAPI_DELEGATION_POLICY_FLAG
   CURLGSSAPI_DELEGATION_FLAG))

(define-exact-integer->symbol-function curl-constant-khtype->symbol
  (CURLKHTYPE_UNKNOWN
   CURLKHTYPE_RSA1
   CURLKHTYPE_RSA
   CURLKHTYPE_DSS))

(define-exact-integer->symbol-function curl-constant-khstat->symbol
  (CURLKHSTAT_FINE_ADD_TO_FILE
   CURLKHSTAT_FINE
   CURLKHSTAT_REJECT
   CURLKHSTAT_DEFER
   CURLKHSTAT_LAST))

(define-exact-integer->symbol-function curl-constant-khmatch->symbol
  (CURLKHMATCH_OK
   CURLKHMATCH_MISMATCH
   CURLKHMATCH_MISSING
   CURLKHMATCH_LAST))

(define-exact-integer->symbol-function curl-constant-use-ssl->symbol
  (CURLUSESSL_NONE
   CURLUSESSL_TRY
   CURLUSESSL_CONTROL
   CURLUSESSL_ALL
   CURLUSESSL_LAST))

(define-exact-integer->symbol-function curl-constant-ftp-ssl-ccc->symbol
  (CURLFTPSSL_CCC_NONE
   CURLFTPSSL_CCC_PASSIVE
   CURLFTPSSL_CCC_ACTIVE
   CURLFTPSSL_CCC_LAST))

(define-exact-integer->symbol-function curl-constant-ftp-auth->symbol
  (CURLFTPAUTH_DEFAULT
   CURLFTPAUTH_SSL
   CURLFTPAUTH_TLS
   CURLFTPAUTH_LAST))

(define-exact-integer->symbol-function curl-constant-ftp-create-dir->symbol
  (CURLFTP_CREATE_DIR_NONE
   CURLFTP_CREATE_DIR
   CURLFTP_CREATE_DIR_RETRY
   CURLFTP_CREATE_DIR_LAST))

(define-exact-integer->symbol-function curl-constant-ftp-method->symbol
  (CURLFTPMETHOD_DEFAULT
   CURLFTPMETHOD_MULTICWD
   CURLFTPMETHOD_NOCWD
   CURLFTPMETHOD_SINGLECWD
   CURLFTPMETHOD_LAST))

(define-exact-integer->symbol-function curl-constant-proto->symbol
  (CURLPROTO_HTTP
   CURLPROTO_HTTPS
   CURLPROTO_FTP
   CURLPROTO_FTPS
   CURLPROTO_SCP
   CURLPROTO_SFTP
   CURLPROTO_TELNET
   CURLPROTO_LDAP
   CURLPROTO_LDAPS
   CURLPROTO_DICT
   CURLPROTO_FILE
   CURLPROTO_TFTP
   CURLPROTO_IMAP
   CURLPROTO_IMAPS
   CURLPROTO_POP3
   CURLPROTO_POP3S
   CURLPROTO_SMTP
   CURLPROTO_SMTPS
   CURLPROTO_RTSP
   CURLPROTO_RTMP
   CURLPROTO_RTMPT
   CURLPROTO_RTMPE
   CURLPROTO_RTMPTE
   CURLPROTO_RTMPS
   CURLPROTO_RTMPTS
   CURLPROTO_GOPHER
   CURLPROTO_ALL))

(define-exact-integer->symbol-function curl-constant-opt-type->symbol
  (CURLOPTTYPE_LONG
   CURLOPTTYPE_OBJECTPOINT
   CURLOPTTYPE_FUNCTIONPOINT
   CURLOPTTYPE_OFF_T))

(define-exact-integer->symbol-function curl-constant-opt->symbol
  (CURLOPT_FILE
   CURLOPT_URL
   CURLOPT_PORT
   CURLOPT_PROXY
   CURLOPT_USERPWD
   CURLOPT_PROXYUSERPWD
   CURLOPT_RANGE
   CURLOPT_INFILE
   CURLOPT_ERRORBUFFER
   CURLOPT_WRITEFUNCTION
   CURLOPT_READFUNCTION
   CURLOPT_TIMEOUT
   CURLOPT_INFILESIZE
   CURLOPT_POSTFIELDS
   CURLOPT_REFERER
   CURLOPT_FTPPORT
   CURLOPT_USERAGENT
   CURLOPT_LOW_SPEED_LIMIT
   CURLOPT_LOW_SPEED_TIME
   CURLOPT_RESUME_FROM
   CURLOPT_COOKIE
   CURLOPT_HTTPHEADER
   CURLOPT_HTTPPOST
   CURLOPT_SSLCERT
   CURLOPT_KEYPASSWD
   CURLOPT_CRLF
   CURLOPT_QUOTE
   CURLOPT_WRITEHEADER
   CURLOPT_COOKIEFILE
   CURLOPT_SSLVERSION
   CURLOPT_TIMECONDITION
   CURLOPT_TIMEVALUE
   CURLOPT_CUSTOMREQUEST
   CURLOPT_STDERR
   CURLOPT_POSTQUOTE
   CURLOPT_WRITEINFO
   CURLOPT_VERBOSE
   CURLOPT_HEADER
   CURLOPT_NOPROGRESS
   CURLOPT_NOBODY
   CURLOPT_FAILONERROR
   CURLOPT_UPLOAD
   CURLOPT_POST
   CURLOPT_DIRLISTONLY
   CURLOPT_APPEND
   CURLOPT_NETRC
   CURLOPT_FOLLOWLOCATION
   CURLOPT_TRANSFERTEXT
   CURLOPT_PUT
   CURLOPT_PROGRESSFUNCTION
   CURLOPT_PROGRESSDATA
   CURLOPT_AUTOREFERER
   CURLOPT_PROXYPORT
   CURLOPT_POSTFIELDSIZE
   CURLOPT_HTTPPROXYTUNNEL
   CURLOPT_INTERFACE
   CURLOPT_KRBLEVEL
   CURLOPT_SSL_VERIFYPEER
   CURLOPT_CAINFO
   CURLOPT_MAXREDIRS
   CURLOPT_FILETIME
   CURLOPT_TELNETOPTIONS
   CURLOPT_MAXCONNECTS
   CURLOPT_CLOSEPOLICY
   CURLOPT_FRESH_CONNECT
   CURLOPT_FORBID_REUSE
   CURLOPT_RANDOM_FILE
   CURLOPT_EGDSOCKET
   CURLOPT_CONNECTTIMEOUT
   CURLOPT_HEADERFUNCTION
   CURLOPT_HTTPGET
   CURLOPT_SSL_VERIFYHOST
   CURLOPT_COOKIEJAR
   CURLOPT_SSL_CIPHER_LIST
   CURLOPT_HTTP_VERSION
   CURLOPT_FTP_USE_EPSV
   CURLOPT_SSLCERTTYPE
   CURLOPT_SSLKEY
   CURLOPT_SSLKEYTYPE
   CURLOPT_SSLENGINE
   CURLOPT_SSLENGINE_DEFAULT
   CURLOPT_DNS_USE_GLOBAL_CACHE
   CURLOPT_DNS_CACHE_TIMEOUT
   CURLOPT_PREQUOTE
   CURLOPT_DEBUGFUNCTION
   CURLOPT_DEBUGDATA
   CURLOPT_COOKIESESSION
   CURLOPT_CAPATH
   CURLOPT_BUFFERSIZE
   CURLOPT_NOSIGNAL
   CURLOPT_SHARE
   CURLOPT_PROXYTYPE
   CURLOPT_ACCEPT_ENCODING
   CURLOPT_PRIVATE
   CURLOPT_HTTP200ALIASES
   CURLOPT_UNRESTRICTED_AUTH
   CURLOPT_FTP_USE_EPRT
   CURLOPT_HTTPAUTH
   CURLOPT_SSL_CTX_FUNCTION
   CURLOPT_SSL_CTX_DATA
   CURLOPT_FTP_CREATE_MISSING_DIRS
   CURLOPT_PROXYAUTH
   CURLOPT_FTP_RESPONSE_TIMEOUT
   CURLOPT_SERVER_RESPONSE_TIMEOUT
   CURLOPT_IPRESOLVE
   CURLOPT_MAXFILESIZE
   CURLOPT_INFILESIZE_LARGE
   CURLOPT_RESUME_FROM_LARGE
   CURLOPT_MAXFILESIZE_LARGE
   CURLOPT_NETRC_FILE
   CURLOPT_USE_SSL
   CURLOPT_POSTFIELDSIZE_LARGE
   CURLOPT_TCP_NODELAY
   CURLOPT_FTPSSLAUTH
   CURLOPT_IOCTLFUNCTION
   CURLOPT_IOCTLDATA
   CURLOPT_FTP_ACCOUNT
   CURLOPT_COOKIELIST
   CURLOPT_IGNORE_CONTENT_LENGTH
   CURLOPT_FTP_SKIP_PASV_IP
   CURLOPT_FTP_FILEMETHOD
   CURLOPT_LOCALPORT
   CURLOPT_LOCALPORTRANGE
   CURLOPT_CONNECT_ONLY
   CURLOPT_CONV_FROM_NETWORK_FUNCTION
   CURLOPT_CONV_TO_NETWORK_FUNCTION
   CURLOPT_CONV_FROM_UTF8_FUNCTION
   CURLOPT_MAX_SEND_SPEED_LARGE
   CURLOPT_MAX_RECV_SPEED_LARGE
   CURLOPT_FTP_ALTERNATIVE_TO_USER
   CURLOPT_SOCKOPTFUNCTION
   CURLOPT_SOCKOPTDATA
   CURLOPT_SSL_SESSIONID_CACHE
   CURLOPT_SSH_AUTH_TYPES
   CURLOPT_SSH_PUBLIC_KEYFILE
   CURLOPT_SSH_PRIVATE_KEYFILE
   CURLOPT_FTP_SSL_CCC
   CURLOPT_TIMEOUT_MS
   CURLOPT_CONNECTTIMEOUT_MS
   CURLOPT_HTTP_TRANSFER_DECODING
   CURLOPT_HTTP_CONTENT_DECODING
   CURLOPT_NEW_FILE_PERMS
   CURLOPT_NEW_DIRECTORY_PERMS
   CURLOPT_POSTREDIR
   CURLOPT_SSH_HOST_PUBLIC_KEY_MD5
   CURLOPT_OPENSOCKETFUNCTION
   CURLOPT_OPENSOCKETDATA
   CURLOPT_COPYPOSTFIELDS
   CURLOPT_PROXY_TRANSFER_MODE
   CURLOPT_SEEKFUNCTION
   CURLOPT_SEEKDATA
   CURLOPT_CRLFILE
   CURLOPT_ISSUERCERT
   CURLOPT_ADDRESS_SCOPE
   CURLOPT_CERTINFO
   CURLOPT_USERNAME
   CURLOPT_PASSWORD
   CURLOPT_PROXYUSERNAME
   CURLOPT_PROXYPASSWORD
   CURLOPT_NOPROXY
   CURLOPT_TFTP_BLKSIZE
   CURLOPT_SOCKS5_GSSAPI_SERVICE
   CURLOPT_SOCKS5_GSSAPI_NEC
   CURLOPT_PROTOCOLS
   CURLOPT_REDIR_PROTOCOLS
   CURLOPT_SSH_KNOWNHOSTS
   CURLOPT_SSH_KEYFUNCTION
   CURLOPT_SSH_KEYDATA
   CURLOPT_MAIL_FROM
   CURLOPT_MAIL_RCPT
   CURLOPT_FTP_USE_PRET
   CURLOPT_RTSP_REQUEST
   CURLOPT_RTSP_SESSION_ID
   CURLOPT_RTSP_STREAM_URI
   CURLOPT_RTSP_TRANSPORT
   CURLOPT_RTSP_CLIENT_CSEQ
   CURLOPT_RTSP_SERVER_CSEQ
   CURLOPT_INTERLEAVEDATA
   CURLOPT_INTERLEAVEFUNCTION
   CURLOPT_WILDCARDMATCH
   CURLOPT_CHUNK_BGN_FUNCTION
   CURLOPT_CHUNK_END_FUNCTION
   CURLOPT_FNMATCH_FUNCTION
   CURLOPT_CHUNK_DATA
   CURLOPT_FNMATCH_DATA
   CURLOPT_RESOLVE
   CURLOPT_TLSAUTH_USERNAME
   CURLOPT_TLSAUTH_PASSWORD
   CURLOPT_TLSAUTH_TYPE
   CURLOPT_TRANSFER_ENCODING
   CURLOPT_CLOSESOCKETFUNCTION
   CURLOPT_CLOSESOCKETDATA
   CURLOPT_GSSAPI_DELEGATION
   CURLOPT_DNS_SERVERS
   CURLOPT_ACCEPTTIMEOUT_MS
   CURLOPT_TCP_KEEPALIVE
   CURLOPT_TCP_KEEPIDLE
   CURLOPT_TCP_KEEPINTVL
   CURLOPT_SSL_OPTIONS
   CURLOPT_MAIL_AUTH
   CURLOPT_LASTENTRY

   CURLOPT_WRITEDATA
   CURLOPT_READDATA
   CURLOPT_HEADERDATA
   CURLOPT_RTSPHEADER))

(define-exact-integer->symbol-function curl-constant-ip-resolve->symbol
  (CURL_IPRESOLVE_WHATEVER
   CURL_IPRESOLVE_V4
   CURL_IPRESOLVE_V6))

(define-exact-integer->symbol-function curl-constant-http-version->symbol
  (CURL_HTTP_VERSION_NONE
   CURL_HTTP_VERSION_1_0
   CURL_HTTP_VERSION_1_1
   CURL_HTTP_VERSION_LAST))

(define-exact-integer->symbol-function curl-constant-rts-preq->symbol
  (CURL_RTSPREQ_NONE
   CURL_RTSPREQ_OPTIONS
   CURL_RTSPREQ_DESCRIBE
   CURL_RTSPREQ_ANNOUNCE
   CURL_RTSPREQ_SETUP
   CURL_RTSPREQ_PLAY
   CURL_RTSPREQ_PAUSE
   CURL_RTSPREQ_TEARDOWN
   CURL_RTSPREQ_GET_PARAMETER
   CURL_RTSPREQ_SET_PARAMETER
   CURL_RTSPREQ_RECORD
   CURL_RTSPREQ_RECEIVE
   CURL_RTSPREQ_LAST))

(define-exact-integer->symbol-function curl-constant-netrc->symbol
  (CURL_NETRC_IGNORED
   CURL_NETRC_OPTIONAL
   CURL_NETRC_REQUIRED
   CURL_NETRC_LAST))

(define-exact-integer->symbol-function curl-constant-ssl-version->symbol
  (CURL_SSLVERSION_DEFAULT
   CURL_SSLVERSION_TLSv1
   CURL_SSLVERSION_SSLv2
   CURL_SSLVERSION_SSLv3
   CURL_SSLVERSION_LAST))

(define-exact-integer->symbol-function curl-constant-tls-auth->symbol
  (CURL_TLSAUTH_NONE
   CURL_TLSAUTH_SRP
   CURL_TLSAUTH_LAST))

(define-exact-integer->symbol-function curl-constant-redir->symbol
  (CURL_REDIR_GET_ALL
   CURL_REDIR_POST_301
   CURL_REDIR_POST_302
   CURL_REDIR_POST_303
   CURL_REDIR_POST_ALL))

(define-exact-integer->symbol-function curl-constant-time-cond->symbol
  (CURL_TIMECOND_NONE
   CURL_TIMECOND_IFMODSINCE
   CURL_TIMECOND_IFUNMODSINCE
   CURL_TIMECOND_LASTMOD
   CURL_TIMECOND_LAST))

(define-exact-integer->symbol-function curl-constant-form->symbol
  (CURLFORM_NOTHING
   CURLFORM_COPYNAME
   CURLFORM_PTRNAME
   CURLFORM_NAMELENGTH
   CURLFORM_COPYCONTENTS
   CURLFORM_PTRCONTENTS
   CURLFORM_CONTENTSLENGTH
   CURLFORM_FILECONTENT
   CURLFORM_ARRAY
   CURLFORM_OBSOLETE
   CURLFORM_FILE
   CURLFORM_BUFFER
   CURLFORM_BUFFERPTR
   CURLFORM_BUFFERLENGTH
   CURLFORM_CONTENTTYPE
   CURLFORM_CONTENTHEADER
   CURLFORM_FILENAME
   CURLFORM_END
   CURLFORM_OBSOLETE2
   CURLFORM_STREAM
   CURLFORM_LASTENTRY))

(define-exact-integer->symbol-function curl-constant-form-add->symbol
  (CURL_FORMADD_OK
   CURL_FORMADD_MEMORY
   CURL_FORMADD_OPTION_TWICE
   CURL_FORMADD_NULL
   CURL_FORMADD_UNKNOWN_OPTION
   CURL_FORMADD_INCOMPLETE
   CURL_FORMADD_ILLEGAL_ARRAY
   CURL_FORMADD_DISABLED
   CURL_FORMADD_LAST))

(define-exact-integer->symbol-function curl-constant-info-type->symbol
  (CURLINFO_STRING
   CURLINFO_LONG
   CURLINFO_DOUBLE
   CURLINFO_SLIST
   CURLINFO_MASK
   CURLINFO_TYPEMASK))

(define-exact-integer->symbol-function curl-constant-info->symbol
  (CURLINFO_NONE
   CURLINFO_EFFECTIVE_URL
   CURLINFO_RESPONSE_CODE
   CURLINFO_TOTAL_TIME
   CURLINFO_NAMELOOKUP_TIME
   CURLINFO_CONNECT_TIME
   CURLINFO_PRETRANSFER_TIME
   CURLINFO_SIZE_UPLOAD
   CURLINFO_SIZE_DOWNLOAD
   CURLINFO_SPEED_DOWNLOAD
   CURLINFO_SPEED_UPLOAD
   CURLINFO_HEADER_SIZE
   CURLINFO_REQUEST_SIZE
   CURLINFO_SSL_VERIFYRESULT
   CURLINFO_FILETIME
   CURLINFO_CONTENT_LENGTH_DOWNLOAD
   CURLINFO_CONTENT_LENGTH_UPLOAD
   CURLINFO_STARTTRANSFER_TIME
   CURLINFO_CONTENT_TYPE
   CURLINFO_REDIRECT_TIME
   CURLINFO_REDIRECT_COUNT
   CURLINFO_PRIVATE
   CURLINFO_HTTP_CONNECTCODE
   CURLINFO_HTTPAUTH_AVAIL
   CURLINFO_PROXYAUTH_AVAIL
   CURLINFO_OS_ERRNO
   CURLINFO_NUM_CONNECTS
   CURLINFO_SSL_ENGINES
   CURLINFO_COOKIELIST
   CURLINFO_LASTSOCKET
   CURLINFO_FTP_ENTRY_PATH
   CURLINFO_REDIRECT_URL
   CURLINFO_PRIMARY_IP
   CURLINFO_APPCONNECT_TIME
   CURLINFO_CERTINFO
   CURLINFO_CONDITION_UNMET
   CURLINFO_RTSP_SESSION_ID
   CURLINFO_RTSP_CLIENT_CSEQ
   CURLINFO_RTSP_SERVER_CSEQ
   CURLINFO_RTSP_CSEQ_RECV
   CURLINFO_PRIMARY_PORT
   CURLINFO_LOCAL_IP
   CURLINFO_LOCAL_PORT
   CURLINFO_LASTONE
   CURLINFO_HTTP_CODE))

(define-exact-integer->symbol-function curl-constant-close-policy->symbol
  (CURLCLOSEPOLICY_NONE
   CURLCLOSEPOLICY_OLDEST
   CURLCLOSEPOLICY_LEAST_RECENTLY_USED
   CURLCLOSEPOLICY_LEAST_TRAFFIC
   CURLCLOSEPOLICY_SLOWEST
   CURLCLOSEPOLICY_CALLBACK
   CURLCLOSEPOLICY_LAST))

(define-exact-integer->symbol-function curl-constant-global->symbol
  (CURL_GLOBAL_SSL
   CURL_GLOBAL_WIN32
   CURL_GLOBAL_ALL
   CURL_GLOBAL_NOTHING
   CURL_GLOBAL_DEFAULT))

(define-exact-integer->symbol-function curl-constant-lock-data->symbol
  (CURL_LOCK_DATA_NONE
   CURL_LOCK_DATA_SHARE
   CURL_LOCK_DATA_COOKIE
   CURL_LOCK_DATA_DNS
   CURL_LOCK_DATA_SSL_SESSION
   CURL_LOCK_DATA_CONNECT
   CURL_LOCK_DATA_LAST))

(define-exact-integer->symbol-function curl-constant-lock-access->symbol
  (CURL_LOCK_ACCESS_NONE
   CURL_LOCK_ACCESS_SHARED
   CURL_LOCK_ACCESS_SINGLE
   CURL_LOCK_ACCESS_LAST))

(define-exact-integer->symbol-function curl-constant-she->symbol
  (CURLSHE_OK
   CURLSHE_BAD_OPTION
   CURLSHE_IN_USE
   CURLSHE_INVALID
   CURLSHE_NOMEM
   CURLSHE_NOT_BUILT_IN
   CURLSHE_LAST))

(define-exact-integer->symbol-function curl-constant-shopt->symbol
  (CURLSHOPT_NONE
   CURLSHOPT_SHARE
   CURLSHOPT_UNSHARE
   CURLSHOPT_LOCKFUNC
   CURLSHOPT_UNLOCKFUNC
   CURLSHOPT_USERDATA
   CURLSHOPT_LAST))

(define-exact-integer->symbol-function curl-constant-version-num->symbol
  (CURLVERSION_FIRST
   CURLVERSION_SECOND
   CURLVERSION_THIRD
   CURLVERSION_FOURTH
   CURLVERSION_LAST
   CURLVERSION_NOW))

(define-exact-integer->symbol-function curl-constant-version->symbol
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

(define-exact-integer->symbol-function curl-constant-pause->symbol
  (CURLPAUSE_RECV
   CURLPAUSE_RECV_CONT
   CURLPAUSE_SEND
   CURLPAUSE_SEND_CONT
   CURLPAUSE_ALL
   CURLPAUSE_CONT))

(define-exact-integer->symbol-function curl-constant-m->symbol
  (CURLM_CALL_MULTI_PERFORM
   CURLM_OK
   CURLM_BAD_HANDLE
   CURLM_BAD_EASY_HANDLE
   CURLM_OUT_OF_MEMORY
   CURLM_INTERNAL_ERROR
   CURLM_BAD_SOCKET
   CURLM_UNKNOWN_OPTION
   CURLM_LAST
   CURLM_CALL_MULTI_SOCKET))

(define-exact-integer->symbol-function curl-constant-msg->symbol
  (CURLMSG_NONE
   CURLMSG_DONE
   CURLMSG_LAST))

(define-exact-integer->symbol-function curl-constant-poll->symbol
  (CURL_POLL_NONE
   CURL_POLL_IN
   CURL_POLL_OUT
   CURL_POLL_INOUT
   CURL_POLL_REMOVE))

(define-exact-integer->symbol-function curl-constant-cselect->symbol
  (CURL_CSELECT_IN
   CURL_CSELECT_OUT
   CURL_CSELECT_ERR))

(define-exact-integer->symbol-function curl-constant-mopt->symbol
  (CURLMOPT_SOCKETFUNCTION
   CURLMOPT_SOCKETDATA
   CURLMOPT_PIPELINING
   CURLMOPT_TIMERFUNCTION
   CURLMOPT_TIMERDATA
   CURLMOPT_MAXCONNECTS
   CURLMOPT_LASTENTRY))


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
