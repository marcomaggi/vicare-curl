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
;;;Copyright (C) 2012, 2013, 2015, 2016, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare net curl (0 4 2017 1 27))
  (options typed-language)
  (foreign-library "vicare-curl")
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
    curl-form-data?				curl-form-data?/alive
    (rename (%make-curl-form-data		make-curl-form-data))
    curl-form-data-string
    curl-form-data-custom-destructor		set-curl-form-data-custom-destructor!

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
    curl-share-custom-destructor		set-curl-share-custom-destructor!

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
    curl-easy-custom-destructor			set-curl-easy-custom-destructor!

    ;; multi API
    curl-multi-init				curl-multi-cleanup
    curl-multi-add-handle			curl-multi-remove-handle
    (rename (%curl-multi-easies curl-multi-easies))
    curl-multi-setopt				curl-multi-fdset
    curl-multi-perform				curl-multi-info-read
    curl-multi-socket				curl-multi-socket-action
    curl-multi-socket-all			curl-multi-wait
    curl-multi-timeout				curl-multi-assign
    curl-multi-strerror

    curl-multi
    curl-multi?					curl-multi?/alive
    curl-multi-custom-destructor		set-curl-multi-custom-destructor!

    curl-waitfd
    make-curl-waitfd				curl-waitfd?
    curl-waitfd-fd
    curl-waitfd-events
    curl-waitfd-revents

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
    make-curl-multi-timer-callback		make-curl-xferinfo-callback

    ;; miscellaneous functions
    curl-free					curl-getdate

    ;; Scheme representation of "struct curl_tlssessioninfo"
    curl-tls-session-info
    make-curl-tls-session-info			curl-tls-session-info?
    curl-tls-session-info-backend
    curl-tls-session-info-internals

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

    curl-fileinfo?				pointer->curl-fileinfo
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

    ;; accessors for "struct curl_tlssessioninfo"
    curl-tlssessioninfo.backend			curl-tlssessioninfo.internals

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
    curl-constant-wait-poll->symbol
    curl-constant-cselect->symbol
    curl-constant-mopt->symbol)
  (import (vicare (0 4 2017 1 (>= 10)))
    (prefix (vicare system structs) structs::)
    (vicare net curl constants (0 4 2015 5 28))
    (prefix (vicare net curl unsafe-capi (0 4 2015 5 28))
	    capi::)
    (vicare system $fx)
    (vicare system $pairs)
    (prefix (vicare ffi (or (0 4 2015 5 (>= 28))
			    (0 4 2015 (>= 6))
			    (0 4 (>= 2016))))
	    ffi::)
    (vicare ffi foreign-pointer-wrapper)
    (prefix (vicare platform words) words::)
    (vicare language-extensions syntaxes)
    (vicare arguments general-c-buffers))


;;;; arguments validation

(define (pointer-or-memory-block? obj)
  (or (pointer? obj)
      (memory-block? obj)))

(define-syntax-rule (file-descriptor? obj)
  (non-negative-fixnum? obj))

(define (action-socket-descriptor? obj)
  (and (fixnum? obj)
       (or ($fx= obj CURL_SOCKET_TIMEOUT)
	   ($fxnonnegative? obj))))

(define (assert-curl-share-parameter who parameter option)
  (unless (cond ((or (= option CURLSHOPT_SHARE)
		     (= option CURLSHOPT_UNSHARE))
		 (words::signed-int? parameter))
		(else
		 (or (not parameter)
		     (pointer? parameter))))
    (procedure-arguments-consistency-violation who
      "invalid matching between \"curl-share\" option and parameter"
      parameter option)))

(define (false-or-vector-of-curl-waitfd? obj)
  (or (not obj)
      (and (vector? obj)
	   (vector-for-all curl-waitfd? obj))))


;;;; helpers

(define-syntax-rule (unimplemented who)
  (assertion-violation who "unimplemented function"))

;;; --------------------------------------------------------------------

(define-syntax-rule (%define-raw-struct-accessor ?who ?accessor)
  ;;Used to define a  function to access a C language struct  field through a pointer
  ;;to it.
  ;;
  (define* (?who {stru pointer?})
    (?accessor stru)))


;;;; version functions

(define (vicare-curl-version-interface-current)
  (capi::vicare-curl-version-interface-current))

(define (vicare-curl-version-interface-revision)
  (capi::vicare-curl-version-interface-revision))

(define (vicare-curl-version-interface-age)
  (capi::vicare-curl-version-interface-age))

(define (vicare-curl-version)
  (ascii->string (capi::vicare-curl-version)))

;;; --------------------------------------------------------------------

(define (curl-version)
  (ascii->string (capi::curl-version)))

;;; --------------------------------------------------------------------

(structs::define-struct curl-version-info-data
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

(define* (curl-version-info {version-code non-negative-fixnum?})
  (receive-and-return (rv)
      (capi::curl-version-info (type-descriptor curl-version-info-data) version-code)
    (internal-body
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
       rv (map ascii->string (curl-version-info-data-protocols rv))))))

(define* (curl-version-info-features->symbols {S curl-version-info-data?})
  (let loop ((result   '())
	     (features (curl-version-info-data-features S))
	     (flags	`( ;;
			  ,CURL_VERSION_IPV6		,CURL_VERSION_KERBEROS4
			  ,CURL_VERSION_SSL		,CURL_VERSION_LIBZ
			  ,CURL_VERSION_NTLM		,CURL_VERSION_GSSNEGOTIATE
			  ,CURL_VERSION_DEBUG		,CURL_VERSION_ASYNCHDNS
			  ,CURL_VERSION_SPNEGO		,CURL_VERSION_LARGEFILE
			  ,CURL_VERSION_IDN		,CURL_VERSION_SSPI
			  ,CURL_VERSION_CONV		,CURL_VERSION_CURLDEBUG
			  ,CURL_VERSION_TLSAUTH_SRP	,CURL_VERSION_NTLM_WB)))
    (if (null? flags)
	result
      (loop (let ((flag ($car flags)))
	      (if (zero? (bitwise-and flag features))
		  result
		(cons (%curl-version-info-features->symbols flag)
		      result)))
	    features
	    ($cdr flags)))))

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

(define* (curl-version-feature? {S curl-version-info-data?} {feature non-negative-fixnum?})
  (not (zero? (bitwise-and feature (curl-version-info-data-features S)))))


;;;; global initialisation and finalisation functions

(define* (curl-global-init {flags words::signed-long?})
  (capi::curl-global-init flags))

(define* (curl-global-init-mem {flags words::signed-long?}
			       {malloc-callback false-or-pointer?} {free-callback false-or-pointer?} {realloc-callback false-or-pointer?}
			       {strdup-callback false-or-pointer?} {calloc-callback false-or-pointer?})
  (capi::curl-global-init-mem flags
			     malloc-callback free-callback realloc-callback
			     strdup-callback calloc-callback))

(define (curl-global-cleanup)
  (capi::curl-global-cleanup))

;;; --------------------------------------------------------------------

(define make-curl-malloc-callback
  ;; void *(*curl_malloc_callback)(size_t size)
  (let ((maker (ffi::make-c-callback-maker 'pointer '(size_t))))
    (lambda (user-scheme-callback)
      (maker (lambda (number-of-bytes)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (null-pointer)))
		 (user-scheme-callback number-of-bytes)))))))

(define make-curl-free-callback
  ;; void (*curl_free_callback)(void *ptr)
  (let ((maker (ffi::make-c-callback-maker 'void '(pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (ptr)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (void)))
		 (user-scheme-callback ptr)))))))

(define make-curl-realloc-callback
  ;; void *(*curl_realloc_callback)(void *ptr, size_t size)
  (let ((maker (ffi::make-c-callback-maker 'pointer '(pointer size_t))))
    (lambda (user-scheme-callback)
      (maker (lambda (ptr number-of-bytes)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (null-pointer)))
		 (user-scheme-callback ptr number-of-bytes)))))))

(define make-curl-strdup-callback
  ;; char *(*curl_strdup_callback)(const char *str)
  (let ((maker (ffi::make-c-callback-maker 'pointer '(pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (ptr)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (null-pointer)))
		 (user-scheme-callback ptr)))))))

(define make-curl-calloc-callback
  ;; void *(*curl_calloc_callback)(size_t nmemb, size_t size)
  (let ((maker (ffi::make-c-callback-maker 'pointer '(size_t size_t))))
    (lambda (user-scheme-callback)
      (maker (lambda (number-of-items item-size)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (null-pointer)))
		 (user-scheme-callback number-of-items item-size)))))))


;;;; string lists

(case-define* curl-slist-append
  ((string)
   (curl-slist-append #f string))
  (({slist false-or-pointer?} {string general-c-string?})
   (with-general-c-strings
       ((string^ string))
     (string-to-bytevector string->utf8)
     (capi::curl-slist-append slist string^))))

(define* (curl-slist-free-all {slist false-or-pointer?})
  (capi::curl-slist-free-all slist)
  (values))

(define* (curl-slist->list {slist false-or-pointer?})
  (map ascii->string (reverse (capi::curl-slist->list slist))))

(define* (list->curl-slist {list-of-strings list-of-strings?})
  (fold-left (lambda ({_ <top>} slist str)
	       (curl-slist-append slist str))
    #f list-of-strings))


;;;; multipart/formdata composition

(define-foreign-pointer-wrapper curl-form-data
  (fields pointer-to-last)
  (foreign-destructor capi::curl-formfree)
  (collector-struct-type #f))

(define (%struct-curl-form-data-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[curl-form-data")
  (%display " pointer=")	(%display ($curl-form-data-pointer S))
  (%display " data=")		(%write   (curl-form-data-string   S))
  (%display "]"))

;;; --------------------------------------------------------------------

(define (%make-curl-form-data)
  (make-curl-form-data/owner (null-pointer) (null-pointer)))

;;; --------------------------------------------------------------------

(module (curl-formadd)

  (define (%normalise-val val)
    (cond ((words::signed-long? val)
	   val)
	  ((string? val)
	   (string->ascii val))
	  (else
	   val)))

  (define (a-value? obj)
    (or (words::signed-long? obj)
	(string? obj)
	(bytevector? obj)
	(pointer? obj)
	(memory-block? obj)))

  (define (curl-form-end? obj)
    (eqv? obj CURLFORM_END))

  (case-define* curl-formadd
    ((post opt1 val1 {optend curl-form-end?})
     (curl-formadd post opt1 val1))

    (({post curl-form-data?} {opt1 words::signed-int?} {val1 a-value?})
     (capi::curl-formadd-1 post ($curl-form-data-pointer-to-last post)
			   opt1 (%normalise-val val1)))

    ((post opt1 val1 opt2 val2 {optend curl-form-end?})
     (curl-formadd post
		   opt1 val1
		   opt2 val2))

    (({post curl-form-data?}
      {opt1 words::signed-int?} {val1 a-value?}
      {opt2 words::signed-int?} {val2 a-value?})
     (capi::curl-formadd-2 post ($curl-form-data-pointer-to-last post)
			   opt1 (%normalise-val val1)
			   opt2 (%normalise-val val2)))

    (({post curl-form-data?}
      {opt1 words::signed-int?} {val1 a-value?}
      {opt2 words::signed-int?} {val2 a-value?}
      {opt3 words::signed-int?} {val3 a-value?}
      {optend curl-form-end?})
     (curl-formadd post
		   opt1 val1
		   opt2 val2
		   opt3 val3))

    (({post curl-form-data?}
      {opt1 words::signed-int?} {val1 a-value?}
      {opt2 words::signed-int?} {val2 a-value?}
      {opt3 words::signed-int?} {val3 a-value?})
     (capi::curl-formadd-3 post ($curl-form-data-pointer-to-last post)
			   opt1 (%normalise-val val1)
			   opt2 (%normalise-val val2)
			   opt3 (%normalise-val val3)))

    (({post curl-form-data?}
      {opt1 words::signed-int?} {val1 a-value?}
      {opt2 words::signed-int?} {val2 a-value?}
      {opt3 words::signed-int?} {val3 a-value?}
      {opt4 words::signed-int?} {val4 a-value?}
      {optend curl-form-end?})
     (curl-formadd post
		   opt1 val1
		   opt2 val2
		   opt3 val3
		   opt4 val4))

    (({post curl-form-data?}
      {opt1 words::signed-int?} {val1 a-value?}
      {opt2 words::signed-int?} {val2 a-value?}
      {opt3 words::signed-int?} {val3 a-value?}
      {opt4 words::signed-int?} {val4 a-value?})
     (capi::curl-formadd-4 post ($curl-form-data-pointer-to-last post)
			   opt1 (%normalise-val val1)
			   opt2 (%normalise-val val2)
			   opt3 (%normalise-val val3)
			   opt4 (%normalise-val val4)))

    #| end of CASE-DEFINE* |# )

  #| end of module |# )

(define* (curl-formget {post curl-form-data?} {custom-data false-or-pointer?} {callback ffi::false-or-c-callback?})
  (capi::curl-formget post custom-data callback))

(define* (curl-formfree {post curl-form-data?})
  ($curl-form-data-finalise post)
  (values))

;;; --------------------------------------------------------------------

(define make-curl-formget-callback
  ;; size_t curl_formget_callback (void *arg, const char *buf, size_t len)
  (let ((maker (ffi::make-c-callback-maker 'size_t '(pointer pointer size_t))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data cstring.ptr cstring.len)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback custom-data cstring.ptr cstring.len)))))))

;;; --------------------------------------------------------------------

(define* (curl-form-data-string {post curl-form-data?})
  (if (pointer-null? (curl-form-data-pointer post))
      ""
    (let* (({data <string>} "")
	   (cb   (make-curl-formget-callback
		  (lambda (custom-data cstring.ptr cstring.len)
		    (set! data (string-append data (cstring->string cstring.ptr cstring.len)))
		    cstring.len))))
      (unwind-protect
	  (let ((rv (curl-formget post #f cb)))
	    (and (not rv) data))
	(ffi::free-c-callback cb)))))


;;;; basic URL string escaping

(case-define* curl-escape
  ((str.data)
   (curl-escape str.data #f))
  (({str.data general-c-string?} str.len)
   (assert-general-c-string-and-length __who__ str.data str.len)
   (with-general-c-strings
       ((str.data^ str.data))
     (capi::curl-escape str.data^ str.len))))

(case-define* curl-unescape
  ((str.data)
   (curl-unescape str.data #f))
  (({str.data general-c-string?} str.len)
   (assert-general-c-string-and-length __who__ str.data str.len)
   (with-general-c-strings
       ((str.data^ str.data))
     (capi::curl-unescape str.data^ str.len))))

;;; --------------------------------------------------------------------

(case-define* curl-escape/string
  ((str.data)
   (curl-escape/string str.data #f))
  ((str.data str.len)
   (let ((rv (curl-escape str.data str.len)))
     (and rv (ascii->string rv)))))

(case-define* curl-unescape/string
  ((str.data)
   (curl-unescape/string str.data #f))
  ((str.data str.len)
   (let ((rv (curl-unescape str.data str.len)))
     (and rv (ascii->string rv)))))


;;;; shared configuration option sets

(define-foreign-pointer-wrapper curl-share
  (foreign-destructor capi::curl-share-cleanup)
  (collector-struct-type #f))

(define (%struct-curl-share-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[curl-share")
  (%display " pointer=")	(%display ($curl-share-pointer S))
  (%display "]"))

;;; --------------------------------------------------------------------

(define (curl-share-init)
  (let ((rv (capi::curl-share-init)))
    (and rv (make-curl-share/owner rv))))

(define* (curl-share-setopt {share curl-share?/alive} {option words::signed-int?} parameter)
  (assert-curl-share-parameter __who__ parameter option)
  (capi::curl-share-setopt share option parameter))

(define* (curl-share-cleanup {share curl-share?})
  ($curl-share-finalise share))

(define* (curl-share-strerror {errcode words::signed-int?})
  (capi::curl-share-strerror errcode))

(define (curl-share-strerror/string errcode)
  (ascii->string (curl-share-strerror errcode)))

;;; --------------------------------------------------------------------

(define make-curl-lock-function
  ;; void curl_lock_function (CURL *handle, curl_lock_data data,
  ;;                          curl_lock_access locktype, void *userptr)
  (let ((maker (ffi::make-c-callback-maker 'void '(pointer signed-int signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle data locktype userptr)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (void)))
		 (user-scheme-callback (make-curl-easy/not-owner handle #f)
				       data locktype
				       (if (pointer-null? userptr)
					   #f
					 userptr))
		 (void)))))))

(define make-curl-unlock-function
  ;; void curl_unlock_function (CURL *handle, curl_lock_data data, void *userptr)
  (let ((maker (ffi::make-c-callback-maker 'void '(pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle data userptr)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (void)))
		 (user-scheme-callback (make-curl-easy/not-owner handle #f)
				       data
				       (if (pointer-null? userptr)
					   #f
					 userptr))
		 (void)))))))


;;;; easy API

(define-foreign-pointer-wrapper curl-easy
  (foreign-destructor capi::curl-easy-cleanup)
  (collector-struct-type curl-multi))

(define (%struct-curl-easy-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[curl-easy")
  (%display " pointer=")	(%display ($curl-easy-pointer S))
  (%display " owner?=")		(%display (curl-easy-pointer-owner? S))
  (%display "]"))

;;; --------------------------------------------------------------------

(define (curl-easy-init)
  (let ((rv (capi::curl-easy-init)))
    (and rv
	 ;;This struct instance has no collector.
	 (make-curl-easy/owner rv #f))))

(define* (curl-easy-cleanup {easy curl-easy?})
  ($curl-easy-finalise easy))

(define* (curl-easy-reset {easy curl-easy?/alive})
  (capi::curl-easy-reset easy))

;;; --------------------------------------------------------------------

(module (curl-easy-setopt)

  (define* (curl-easy-setopt {easy curl-easy?/alive} {option words::signed-int?} parameter)
    (cond ((= CURLOPT_SHARE option)
	   (%curl-easy-setopt/share easy option parameter))
	  ((<= CURLOPTTYPE_OFF_T option)
	   (%curl-easy-setopt/type-off-t easy option parameter))
	  ((<= CURLOPTTYPE_FUNCTIONPOINT option)
	   (%curl-easy-setopt/function-pointer easy option parameter))
	  ((<= CURLOPTTYPE_OBJECTPOINT option)
	   (%curl-easy-setopt/object-pointer easy option parameter))
	  ((<= CURLOPTTYPE_LONG option)
	   (if (boolean? parameter)
	       (capi::curl-easy-setopt easy option (if parameter 1 0))
	     (%curl-easy-setopt/type-long easy option parameter)))
	  (else
	   (procedure-argument-violation __who__
	     "invalid parameter type for selected option"
	     easy option parameter))))

  (define* (%curl-easy-setopt/share easy option {parameter curl-share?/alive})
    (capi::curl-easy-setopt easy option ($curl-share-pointer parameter)))

  (define* (%curl-easy-setopt/type-off-t easy option {parameter words::off_t?})
    (capi::curl-easy-setopt easy option parameter))

  (define* (%curl-easy-setopt/function-pointer easy option {parameter false-or-pointer?})
    (capi::curl-easy-setopt easy option parameter))

  (define* (%curl-easy-setopt/object-pointer easy option {parameter (or not general-c-string?)})
    (with-general-c-strings/false
	((parameter^ parameter))
      (string-to-bytevector string->utf8)
      (capi::curl-easy-setopt easy option parameter^)))

  (define* (%curl-easy-setopt/type-long easy option {parameter words::signed-long?})
    (capi::curl-easy-setopt easy option parameter))

  #| end of module |# )

(define* (curl-easy-getinfo {easy curl-easy?/alive} {info words::signed-int?})
  (let ((rv (capi::curl-easy-getinfo easy info)))
    (if (pair? rv)
	(let ((retval.type	($car rv))
	      (retval.value	($cdr rv)))
	  ;;NOTE  By using  EQV?  we make  the  tests work  even  when the  CURLINFO_
	  ;;constants are not defined by the underlying Libcurl.
	  (cond ((eqv? info CURLINFO_CERTINFO)
		 (values CURLE_OK (and retval.value
				       (curl-certinfo.certinfo retval.value))))
		((eqv? info CURLINFO_PRIVATE)
		 (values CURLE_OK retval.value))
		((eqv? info CURLINFO_TLS_SESSION)
		 (values CURLE_OK (and retval.value
				       (make-curl-tls-session-info (curl-tlssessioninfo.backend   retval.value)
								   (curl-tlssessioninfo.internals retval.value)))))
		(else
		 (cond ((= retval.type CURLINFO_SLIST)
			(values CURLE_OK (and retval.value
					      (let ((rv (curl-slist->list retval.value)))
						(curl-slist-free-all retval.value)
						rv))))
		       ((= retval.type CURLINFO_DOUBLE CURLINFO_LONG)
			(values CURLE_OK retval.value))
		       ((= retval.type CURLINFO_STRING)
			(values CURLE_OK (and retval.value (ascii->string retval.value))))
		       (else
			(values CURLE_OK retval.value))))))
      (values rv #f))))

;;; --------------------------------------------------------------------

(structs::define-struct curl-tls-session-info
  ;;Scheme  representation of  the C  language structure  "curl_tlssessioninfo".  For
  ;;details:  see the  documentation of  the constant  "CURLINFO_TLS_SESSION" in  the
  ;;manual page of "curl_easy_getinfo()".
  ;;
  (backend
		;One of the constants in the "enum curl_sslbackend".
   internals
		;A pointer object representing the value of the field "internals".
   ))

(module ()

  (define (printer S port sub-printer)
    (define-syntax-rule (%display thing)
      (display thing port))
    (define-syntax-rule (%write thing)
      (write thing port))
    (%display "#[curl-tls-session-info")
    (%display " backend=")	(%display (let ((X ($curl-tls-session-info-backend S)))
					    (cond ((eqv? X CURLSSLBACKEND_AXTLS)	'CURLSSLBACKEND_AXTLS)
						  ((eqv? X CURLSSLBACKEND_CYASSL)	'CURLSSLBACKEND_CYASSL)
						  ((eqv? X CURLSSLBACKEND_DARWINSSL)	'CURLSSLBACKEND_DARWINSSL)
						  ((eqv? X CURLSSLBACKEND_GNUTLS)	'CURLSSLBACKEND_GNUTLS)
						  ((eqv? X CURLSSLBACKEND_GSKIT)	'CURLSSLBACKEND_GSKIT)
						  ((eqv? X CURLSSLBACKEND_NONE)		'CURLSSLBACKEND_NONE)
						  ((eqv? X CURLSSLBACKEND_NSS)		'CURLSSLBACKEND_NSS)
						  ((eqv? X CURLSSLBACKEND_OPENSSL)	'CURLSSLBACKEND_OPENSSL)
						  ((eqv? X CURLSSLBACKEND_POLARSSL)	'CURLSSLBACKEND_POLARSSL)
						  ((eqv? X CURLSSLBACKEND_QSOSSL)	'CURLSSLBACKEND_QSOSSL)
						  ((eqv? X CURLSSLBACKEND_SCHANNEL)	'CURLSSLBACKEND_SCHANNEL)
						  (else X))))
    (%display " internals=")	(%display ($curl-tls-session-info-internals S))
    (%display "]"))

  (structs::set-struct-type-printer! (struct-type-descriptor curl-tls-session-info) printer)

  #| end of module |# )

;;; --------------------------------------------------------------------

(define* (curl-easy-perform {easy curl-easy?/alive})
  (capi::curl-easy-perform easy))

(define* (curl-easy-duphandle {easy curl-easy?/alive})
  (let ((rv (capi::curl-easy-duphandle easy)))
    (and rv (make-curl-easy/owner rv #f))))

(define* (curl-easy-pause {easy curl-easy?/alive} {bitmask words::signed-int?})
  (capi::curl-easy-pause easy bitmask))

;;; --------------------------------------------------------------------

(case-define* curl-easy-recv
  ((easy buffer.data)
   (curl-easy-recv easy buffer.data #f))
  (({easy curl-easy?/alive} {buffer.data general-c-buffer?} buffer.len)
   (assert-general-c-buffer-and-length __who__ buffer.data buffer.len)
   (let ((rv (capi::curl-easy-recv easy buffer.data buffer.len)))
     (if (pair? rv)
	 (values ($car rv) ($cdr rv))
       (values rv #f)))))

(case-define* curl-easy-send
  ((easy buffer.data)
   (curl-easy-send easy buffer.data #f))
  (({easy curl-easy?/alive} {buffer.data general-c-string?} buffer.len)
   (assert-general-c-string-and-length __who__ buffer.data buffer.len)
   (with-general-c-strings
       ((buffer.data^ buffer.data))
     (string-to-bytevector string->utf8)
     (let ((rv (capi::curl-easy-send easy buffer.data^ buffer.len)))
       (if (pair? rv)
	   (values ($car rv) ($cdr rv))
	 (values rv #f))))))

;;; --------------------------------------------------------------------

(case-define* curl-easy-escape
  ((easy chars.data)
   (curl-easy-escape easy chars.data #f))
  (({easy curl-easy?/alive} {chars.data general-c-string?} chars.len)
   (assert-general-c-string-and-length __who__ chars.data chars.len)
   (with-general-c-strings
       ((chars.data^ chars.data))
     (string-to-bytevector string->utf8)
     (capi::curl-easy-escape easy chars.data^ chars.len))))

(case-define* curl-easy-unescape
  ((easy chars.data)
   (curl-easy-unescape easy chars.data #f))
  (({easy curl-easy?/alive} {chars.data general-c-string?} chars.len)
   (assert-general-c-string-and-length __who__ chars.data chars.len)
   (with-general-c-strings
       ((chars.data^ chars.data))
     (string-to-bytevector string->utf8)
     (capi::curl-easy-unescape easy chars.data^ chars.len))))

(case-define* curl-easy-escape/string
  ((easy chars.data)
   (let ((rv (curl-easy-escape easy chars.data #f)))
     (and rv (ascii->string rv))))
  ((easy chars.data chars.len)
   (let ((rv (curl-easy-escape easy chars.data chars.len)))
     (and rv (ascii->string rv)))))

(case-define* curl-easy-unescape/string
  ((easy chars.data)
   (let ((rv (curl-easy-unescape easy chars.data #f)))
     (and rv (ascii->string rv))))
  ((easy chars.data chars.len)
   (let ((rv (curl-easy-unescape easy chars.data chars.len)))
     (and rv (ascii->string rv)))))

;;; --------------------------------------------------------------------

(define* (curl-easy-strerror {code words::signed-int?})
  (let ((rv (capi::curl-easy-strerror code)))
    (and rv (ascii->string rv))))


;;;; multi API

(define-foreign-pointer-wrapper curl-multi
  (foreign-destructor capi::curl-multi-cleanup)
  (collector-struct-type #f)
  (collected-struct-type curl-easy))

(define (%struct-curl-multi-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[curl-multi")
  (%display " pointer=")	(%display ($curl-multi-pointer S))
  (%display " owner?=")		(%display (curl-multi-pointer-owner? S))
  (%display "]"))

;;; --------------------------------------------------------------------

(define (curl-multi-init)
  (let ((rv (capi::curl-multi-init)))
    (and rv (make-curl-multi/owner rv))))

(define* (curl-multi-cleanup {multi curl-multi?})
  ($curl-multi-finalise multi))

;;; --------------------------------------------------------------------

(define* (curl-multi-add-handle {multi curl-multi?/alive} {easy curl-easy?})
  (if ($curl-multi-contains-curl-easy? multi easy)
      CURLM_OK
    (receive-and-return (rv)
	(capi::curl-multi-add-handle multi easy)
      (when (= rv CURLM_OK)
	($curl-multi-register-curl-easy! multi easy)))))

(define* (curl-multi-remove-handle {multi curl-multi?/alive} {easy curl-easy?})
  (if ($curl-multi-contains-curl-easy? multi easy)
      (receive-and-return (rv)
	  (capi::curl-multi-remove-handle multi easy)
	(when (= rv CURLM_OK)
	  ($curl-multi-forget-curl-easy! multi easy)))
    CURLM_OK))

(define* (%curl-multi-easies {multi curl-multi?/alive})
  ;;This function name is prefixed with % to avoid conflict with the accessor for the
  ;;"easies" field of the CURL-MULTI data structure.
  ;;
  ($curl-multi-vector-of-collected-curl-easy multi))

;;; --------------------------------------------------------------------

(module (curl-multi-setopt)

  (define* (curl-multi-setopt {multi curl-multi?/alive} {option words::signed-int?} parameter)
    (cond ((<= CURLOPTTYPE_OFF_T option)
	   (%curl-multi-setopt/type-off-t multi option parameter))
	  ((<= CURLOPTTYPE_FUNCTIONPOINT option)
	   (%curl-multi-setopt/function-pointer multi option parameter))
	  ((<= CURLOPTTYPE_OBJECTPOINT option)
	   (%curl-multi-setopt/object-pointer multi option parameter))
	  ((<= CURLOPTTYPE_LONG option)
	   (if (boolean? parameter)
	       (capi::curl-multi-setopt multi option (if parameter 1 0))
	     (%curl-multi-setopt/type-long multi option parameter)))
	  (else
	   (procedure-argument-violation __who__
	     "invalid parameter type for selected option"
	     multi option parameter))))

  (define* (%curl-multi-setopt/type-off-t multi option {parameter words::off_t?})
    (capi::curl-multi-setopt multi option parameter))

  (define* (%curl-multi-setopt/function-pointer multi option {parameter false-or-pointer?})
    (capi::curl-multi-setopt multi option parameter))

  (define* (%curl-multi-setopt/object-pointer multi option {parameter (or not general-c-string?)})
    (with-general-c-strings/false
	((parameter^ parameter))
      (string-to-bytevector string->utf8)
      (capi::curl-multi-setopt multi option parameter^)))

  (define* (%curl-multi-setopt/type-long multi option {parameter words::signed-long?})
    (capi::curl-multi-setopt multi option parameter))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define* (curl-multi-fdset {multi curl-multi?/alive}
			   {read-fds	(or not general-c-sticky-buffer?)}
			   {write-fds	(or not general-c-sticky-buffer?)}
			   {exc-fds	(or not general-c-sticky-buffer?)})
  (let ((rv (capi::curl-multi-fdset multi read-fds write-fds exc-fds)))
    (values ($car rv)
	    ($cdr rv))))

(case-define* curl-multi-socket-action
  ((multi sock-fd)
   (curl-multi-socket-action multi sock-fd 0))
  (({multi curl-multi?/alive} {sock-fd action-socket-descriptor?} {ev-bitmask words::signed-int?})
   (let ((rv (capi::curl-multi-socket-action multi sock-fd ev-bitmask)))
     (values ($car rv)
	     ($cdr rv)))))

(define* (curl-multi-socket {multi curl-multi?/alive} {sock-fd file-descriptor?})
  ;;This is deprecated.
  ;;
  (let ((rv (capi::curl-multi-socket multi sock-fd)))
    (values ($car rv)
	    ($cdr rv))))

(define* (curl-multi-socket-all {multi curl-multi?/alive})
  ;;This is deprecated.
  ;;
  (capi::curl-multi-socket-all multi))

;;; --------------------------------------------------------------------

(define* (curl-multi-perform {multi curl-multi?/alive})
  (let ((rv (capi::curl-multi-perform multi)))
    (values ($car rv)
	    ($cdr rv))))

;;; --------------------------------------------------------------------

(define* (curl-multi-timeout {multi curl-multi?/alive})
  (let ((rv (capi::curl-multi-timeout multi)))
    (if (pair? rv)
	(values ($car rv)
		($cdr rv))
      (values rv #f))))

(define* (curl-multi-assign {multi curl-multi?/alive} {sock file-descriptor?} {custom-data false-or-pointer?})
  (capi::curl-multi-assign multi sock custom-data))

;;; --------------------------------------------------------------------

(structs::define-struct curl-waitfd
  (fd
		;Fixnum representing a socket descriptor.
   events
		;A fixnum representing a "short int" bitmask of events.
   revents
		;A fixnum representing a "short int" bitmask of events.
   ))

(define* (curl-multi-wait {multi curl-multi?/alive} {extra-fds false-or-vector-of-curl-waitfd?} {timeout words::signed-int?})
  (let ((rv (capi::curl-multi-wait multi extra-fds timeout)))
    (values ($car rv) ($cdr rv))))

;;; --------------------------------------------------------------------

(define* (curl-multi-info-read {multi curl-multi?/alive})
  (let ((rv (capi::curl-multi-info-read multi)))
    (values ($car rv) ($cdr rv))))

(define* (curl-multi-strerror {code words::signed-int?})
  (let ((rv (capi::curl-multi-strerror code)))
    (and rv (ascii->string rv))))


;;;; miscellaneous functions

(define* (curl-free {pointer false-or-pointer?})
  (capi::curl-free pointer)
  (values))

(define* (curl-getdate {date general-c-string?})
  (with-general-c-strings
      ((date^ date))
    (capi::curl-getdate date^)))

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_sockaddr"

(%define-raw-struct-accessor curl-sockaddr.family	capi::curl-sockaddr.family)
(%define-raw-struct-accessor curl-sockaddr.socktype	capi::curl-sockaddr.socktype)
(%define-raw-struct-accessor curl-sockaddr.protocol	capi::curl-sockaddr.protocol)
(%define-raw-struct-accessor curl-sockaddr.addrlen	capi::curl-sockaddr.addrlen)
(%define-raw-struct-accessor curl-sockaddr.addr		capi::curl-sockaddr.addr)

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_fileinfo"

(structs::define-struct curl-fileinfo
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

(define (pointer->curl-fileinfo stru)
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

(%define-raw-struct-accessor curl-fileinfo.filename	capi::curl-fileinfo.filename)
(%define-raw-struct-accessor curl-fileinfo.filetype	capi::curl-fileinfo.filetype)
(%define-raw-struct-accessor curl-fileinfo.time		capi::curl-fileinfo.time)
(%define-raw-struct-accessor curl-fileinfo.perm		capi::curl-fileinfo.perm)
(%define-raw-struct-accessor curl-fileinfo.uid		capi::curl-fileinfo.uid)
(%define-raw-struct-accessor curl-fileinfo.gid		capi::curl-fileinfo.gid)
(%define-raw-struct-accessor curl-fileinfo.size		capi::curl-fileinfo.size)
(%define-raw-struct-accessor curl-fileinfo.hardlinks	capi::curl-fileinfo.hardlinks)
(%define-raw-struct-accessor curl-fileinfo.strings.time	capi::curl-fileinfo.strings.time)
(%define-raw-struct-accessor curl-fileinfo.strings.perm	capi::curl-fileinfo.strings.perm)
(%define-raw-struct-accessor curl-fileinfo.strings.user	capi::curl-fileinfo.strings.user)
(%define-raw-struct-accessor curl-fileinfo.strings.group capi::curl-fileinfo.strings.group)
(%define-raw-struct-accessor curl-fileinfo.strings.target capi::curl-fileinfo.strings.target)
(%define-raw-struct-accessor curl-fileinfo.flags	capi::curl-fileinfo.flags)

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_khkey"

(define* (curl-khkey.key {stru pointer?})
  (let ((rv (capi::curl-khkey.key stru)))
    (values ($car rv) ($cdr rv))))

;;; --------------------------------------------------------------------
;;; accessors and mutators for "struct curl_forms" arrays

(define* (curl-forms-sizeof-array {number-of-structs non-negative-fixnum?})
  (capi::curl-forms-sizeof-array number-of-structs))

(define* (curl-forms.option {pointer general-c-buffer?} {index non-negative-fixnum?})
  (capi::curl-forms.option pointer index))

(define* (curl-forms.value {pointer general-c-buffer?} {index non-negative-fixnum?})
  (capi::curl-forms.value pointer index))

(define* (curl-forms.option-set! {pointer general-c-buffer?} {index non-negative-fixnum?} {value words::signed-int?})
  (let ((rv (capi::curl-forms.option-set! pointer index value)))
    (and rv (ascii->string rv))))

(define* (curl-forms.value-set! {pointer general-c-buffer?} {index non-negative-fixnum?} {value pointer-or-memory-block?})
  (capi::curl-forms.value-set! pointer index value))

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_certinfo"

(define* (curl-certinfo.certinfo {pointer pointer?})
  (let ((rv (capi::curl-certinfo.certinfo pointer)))
    (vector-map (lambda ({_ <top>} slist)
		  (curl-slist->list slist))
      rv)))

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_tlssessioninfo"

(define* (curl-tlssessioninfo.backend {pointer pointer?})
  (capi::curl-tlssessioninfo.backend pointer))

(define* (curl-tlssessioninfo.internals {pointer pointer?})
  (capi::curl-tlssessioninfo.internals pointer))

;;; --------------------------------------------------------------------
;;; accessors for "struct CURLMsg"

(%define-raw-struct-accessor curl-msg.msg		capi::curl-msg.msg)
(%define-raw-struct-accessor curl-msg.data.whatever	capi::curl-msg.data.whatever)
(%define-raw-struct-accessor curl-msg.data.result	capi::curl-msg.data.result)

(define* (curl-msg.easy_handle {stru pointer?})
  ;;This struct instance has no collector.
  (make-curl-easy/owner (capi::curl-msg.easy_handle stru) #f))


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
  (let ((maker (ffi::make-c-callback-maker 'size_t '(pointer size_t size_t pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (buffer size nitems outstream)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback buffer size nitems
				       (%cdata outstream))))))))

(define make-curl-read-callback
  ;; size_t curl_read_callback (char *buffer, size_t size, size_t nitems, void *instream)
  (let ((maker (ffi::make-c-callback-maker 'size_t '(pointer size_t size_t pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (buffer size nitems instream)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_READFUNC_ABORT))
		 (user-scheme-callback buffer size nitems
				       (%cdata instream))))))))

(define make-curl-ioctl-callback
  ;; curlioerr curl_ioctl_callback (CURL *handle, int cmd, void *clientp)
  (let ((maker (ffi::make-c-callback-maker 'signed-int '(pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle cmd custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURLIOE_UNKNOWNCMD))
		 (user-scheme-callback (make-curl-easy/not-owner handle #f)
				       cmd
				       (%cdata custom-data))))))))

(define make-curl-seek-callback
  ;; int curl_seek_callback (void *instream, curl_off_t offset, int origin)
  (let ((maker (ffi::make-c-callback-maker 'signed-int '(pointer off_t signed-int))))
    (lambda (user-scheme-callback)
      (maker (lambda (instream offset origin)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_SEEKFUNC_FAIL))
		 (user-scheme-callback (%cdata instream)
				       offset origin)))))))

(define make-curl-socket-option-callback
  ;; int curl_sockopt_callback (void *clientp, curl_socket_t curlfd, curlsocktype purpose)
  (let ((maker (ffi::make-c-callback-maker 'signed-int '(pointer signed-int signed-int))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data curlfd purpose)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_SOCKOPT_ERROR))
		 (user-scheme-callback (%cdata custom-data) curlfd purpose)))))))

(define make-curl-open-socket-callback
  ;; curl_socket_t curl_opensocket_callback (void *clientp, curlsocktype purpose,
  ;;                                         struct curl_sockaddr *address)
  (let ((maker (ffi::make-c-callback-maker 'signed-int '(pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data purpose address)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_SOCKET_BAD))
		 (user-scheme-callback (%cdata custom-data) purpose address)))))))

(define make-curl-close-socket-callback
  ;; int curl_close-socket_callback (void *clientp, curl_socket_t item)
  (let ((maker (ffi::make-c-callback-maker 'signed-int '(pointer signed-int))))
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
  (let ((maker (ffi::make-c-callback-maker 'signed-int '(pointer double double double double))))
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
  (let ((maker (ffi::make-c-callback-maker 'size_t '(pointer size_t size_t pointer))))
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
  (let ((maker (ffi::make-c-callback-maker 'signed-int
					  '(pointer signed-int pointer size_t pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle type data size custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback (make-curl-easy/not-owner handle #f)
				       type data size
				       (%cdata custom-data))))))))

(define make-curl-ssl-ctx-callback
  ;; CURLcode curl_ssl_ctx_callback (CURL *curl, void *ssl_ctx, void *userptr)
  (let ((maker (ffi::make-c-callback-maker 'signed-int '(pointer pointer pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle ssl-ctx custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURLE_ABORTED_BY_CALLBACK))
		 (user-scheme-callback (make-curl-easy/not-owner handle #f)
				       ssl-ctx (%cdata custom-data))))))))

;;; --------------------------------------------------------------------

(define make-curl-conv-to-network-callback
  ;; CURLcode callback (void * buffer, size_t size)
  (let ((maker (ffi::make-c-callback-maker 'signed-int '(pointer size_t))))
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
  (let ((maker (ffi::make-c-callback-maker 'size_t '(pointer size_t size_t pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (buffer size nmemb custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback buffer size nmemb (%cdata custom-data))))))))

(define make-curl-chunk-begin-callback
  ;; long curl_chunk_bgn_callback (const void *transfer_info, void *ptr, int remains)
  (let ((maker (ffi::make-c-callback-maker 'signed-long '(pointer pointer signed-int))))
    (lambda (user-scheme-callback)
      (maker (lambda (transfer-info custom-data remains)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_CHUNK_BGN_FUNC_FAIL))
		 (user-scheme-callback transfer-info (%cdata custom-data)
				       remains)))))))

(define make-curl-chunk-end-callback
  ;;long curl_chunk_end_callback (void *ptr)
  (let ((maker (ffi::make-c-callback-maker 'signed-long '(pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURL_CHUNK_END_FUNC_FAIL))
		 (user-scheme-callback (%cdata custom-data))))))))

(define make-curl-fnmatch-callback
  ;; int curl_fnmatch_callback (void *ptr, const char *pattern, const char *string)
  (let ((maker (ffi::make-c-callback-maker 'signed-int '(pointer pointer pointer))))
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
  (let ((maker (ffi::make-c-callback-maker 'signed-int
					  '(pointer pointer pointer signed-int pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle knownkey foundkey khmatch custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  CURLKHSTAT_REJECT))
		 (user-scheme-callback (make-curl-easy/not-owner handle #f)
				       (%cdata knownkey) foundkey khmatch
				       (%cdata custom-data))))))))


;;;; callback makers: multi API

(define make-curl-socket-callback
  ;; int curl_socket_callback (CURL *easy, curl_socket_t s, int what, void *userp,
  ;;                           void *socketp)
  (let ((maker (ffi::make-c-callback-maker 'signed-int
					  '(pointer signed-int signed-int pointer pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle sock what callback-custom-data socket-custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback (make-curl-easy/not-owner handle #f)
				       sock what
				       (%cdata callback-custom-data)
				       (%cdata socket-custom-data))
		 0))))))

(define make-curl-multi-timer-callback
  ;; int curl_multi_timer_callback (CURLM *multi, long timeout_ms, void *userp)
  (let ((maker (ffi::make-c-callback-maker 'signed-int '(pointer signed-long pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (handle timeout-ms custom-data)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback (make-curl-multi/not-owner handle)
				       timeout-ms (%cdata custom-data))))))))

(define make-curl-xferinfo-callback
  ;; int curl_xferinfo_callback (void *clientp,
  ;;                             curl_off_t dltotal, curl_off_t dlnow,
  ;;                             curl_off_t ultotal, curl_off_t ulnow);
  (let ((maker (ffi::make-c-callback-maker 'signed-int '(pointer off_t off_t off_t off_t))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data dltotal dlnow ultotal ulnow)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  0))
		 (user-scheme-callback (%cdata custom-data) dltotal dlnow ultotal ulnow)))))))


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
   CURLSOCKTYPE_ACCEPT
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

(define-exact-integer->symbol-function curl-constant-wait-poll->symbol
  (CURL_WAIT_POLLIN
   CURL_WAIT_POLLPRI
   CURL_WAIT_POLLOUT))

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

(structs::set-struct-type-printer! (type-descriptor curl-version-info-data)
  %struct-curl-version-info-data-printer)
(structs::set-struct-type-printer! (type-descriptor curl-form-data)	%struct-curl-form-data-printer)
(structs::set-struct-type-printer! (type-descriptor curl-share)		%struct-curl-share-printer)
(structs::set-struct-type-printer! (type-descriptor curl-easy)		%struct-curl-easy-printer)
(structs::set-struct-type-printer! (type-descriptor curl-multi)		%struct-curl-multi-printer)

#| end of library |# )

;;; end of file
;;Local Variables:
;;eval: (put '%define-raw-struct-accessor 'scheme-indent-function 1)
;;End:
