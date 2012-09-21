;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/cURL
;;;Contents: unsafe interface to the C language API
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare net curl unsafe-capi)
  (export

    ;; version functions
    vicare-curl-version-interface-current
    vicare-curl-version-interface-revision
    vicare-curl-version-interface-age
    vicare-curl-version
    curl-version			curl-version-info

    ;; initialisation functions
    curl-global-init			curl-global-init-mem
    curl-global-cleanup

    ;; string lists
    curl-slist-append			curl-slist-free-all
    curl-slist->list

    ;; multipart/formdata composition
    curl-formadd-1			curl-formadd-2
    curl-formadd-3			curl-formadd-4
    curl-formget			curl-formfree

    ;; basic URL string escaping
    curl-escape				curl-unescape

    ;; shared configuration option sets
    curl-share-init			curl-share-cleanup
    curl-share-setopt			curl-share-strerror

    ;; miscellaneous functions
    curl-free				curl-getdate

    ;; accessors for "struct curl_sockaddr"
    curl-sockaddr.family		curl-sockaddr.socktype
    curl-sockaddr.protocol		curl-sockaddr.addrlen
    curl-sockaddr.addr

    ;; accessors for "struct curl_fileinfo"
    curl-fileinfo.filename		curl-fileinfo.filetype
    curl-fileinfo.time			curl-fileinfo.perm
    curl-fileinfo.uid			curl-fileinfo.gid
    curl-fileinfo.size			curl-fileinfo.hardlinks
    curl-fileinfo.strings.time		curl-fileinfo.strings.perm
    curl-fileinfo.strings.user		curl-fileinfo.strings.group
    curl-fileinfo.strings.target	curl-fileinfo.flags

    ;; accessors for "struct curl_khkey"
    curl-khkey.key

    ;; accessors and mutators for "struct curl_forms" arrays
    curl-forms-sizeof-array
    curl-forms.option			curl-forms.option-set!
    curl-forms.value			curl-forms.value-set!

    ;; accessors for "struct curl_certinfo"
    curl-certinfo.certinfo

    ;; accessors for "struct CURLMsg"
    curl-msg.msg			curl-msg.easy_handle
    curl-msg.data.whatever		curl-msg.data.result

    ;; easy API
    curl-easy-init			curl-easy-cleanup
    curl-easy-reset
    curl-easy-setopt			curl-easy-getinfo
    curl-easy-perform			curl-easy-duphandle
    curl-easy-recv			curl-easy-send
    curl-easy-strerror			curl-easy-pause
    curl-easy-escape			curl-easy-unescape

    ;; multi API
    curl-multi-init			curl-multi-cleanup
    curl-multi-add-handle		curl-multi-remove-handle
    curl-multi-setopt			curl-multi-fdset
    curl-multi-perform			curl-multi-info-read
    curl-multi-socket			curl-multi-socket-action
    curl-multi-socket-all
    curl-multi-timeout			curl-multi-assign
    curl-multi-strerror)
  (import (vicare))


;;;; helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))


;;;; version functions

(define-inline (vicare-curl-version-interface-current)
  (foreign-call "ikrt_vicare_curl_version_interface_current"))

(define-inline (vicare-curl-version-interface-revision)
  (foreign-call "ikrt_vicare_curl_version_interface_revision"))

(define-inline (vicare-curl-version-interface-age)
  (foreign-call "ikrt_vicare_curl_version_interface_age"))

(define-inline (vicare-curl-version)
  (foreign-call "ikrt_vicare_curl_version"))

;;; --------------------------------------------------------------------

(define-inline (curl-version)
  (foreign-call "ikrt_curl_version"))

(define-inline (curl-version-info rtd version-code)
  (foreign-call "ikrt_curl_version_info" rtd version-code))


;;;; initialisation functions

(define-inline (curl-global-init flags)
  (foreign-call "ikrt_curl_global_init" flags))

(define-inline (curl-global-init-mem flags
				     malloc-callback free-callback realloc-callback
				     strdup-callback calloc-callback)
  (foreign-call "ikrt_curl_global_init_mem" flags
		malloc-callback free-callback realloc-callback
		strdup-callback calloc-callback))

(define-inline (curl-global-cleanup)
  (foreign-call "ikrt_curl_global_cleanup"))


;;;; string lists

(define-inline (curl-slist-append slist string)
  (foreign-call "ikrt_curl_slist_append" slist string))

(define-inline (curl-slist-free-all slist)
  (foreign-call "ikrt_curl_slist_free_all" slist))

(define-inline (curl-slist->list slist)
  (foreign-call "ikrt_curl_slist_to_bytevectors" slist))


;;;; multipart/formdata composition

(define-inline (curl-formadd-1 post last-item opt val)
  (foreign-call "ikrt_curl_formadd_1" post last-item opt val))

(define-inline (curl-formadd-2 post last-item
			       opt1 val1
			       opt2 val2)
  (foreign-call "ikrt_curl_formadd_2" post last-item
		opt1 val1
		opt2 val2))

(define-inline (curl-formadd-3 post last-item
			       opt1 val1
			       opt2 val2
			       opt3 val3)
  (foreign-call "ikrt_curl_formadd_3" post last-item
		opt1 val1
		opt2 val2
		opt3 val3))

(define-inline (curl-formadd-4 post last-item
			       opt1 val1
			       opt2 val2
			       opt3 val3
			       opt4 val4)
  (foreign-call "ikrt_curl_formadd_4" post last-item
		opt1 val1
		opt2 val2
		opt3 val3
		opt4 val4))

;;; --------------------------------------------------------------------

(define-inline (curl-formget post custom-data callback)
  (foreign-call "ikrt_curl_formget" post custom-data callback))

(define-inline (curl-formfree post)
  (foreign-call "ikrt_curl_formfree" post))


;;;; basic URL string escaping

(define-inline (curl-escape str.data str.len)
  (foreign-call "ikrt_curl_escape" str.data str.len))

(define-inline (curl-unescape str.data str.len)
  (foreign-call "ikrt_curl_unescape" str.data str.len))


;;;; shared configuration option sets

(define-inline (curl-share-init)
  (foreign-call "ikrt_curl_share_init"))

(define-inline (curl-share-setopt share option parameter)
  (foreign-call "ikrt_curl_share_setopt" share option parameter))

(define-inline (curl-share-cleanup share)
  (foreign-call "ikrt_curl_share_cleanup" share))

(define-inline (curl-share-strerror errcode)
  (foreign-call "ikrt_curl_share_strerror" errcode))


;;;; miscellaneous functions

(define-inline (curl-free ptr)
  (foreign-call "ikrt_curl_free" ptr))

(define-inline (curl-getdate date)
  (foreign-call "ikrt_curl_getdate" date))

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_sockaddr"

(define-inline (curl-sockaddr.family pointer)
  (foreign-call "ikrt_curl_sockaddr_family" pointer))

(define-inline (curl-sockaddr.socktype pointer)
  (foreign-call "ikrt_curl_sockaddr_socktype" pointer))

(define-inline (curl-sockaddr.protocol pointer)
  (foreign-call "ikrt_curl_sockaddr_protocol" pointer))

(define-inline (curl-sockaddr.addrlen pointer)
  (foreign-call "ikrt_curl_sockaddr_addrlen" pointer))

(define-inline (curl-sockaddr.addr pointer)
  (foreign-call "ikrt_curl_sockaddr_addr" pointer))

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_fileinfo"

(define-inline (curl-fileinfo.filename struct)
  (foreign-call "ikrt_curl_fileinfo_filename" struct))

(define-inline (curl-fileinfo.filetype struct)
  (foreign-call "ikrt_curl_fileinfo_filetype" struct))

(define-inline (curl-fileinfo.time struct)
  (foreign-call "ikrt_curl_fileinfo_time" struct))

(define-inline (curl-fileinfo.perm struct)
  (foreign-call "ikrt_curl_fileinfo_perm" struct))

(define-inline (curl-fileinfo.uid struct)
  (foreign-call "ikrt_curl_fileinfo_uid" struct))

(define-inline (curl-fileinfo.gid struct)
  (foreign-call "ikrt_curl_fileinfo_gid" struct))

(define-inline (curl-fileinfo.size struct)
  (foreign-call "ikrt_curl_fileinfo_size" struct))

(define-inline (curl-fileinfo.hardlinks struct)
  (foreign-call "ikrt_curl_fileinfo_hardlinks" struct))

(define-inline (curl-fileinfo.strings.time struct)
  (foreign-call "ikrt_curl_fileinfo_strings_time" struct))

(define-inline (curl-fileinfo.strings.perm struct)
  (foreign-call "ikrt_curl_fileinfo_strings_perm" struct))

(define-inline (curl-fileinfo.strings.user struct)
  (foreign-call "ikrt_curl_fileinfo_strings_user" struct))

(define-inline (curl-fileinfo.strings.group struct)
  (foreign-call "ikrt_curl_fileinfo_strings_group" struct))

(define-inline (curl-fileinfo.strings.target struct)
  (foreign-call "ikrt_curl_fileinfo_strings_target" struct))

(define-inline (curl-fileinfo.flags struct)
  (foreign-call "ikrt_curl_fileinfo_flags" struct))

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_khkey"

(define-inline (curl-khkey.key struct)
  (foreign-call "ikrt_curl_khkey_key" struct))

;;; --------------------------------------------------------------------
;;; accessors and mutators for "struct curl_forms" arrays

(define-inline (curl-forms-sizeof-array number-of-structs)
  (foreign-call "ikrt_curl_forms_sizeof" number-of-structs))

(define-inline (curl-forms.option array index)
  (foreign-call "ikrt_curl_forms_option" array index))

(define-inline (curl-forms.option-set! array index value)
  (foreign-call "ikrt_curl_forms_option_set" array index value))

(define-inline (curl-forms.value array index)
  (foreign-call "ikrt_curl_forms_value" array index))

(define-inline (curl-forms.value-set! array index value)
  (foreign-call "ikrt_curl_forms_value_set" array index value))

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_certinfo"

(define-inline (curl-certinfo.certinfo struct)
  (foreign-call "ikrt_curl_certinfo_certinfo" struct))

;;; --------------------------------------------------------------------
;;; accessors for "struct CURLMsg"

(define-inline (curl-msg.msg struct)
  (foreign-call "ikrt_curl_msg_msg" struct))

(define-inline (curl-msg.easy_handle struct)
  (foreign-call "ikrt_curl_msg_easy_handle" struct))

(define-inline (curl-msg.data.whatever struct)
  (foreign-call "ikrt_curl_msg_data_whatever" struct))

(define-inline (curl-msg.data.result struct)
  (foreign-call "ikrt_curl_msg_data_result" struct))


;;;; easy API

(define-inline (curl-easy-init)
  (foreign-call "ikrt_curl_easy_init"))

(define-inline (curl-easy-cleanup easy)
  (foreign-call "ikrt_curl_easy_cleanup" easy))

(define-inline (curl-easy-reset easy)
  (foreign-call "ikrt_curl_easy_reset" easy))

;;; --------------------------------------------------------------------

(define-inline (curl-easy-setopt easy option parameter)
  (foreign-call "ikrt_curl_easy_setopt" easy option parameter))

(define-inline (curl-easy-getinfo easy info)
  (foreign-call "ikrt_curl_easy_getinfo" easy info))

;;; --------------------------------------------------------------------

(define-inline (curl-easy-perform easy)
  (foreign-call "ikrt_curl_easy_perform" easy))

(define-inline (curl-easy-duphandle easy)
  (foreign-call "ikrt_curl_easy_duphandle" easy))

(define-inline (curl-easy-pause easy bitmask)
  (foreign-call "ikrt_curl_easy_pause" easy bitmask))

;;; --------------------------------------------------------------------

(define-inline (curl-easy-recv easy buffer.data buffer.len)
  (foreign-call "ikrt_curl_easy_recv" easy buffer.data buffer.len))

(define-inline (curl-easy-send easy buffer.data buffer.len)
  (foreign-call "ikrt_curl_easy_send" easy buffer.data buffer.len))

;;; --------------------------------------------------------------------

(define-inline (curl-easy-escape easy chars.data chars.len)
  (foreign-call "ikrt_curl_easy_escape" easy chars.data chars.len))

(define-inline (curl-easy-unescape easy chars.data chars.len)
  (foreign-call "ikrt_curl_easy_unescape" easy chars.data chars.len))

;;; --------------------------------------------------------------------

(define-inline (curl-easy-strerror code)
  (foreign-call "ikrt_curl_easy_strerror" code))


;;;; multi API

(define-inline (curl-multi-init)
  (foreign-call "ikrt_curl_multi_init"))

(define-inline (curl-multi-cleanup multi)
  (foreign-call "ikrt_curl_multi_cleanup" multi))

;;; --------------------------------------------------------------------

(define-inline (curl-multi-add-handle multi easy)
  (foreign-call "ikrt_curl_multi_add_handle" multi easy))

(define-inline (curl-multi-remove-handle multi easy)
  (foreign-call "ikrt_curl_multi_remove_handle" multi easy))

;;; --------------------------------------------------------------------

(define-inline (curl-multi-setopt multi option parameter)
  (foreign-call "ikrt_curl_multi_setopt" multi option parameter))

;;; --------------------------------------------------------------------

(define-inline (curl-multi-fdset)
  (foreign-call "ikrt_curl_multi_fdset"))

(define-inline (curl-multi-socket)
  (foreign-call "ikrt_curl_multi_socket"))

(define-inline (curl-multi-socket-action)
  (foreign-call "ikrt_curl_multi_socket_action"))

(define-inline (curl-multi-socket-all)
  (foreign-call "ikrt_curl_multi_socket_all"))

(define-inline (curl-multi-timeout multi)
  (foreign-call "ikrt_curl_multi_timeout" multi))

;;; --------------------------------------------------------------------

(define-inline (curl-multi-perform multi)
  (foreign-call "ikrt_curl_multi_perform" multi))

;;; --------------------------------------------------------------------

(define-inline (curl-multi-assign multi sock custom-data)
  (foreign-call "ikrt_curl_multi_assign" multi sock custom-data))

;;; --------------------------------------------------------------------

(define-inline (curl-multi-info-read multi)
  (foreign-call "ikrt_curl_multi_info_read" multi))

(define-inline (curl-multi-strerror code)
  (foreign-call "ikrt_curl_multi_strerror" code))


;;;; done

)

;;; end of file
