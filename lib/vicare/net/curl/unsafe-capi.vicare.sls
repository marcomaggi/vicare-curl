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
;;;Copyright (C) 2012, 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    curl-multi-socket-all		curl-multi-wait
    curl-multi-timeout			curl-multi-assign
    curl-multi-strerror)
  (import (vicare))


;;;; version functions

(define-syntax-rule (vicare-curl-version-interface-current)
  (foreign-call "ikrt_vicare_curl_version_interface_current"))

(define-syntax-rule (vicare-curl-version-interface-revision)
  (foreign-call "ikrt_vicare_curl_version_interface_revision"))

(define-syntax-rule (vicare-curl-version-interface-age)
  (foreign-call "ikrt_vicare_curl_version_interface_age"))

(define-syntax-rule (vicare-curl-version)
  (foreign-call "ikrt_vicare_curl_version"))

;;; --------------------------------------------------------------------

(define-syntax-rule (curl-version)
  (foreign-call "ikrt_curl_version"))

(define-syntax-rule (curl-version-info rtd version-code)
  (foreign-call "ikrt_curl_version_info" rtd version-code))


;;;; initialisation functions

(define-syntax-rule (curl-global-init flags)
  (foreign-call "ikrt_curl_global_init" flags))

(define-syntax-rule (curl-global-init-mem flags
				     malloc-callback free-callback realloc-callback
				     strdup-callback calloc-callback)
  (foreign-call "ikrt_curl_global_init_mem" flags
		malloc-callback free-callback realloc-callback
		strdup-callback calloc-callback))

(define-syntax-rule (curl-global-cleanup)
  (foreign-call "ikrt_curl_global_cleanup"))


;;;; string lists

(define-syntax-rule (curl-slist-append slist string)
  (foreign-call "ikrt_curl_slist_append" slist string))

(define-syntax-rule (curl-slist-free-all slist)
  (foreign-call "ikrt_curl_slist_free_all" slist))

(define-syntax-rule (curl-slist->list slist)
  (foreign-call "ikrt_curl_slist_to_bytevectors" slist))


;;;; multipart/formdata composition

(define-syntax-rule (curl-formadd-1 post last-item opt val)
  (foreign-call "ikrt_curl_formadd_1" post last-item opt val))

(define-syntax-rule (curl-formadd-2 post last-item
			       opt1 val1
			       opt2 val2)
  (foreign-call "ikrt_curl_formadd_2" post last-item
		opt1 val1
		opt2 val2))

(define-syntax-rule (curl-formadd-3 post last-item
			       opt1 val1
			       opt2 val2
			       opt3 val3)
  (foreign-call "ikrt_curl_formadd_3" post last-item
		opt1 val1
		opt2 val2
		opt3 val3))

(define-syntax-rule (curl-formadd-4 post last-item
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

(define-syntax-rule (curl-formget post custom-data callback)
  (foreign-call "ikrt_curl_formget" post custom-data callback))

(define-syntax-rule (curl-formfree post)
  (foreign-call "ikrt_curl_formfree" post))


;;;; basic URL string escaping

(define-syntax-rule (curl-escape str.data str.len)
  (foreign-call "ikrt_curl_escape" str.data str.len))

(define-syntax-rule (curl-unescape str.data str.len)
  (foreign-call "ikrt_curl_unescape" str.data str.len))


;;;; shared configuration option sets

(define-syntax-rule (curl-share-init)
  (foreign-call "ikrt_curl_share_init"))

(define-syntax-rule (curl-share-setopt share option parameter)
  (foreign-call "ikrt_curl_share_setopt" share option parameter))

(define-syntax-rule (curl-share-cleanup share)
  (foreign-call "ikrt_curl_share_cleanup" share))

(define-syntax-rule (curl-share-strerror errcode)
  (foreign-call "ikrt_curl_share_strerror" errcode))


;;;; miscellaneous functions

(define-syntax-rule (curl-free ptr)
  (foreign-call "ikrt_curl_free" ptr))

(define-syntax-rule (curl-getdate date)
  (foreign-call "ikrt_curl_getdate" date))

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_sockaddr"

(define-syntax-rule (curl-sockaddr.family pointer)
  (foreign-call "ikrt_curl_sockaddr_family" pointer))

(define-syntax-rule (curl-sockaddr.socktype pointer)
  (foreign-call "ikrt_curl_sockaddr_socktype" pointer))

(define-syntax-rule (curl-sockaddr.protocol pointer)
  (foreign-call "ikrt_curl_sockaddr_protocol" pointer))

(define-syntax-rule (curl-sockaddr.addrlen pointer)
  (foreign-call "ikrt_curl_sockaddr_addrlen" pointer))

(define-syntax-rule (curl-sockaddr.addr pointer)
  (foreign-call "ikrt_curl_sockaddr_addr" pointer))

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_fileinfo"

(define-syntax-rule (curl-fileinfo.filename struct)
  (foreign-call "ikrt_curl_fileinfo_filename" struct))

(define-syntax-rule (curl-fileinfo.filetype struct)
  (foreign-call "ikrt_curl_fileinfo_filetype" struct))

(define-syntax-rule (curl-fileinfo.time struct)
  (foreign-call "ikrt_curl_fileinfo_time" struct))

(define-syntax-rule (curl-fileinfo.perm struct)
  (foreign-call "ikrt_curl_fileinfo_perm" struct))

(define-syntax-rule (curl-fileinfo.uid struct)
  (foreign-call "ikrt_curl_fileinfo_uid" struct))

(define-syntax-rule (curl-fileinfo.gid struct)
  (foreign-call "ikrt_curl_fileinfo_gid" struct))

(define-syntax-rule (curl-fileinfo.size struct)
  (foreign-call "ikrt_curl_fileinfo_size" struct))

(define-syntax-rule (curl-fileinfo.hardlinks struct)
  (foreign-call "ikrt_curl_fileinfo_hardlinks" struct))

(define-syntax-rule (curl-fileinfo.strings.time struct)
  (foreign-call "ikrt_curl_fileinfo_strings_time" struct))

(define-syntax-rule (curl-fileinfo.strings.perm struct)
  (foreign-call "ikrt_curl_fileinfo_strings_perm" struct))

(define-syntax-rule (curl-fileinfo.strings.user struct)
  (foreign-call "ikrt_curl_fileinfo_strings_user" struct))

(define-syntax-rule (curl-fileinfo.strings.group struct)
  (foreign-call "ikrt_curl_fileinfo_strings_group" struct))

(define-syntax-rule (curl-fileinfo.strings.target struct)
  (foreign-call "ikrt_curl_fileinfo_strings_target" struct))

(define-syntax-rule (curl-fileinfo.flags struct)
  (foreign-call "ikrt_curl_fileinfo_flags" struct))

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_khkey"

(define-syntax-rule (curl-khkey.key struct)
  (foreign-call "ikrt_curl_khkey_key" struct))

;;; --------------------------------------------------------------------
;;; accessors and mutators for "struct curl_forms" arrays

(define-syntax-rule (curl-forms-sizeof-array number-of-structs)
  (foreign-call "ikrt_curl_forms_sizeof" number-of-structs))

(define-syntax-rule (curl-forms.option array index)
  (foreign-call "ikrt_curl_forms_option" array index))

(define-syntax-rule (curl-forms.option-set! array index value)
  (foreign-call "ikrt_curl_forms_option_set" array index value))

(define-syntax-rule (curl-forms.value array index)
  (foreign-call "ikrt_curl_forms_value" array index))

(define-syntax-rule (curl-forms.value-set! array index value)
  (foreign-call "ikrt_curl_forms_value_set" array index value))

;;; --------------------------------------------------------------------
;;; accessors for "struct curl_certinfo"

(define-syntax-rule (curl-certinfo.certinfo struct)
  (foreign-call "ikrt_curl_certinfo_certinfo" struct))

;;; --------------------------------------------------------------------
;;; accessors for "struct CURLMsg"

(define-syntax-rule (curl-msg.msg struct)
  (foreign-call "ikrt_curl_msg_msg" struct))

(define-syntax-rule (curl-msg.easy_handle struct)
  (foreign-call "ikrt_curl_msg_easy_handle" struct))

(define-syntax-rule (curl-msg.data.whatever struct)
  (foreign-call "ikrt_curl_msg_data_whatever" struct))

(define-syntax-rule (curl-msg.data.result struct)
  (foreign-call "ikrt_curl_msg_data_result" struct))


;;;; easy API

(define-syntax-rule (curl-easy-init)
  (foreign-call "ikrt_curl_easy_init"))

(define-syntax-rule (curl-easy-cleanup easy)
  (foreign-call "ikrt_curl_easy_cleanup" easy))

(define-syntax-rule (curl-easy-reset easy)
  (foreign-call "ikrt_curl_easy_reset" easy))

;;; --------------------------------------------------------------------

(define-syntax-rule (curl-easy-setopt easy option parameter)
  (foreign-call "ikrt_curl_easy_setopt" easy option parameter))

(define-syntax-rule (curl-easy-getinfo easy info)
  (foreign-call "ikrt_curl_easy_getinfo" easy info))

;;; --------------------------------------------------------------------

(define-syntax-rule (curl-easy-perform easy)
  (foreign-call "ikrt_curl_easy_perform" easy))

(define-syntax-rule (curl-easy-duphandle easy)
  (foreign-call "ikrt_curl_easy_duphandle" easy))

(define-syntax-rule (curl-easy-pause easy bitmask)
  (foreign-call "ikrt_curl_easy_pause" easy bitmask))

;;; --------------------------------------------------------------------

(define-syntax-rule (curl-easy-recv easy buffer.data buffer.len)
  (foreign-call "ikrt_curl_easy_recv" easy buffer.data buffer.len))

(define-syntax-rule (curl-easy-send easy buffer.data buffer.len)
  (foreign-call "ikrt_curl_easy_send" easy buffer.data buffer.len))

;;; --------------------------------------------------------------------

(define-syntax-rule (curl-easy-escape easy chars.data chars.len)
  (foreign-call "ikrt_curl_easy_escape" easy chars.data chars.len))

(define-syntax-rule (curl-easy-unescape easy chars.data chars.len)
  (foreign-call "ikrt_curl_easy_unescape" easy chars.data chars.len))

;;; --------------------------------------------------------------------

(define-syntax-rule (curl-easy-strerror code)
  (foreign-call "ikrt_curl_easy_strerror" code))


;;;; multi API

(define-syntax-rule (curl-multi-init)
  (foreign-call "ikrt_curl_multi_init"))

(define-syntax-rule (curl-multi-cleanup multi)
  (foreign-call "ikrt_curl_multi_cleanup" multi))

;;; --------------------------------------------------------------------

(define-syntax-rule (curl-multi-add-handle multi easy)
  (foreign-call "ikrt_curl_multi_add_handle" multi easy))

(define-syntax-rule (curl-multi-remove-handle multi easy)
  (foreign-call "ikrt_curl_multi_remove_handle" multi easy))

;;; --------------------------------------------------------------------

(define-syntax-rule (curl-multi-setopt multi option parameter)
  (foreign-call "ikrt_curl_multi_setopt" multi option parameter))

;;; --------------------------------------------------------------------

(define-syntax-rule (curl-multi-fdset multi read-fds write-fds exc-fds)
  (foreign-call "ikrt_curl_multi_fdset" multi read-fds write-fds exc-fds))

(define-syntax-rule (curl-multi-socket-action multi sock-fd ev-bitmask)
  (foreign-call "ikrt_curl_multi_socket_action" multi sock-fd ev-bitmask))

(define-syntax-rule (curl-multi-timeout multi)
  (foreign-call "ikrt_curl_multi_timeout" multi))

;;;This is deprecated.
(define-syntax-rule (curl-multi-socket multi sock-fd)
  (foreign-call "ikrt_curl_multi_socket" multi sock-fd))

;;;This is deprecated.
(define-syntax-rule (curl-multi-socket-all multi)
  (foreign-call "ikrt_curl_multi_socket_all" multi))

;;; --------------------------------------------------------------------

(define-syntax-rule (curl-multi-perform multi)
  (foreign-call "ikrt_curl_multi_perform" multi))

;;; --------------------------------------------------------------------

(define-syntax-rule (curl-multi-assign multi sock custom-data)
  (foreign-call "ikrt_curl_multi_assign" multi sock custom-data))

;;; --------------------------------------------------------------------

(define-syntax-rule (curl-multi-wait multi extra-fds timeout)
  (foreign-call "ikrt_curl_multi_wait" multi extra-fds timeout))

;;; --------------------------------------------------------------------

(define-syntax-rule (curl-multi-info-read multi)
  (foreign-call "ikrt_curl_multi_info_read" multi))

(define-syntax-rule (curl-multi-strerror code)
  (foreign-call "ikrt_curl_multi_strerror" code))


;;;; done

)

;;; end of file
