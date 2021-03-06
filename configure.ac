dnl @configure_input@
dnl

AC_PREREQ([2.69])
AC_INIT([Vicare/cURL],[0.4d1],[marco.maggi-ipsu@poste.it],
  [vicare-curl],[http://marcomaggi.github.io/vicare.html])
AC_CONFIG_SRCDIR([src])
AC_CONFIG_MACRO_DIR([meta/autotools])
AC_CONFIG_AUX_DIR([meta/autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([foreign subdir-objects dist-xz no-dist-gzip])
AM_MAINTAINER_MODE

AC_PROG_CC
AC_PROG_CC_C_O
AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P
LT_PREREQ([2.4])
LT_INIT([shared disable-static])

VICARE_SCHEME
VICARE_CHECK_LIBRARY([VICARE],[(vicare (0 4 2017 1 (>= 10)))])
AS_IF([test "$vicare_cv_schemelib_VICARE" = no],
  [AC_MSG_ERROR([wrong Vicare version],1)])

VICARE_CHECK_LIBRARY([VICARE_FFI],
  [(vicare ffi (or (0 4 2015 5 (>= 28))
                   (0 4 2015 (>= 6))
                   (0 4 (>= 2016))))])
AS_IF([test "$vicare_cv_schemelib_VICARE_FFI" = no],
  [AC_MSG_ERROR([wrong Vicare FFI library version],1)])

# Command  line  options  for  the executable  "vicare".   The  variable
# AX_VFLAGS is  for options preselected by  the building infrastructure.
# The variable VFLAGS  is for user options selected on  the command line
# of "configure" and "make", for example:
#
#    $ make VFLAGS="-g --print-loaded-libraries"
#
AS_VAR_SET(AX_VFLAGS,["-O2 --verbose"])
AS_VAR_SET_IF(VFLAGS,,[AS_VAR_SET(VFLAGS)])

VICARE_OPTION_DEBUGGING_MODE

AM_CONDITIONAL([WANT_VICARE_DEBUG],[test x$VICARE_DEBUG = xyes])
AM_COND_IF([WANT_VICARE_DEBUG],[AS_VAR_SET(VFLAGS,"$VFLAGS -g")])

VICARE_ENABLE_OPTION([INSTALL_SOURCES],[sources-installation],[no],
  [whether to install source Scheme libraries],
  [enable installation of source Scheme libraries])
AM_CONDITIONAL([WANT_INSTALL_SOURCES],
  [test "x$vicare_enable_INSTALL_SOURCES" = xyes])

AC_LANG([C])
AC_DEFINE([_GNU_SOURCE],[1],[enable everything GNU])

VICARE_ENABLE_OPTION([TIME_TESTS],[time-tests],[no],
  [whether to enable timing of tests],
  [enable timing of tests])
AM_CONDITIONAL([WANT_TIME_TESTS],[test "x$vicare_enable_TIME_TESTS" = xyes])

AM_COND_IF([WANT_TIME_TESTS],
  [AC_PATH_PROG([TIME_PROGRAM],[time])])

#page
#### cURL configuration

AC_CHECK_HEADERS([curl/curl.h],,
  [AC_MSG_FAILURE([missing curl/curl.h header],[2])])
AC_CHECK_HEADERS([curl/easy.h],,
  [AC_MSG_FAILURE([missing curl/curl.h header],[2])],
  [#include <curl/curl.h>])
AC_CHECK_HEADERS([curl/multi.h],,
  [AC_MSG_FAILURE([missing curl/curl.h header],[2])],
  [#include <curl/curl.h>])
AC_CHECK_LIB([curl],[curl_free],,
  [AC_MSG_FAILURE([missing cURL library],[2])])

dnl AC_CHECK_HEADER([ioctl.h])
dnl AC_CHECK_HEADER([time.h])

VICARE_EXTENSION_LIBRARY_VERSION([curl],[0],[1],[0])

#page
#### symbols

m4_define([VICARE_INCLUDES],[
AC_INCLUDES_DEFAULT
#ifdef HAVE_VICARE_H
#  include <vicare.h>
#endif
#ifdef HAVE_CURL_CURL_H
#  include <curl/curl.h>
#endif
#ifdef HAVE_CURL_EASY_H
#  include <curl/easy.h>
#endif
#ifdef HAVE_CURL_MULTI_H
#  include <curl/multi.h>
#endif
])

dnl Version numbers.
VICARE_CONSTANT_TESTS([LIBCURL_VERSION_MAJOR LIBCURL_VERSION_MINOR LIBCURL_VERSION_PATCH
  LIBCURL_VERSION_NUM])
VICARE_STRING_CONSTANT_TESTS([LIBCURL_COPYRIGHT LIBCURL_VERSION LIBCURL_TIMESTAMP])

dnl Miscellaneous constants.
VICARE_CONSTANT_TESTS([CURL_SOCKET_BAD CURL_MAX_WRITE_SIZE CURL_MAX_HTTP_HEADER
  CURL_WRITEFUNC_PAUSE])

dnl
VICARE_CONSTANT_TESTS([CURL_HTTPPOST_FILENAME CURL_HTTPPOST_READFILE CURL_HTTPPOST_PTRNAME
  CURL_HTTPPOST_PTRCONTENTS CURL_HTTPPOST_BUFFER CURL_HTTPPOST_PTRBUFFER CURL_HTTPPOST_CALLBACK
  CURL_HTTPPOST_LARGE])

dnl enumeration of file types
VICARE_CONSTANT_TESTS([CURLFILETYPE_FILE CURLFILETYPE_DIRECTORY CURLFILETYPE_SYMLINK
  CURLFILETYPE_DEVICE_BLOCK CURLFILETYPE_DEVICE_CHAR CURLFILETYPE_NAMEDPIPE
  CURLFILETYPE_SOCKET CURLFILETYPE_DOOR CURLFILETYPE_UNKNOWN])

dnl
VICARE_CONSTANT_TESTS([CURLFINFOFLAG_KNOWN_FILENAME CURLFINFOFLAG_KNOWN_FILETYPE
  CURLFINFOFLAG_KNOWN_TIME CURLFINFOFLAG_KNOWN_PERM CURLFINFOFLAG_KNOWN_UID
  CURLFINFOFLAG_KNOWN_GID CURLFINFOFLAG_KNOWN_SIZE CURLFINFOFLAG_KNOWN_HLINKCOUNT])

dnl return codes for CURLOPT_CHUNK_BGN_FUNCTION
VICARE_CONSTANT_TESTS([CURL_CHUNK_BGN_FUNC_OK CURL_CHUNK_BGN_FUNC_FAIL
  CURL_CHUNK_BGN_FUNC_SKIP])

dnl return codes for CURLOPT_CHUNK_END_FUNCTION
VICARE_CONSTANT_TESTS([CURL_CHUNK_END_FUNC_OK CURL_CHUNK_END_FUNC_FAIL])

dnl return codes for FNMATCHFUNCTION
VICARE_CONSTANT_TESTS([CURL_FNMATCHFUNC_MATCH CURL_FNMATCHFUNC_NOMATCH CURL_FNMATCHFUNC_FAIL])

dnl These are the return codes for the seek callbacks
VICARE_CONSTANT_TESTS([CURL_SEEKFUNC_OK CURL_SEEKFUNC_FAIL CURL_SEEKFUNC_CANTSEEK])

dnl
VICARE_CONSTANT_TESTS([CURL_READFUNC_ABORT CURL_READFUNC_PAUSE])

dnl typedef enum curlsocktype
VICARE_CONSTANT_TESTS([CURLSOCKTYPE_IPCXN CURLSOCKTYPE_ACCEPT CURLSOCKTYPE_LAST])

dnl The return code from the sockopt_callback can signal
dnl information back to libcurl:
VICARE_CONSTANT_TESTS([CURL_SOCKOPT_OK CURL_SOCKOPT_ERROR CURL_SOCKOPT_ALREADY_CONNECTED])

dnl typedef enum curlioerr
VICARE_CONSTANT_TESTS([CURLIOE_OK CURLIOE_UNKNOWNCMD CURLIOE_FAILRESTART CURLIOE_LAST])

dnl typedef enum curliocmd
VICARE_CONSTANT_TESTS([CURLIOCMD_NOP CURLIOCMD_RESTARTREAD CURLIOCMD_LAST])

dnl typedef enum curl_infotype
dnl
dnl The constant CURLINFO_TLS_SESSION is defined below with other
dnl CURLINFO_ stuff.
dnl
VICARE_CONSTANT_TESTS([CURLINFO_TEXT CURLINFO_HEADER_IN CURLINFO_HEADER_OUT
  CURLINFO_DATA_IN CURLINFO_DATA_OUT CURLINFO_SSL_DATA_IN
  CURLINFO_SSL_DATA_OUT CURLINFO_END])

dnl bitmask defines for CURLOPT_HEADEROPT
VICARE_CONSTANT_TESTS([CURLHEADER_SEPARATE CURLHEADER_UNIFIED])

dnl typedef enum CURLcode
VICARE_CONSTANT_TESTS([CURLE_OK
  CURLE_UNSUPPORTED_PROTOCOL CURLE_FAILED_INIT CURLE_URL_MALFORMAT
  CURLE_NOT_BUILT_IN CURLE_COULDNT_RESOLVE_PROXY CURLE_COULDNT_RESOLVE_HOST
  CURLE_COULDNT_CONNECT CURLE_FTP_WEIRD_SERVER_REPLY CURLE_REMOTE_ACCESS_DENIED
  CURLE_FTP_ACCEPT_FAILED CURLE_FTP_WEIRD_PASS_REPLY CURLE_FTP_ACCEPT_TIMEOUT
  CURLE_FTP_WEIRD_PASV_REPLY CURLE_FTP_WEIRD_227_FORMAT CURLE_FTP_CANT_GET_HOST
  CURLE_OBSOLETE16 CURLE_FTP_COULDNT_SET_TYPE CURLE_PARTIAL_FILE
  CURLE_FTP_COULDNT_RETR_FILE CURLE_OBSOLETE20 CURLE_QUOTE_ERROR
  CURLE_HTTP_RETURNED_ERROR CURLE_WRITE_ERROR CURLE_OBSOLETE24
  CURLE_UPLOAD_FAILED CURLE_READ_ERROR CURLE_OUT_OF_MEMORY
  CURLE_OPERATION_TIMEDOUT CURLE_OBSOLETE29 CURLE_FTP_PORT_FAILED
  CURLE_FTP_COULDNT_USE_REST CURLE_OBSOLETE32 CURLE_RANGE_ERROR
  CURLE_HTTP_POST_ERROR CURLE_SSL_CONNECT_ERROR CURLE_BAD_DOWNLOAD_RESUME
  CURLE_FILE_COULDNT_READ_FILE CURLE_LDAP_CANNOT_BIND CURLE_LDAP_SEARCH_FAILED
  CURLE_OBSOLETE40 CURLE_FUNCTION_NOT_FOUND CURLE_ABORTED_BY_CALLBACK
  CURLE_BAD_FUNCTION_ARGUMENT CURLE_OBSOLETE44 CURLE_INTERFACE_FAILED
  CURLE_OBSOLETE46 CURLE_TOO_MANY_REDIRECTS CURLE_UNKNOWN_OPTION
  CURLE_TELNET_OPTION_SYNTAX CURLE_OBSOLETE50 CURLE_PEER_FAILED_VERIFICATION
  CURLE_GOT_NOTHING CURLE_SSL_ENGINE_NOTFOUND CURLE_SSL_ENGINE_SETFAILED
  CURLE_SEND_ERROR CURLE_RECV_ERROR CURLE_OBSOLETE57
  CURLE_SSL_CERTPROBLEM CURLE_SSL_CIPHER CURLE_SSL_CACERT
  CURLE_BAD_CONTENT_ENCODING CURLE_LDAP_INVALID_URL CURLE_FILESIZE_EXCEEDED
  CURLE_USE_SSL_FAILED CURLE_SEND_FAIL_REWIND CURLE_SSL_ENGINE_INITFAILED
  CURLE_LOGIN_DENIED CURLE_TFTP_NOTFOUND CURLE_TFTP_PERM
  CURLE_REMOTE_DISK_FULL CURLE_TFTP_ILLEGAL CURLE_TFTP_UNKNOWNID
  CURLE_REMOTE_FILE_EXISTS CURLE_TFTP_NOSUCHUSER CURLE_CONV_FAILED
  CURLE_CONV_REQD CURLE_SSL_CACERT_BADFILE CURLE_REMOTE_FILE_NOT_FOUND
  CURLE_SSH CURLE_SSL_SHUTDOWN_FAILED CURLE_AGAIN
  CURLE_SSL_CRL_BADFILE CURLE_SSL_ISSUER_ERROR CURLE_FTP_PRET_FAILED
  CURLE_RTSP_CSEQ_ERROR CURLE_RTSP_SESSION_ERROR CURLE_FTP_BAD_FILE_LIST
  CURLE_CHUNK_FAILED CURLE_HTTP2 CURLE_NO_CONNECTION_AVAILABLE CURLE_SSL_INVALIDCERTSTATUS
  CURLE_SSL_PINNEDPUBKEYNOTMATCH
  CURLE_WEIRD_SERVER_REPLY
  CURL_LAST])

dnl typedef enum curl_proxytype
VICARE_CONSTANT_TESTS([CURLPROXY_HTTP CURLPROXY_HTTP_1_0 CURLPROXY_SOCKS4
  CURLPROXY_SOCKS5 CURLPROXY_SOCKS4A CURLPROXY_SOCKS5_HOSTNAME])

dnl Bitmasks for CURLOPT_HTTPAUTH and CURLOPT_PROXYAUTH options.
VICARE_CONSTANT_TESTS([CURLAUTH_NONE CURLAUTH_BASIC CURLAUTH_DIGEST
  CURLAUTH_GSSNEGOTIATE CURLAUTH_NEGOTIATE CURLAUTH_NTLM CURLAUTH_DIGEST_IE
  CURLAUTH_NTLM_WB CURLAUTH_ONLY CURLAUTH_ANY CURLAUTH_ANYSAFE])

dnl
VICARE_CONSTANT_TESTS([CURLSSH_AUTH_ANY CURLSSH_AUTH_NONE CURLSSH_AUTH_PUBLICKEY
  CURLSSH_AUTH_PASSWORD CURLSSH_AUTH_HOST CURLSSH_AUTH_KEYBOARD CURLSSH_AUTH_AGENT
  CURLSSH_AUTH_DEFAULT])

dnl
VICARE_CONSTANT_TESTS([CURLGSSAPI_DELEGATION_NONE CURLGSSAPI_DELEGATION_POLICY_FLAG
 CURLGSSAPI_DELEGATION_FLAG])

dnl
VICARE_CONSTANT_TESTS([CURL_ERROR_SIZE])

dnl enum curl_khtype
VICARE_CONSTANT_TESTS([CURLKHTYPE_UNKNOWN CURLKHTYPE_RSA1 CURLKHTYPE_RSA CURLKHTYPE_DSS])

dnl enum curl_khstat
VICARE_CONSTANT_TESTS([CURLKHSTAT_FINE_ADD_TO_FILE CURLKHSTAT_FINE
  CURLKHSTAT_REJECT CURLKHSTAT_DEFER CURLKHSTAT_LAST])

dnl enum curl_khmatch
VICARE_CONSTANT_TESTS([CURLKHMATCH_OK CURLKHMATCH_MISMATCH CURLKHMATCH_MISSING
  CURLKHMATCH_LAST])

dnl typedef enum curl_usessl
VICARE_CONSTANT_TESTS([CURLUSESSL_NONE CURLUSESSL_TRY CURLUSESSL_CONTROL
  CURLUSESSL_ALL CURLUSESSL_LAST])

dnl
VICARE_CONSTANT_TESTS([CURLSSLOPT_ALLOW_BEAST])

dnl typedef enum curl_ftpccc
VICARE_CONSTANT_TESTS([CURLFTPSSL_CCC_NONE CURLFTPSSL_CCC_PASSIVE
  CURLFTPSSL_CCC_ACTIVE CURLFTPSSL_CCC_LAST])

dnl typedef enum curl_ftpauth
VICARE_CONSTANT_TESTS([CURLFTPAUTH_DEFAULT CURLFTPAUTH_SSL CURLFTPAUTH_TLS CURLFTPAUTH_LAST])

dnl typedef enum curl_ftpcreatedir
VICARE_CONSTANT_TESTS([CURLFTP_CREATE_DIR_NONE CURLFTP_CREATE_DIR CURLFTP_CREATE_DIR_RETRY
  CURLFTP_CREATE_DIR_LAST])

dnl typedef enum curl_ftpmethod
VICARE_CONSTANT_TESTS([CURLFTPMETHOD_DEFAULT CURLFTPMETHOD_MULTICWD
  CURLFTPMETHOD_NOCWD CURLFTPMETHOD_SINGLECWD CURLFTPMETHOD_LAST])

dnl CURLPROTO_ defines are for the CURLOPT_*PROTOCOLS options
VICARE_CONSTANT_TESTS([CURLPROTO_HTTP CURLPROTO_HTTPS CURLPROTO_FTP
  CURLPROTO_FTPS CURLPROTO_SCP CURLPROTO_SFTP CURLPROTO_TELNET
  CURLPROTO_LDAP CURLPROTO_LDAPS CURLPROTO_DICT CURLPROTO_FILE
  CURLPROTO_TFTP CURLPROTO_IMAP CURLPROTO_IMAPS CURLPROTO_POP3
  CURLPROTO_POP3S CURLPROTO_SMTP CURLPROTO_SMTPS CURLPROTO_RTSP
  CURLPROTO_RTMP CURLPROTO_RTMPT CURLPROTO_RTMPE CURLPROTO_RTMPTE
  CURLPROTO_RTMPS CURLPROTO_RTMPTS CURLPROTO_GOPHER
  CURLPROTO_SMB CURLPROTO_SMBS
  CURLPROTO_ALL])

dnl typedef enum curl_sslbackend, enum for the different supported SSL backends
VICARE_CONSTANT_TESTS([CURLSSLBACKEND_AXTLS
 CURLSSLBACKEND_CYASSL
 CURLSSLBACKEND_DARWINSSL
 CURLSSLBACKEND_GNUTLS
 CURLSSLBACKEND_GSKIT
 CURLSSLBACKEND_NONE
 CURLSSLBACKEND_NSS
 CURLSSLBACKEND_OPENSSL
 CURLSSLBACKEND_POLARSSL
 CURLSSLBACKEND_QSOSSL
 CURLSSLBACKEND_SCHANNEL])

dnl
VICARE_CONSTANT_TESTS([CURLOPTTYPE_LONG CURLOPTTYPE_OBJECTPOINT
  CURLOPTTYPE_FUNCTIONPOINT CURLOPTTYPE_OFF_T])

dnl typedef enum CURLoption
VICARE_CONSTANT_TESTS([CURLOPT_FILE
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
CURLOPT_DNS_INTERFACE
CURLOPT_DNS_LOCAL_IP4
CURLOPT_DNS_LOCAL_IP6
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
CURLOPT_XFERINFOFUNCTION
CURLOPT_XFERINFODATA
CURLOPT_EXPECT_100_TIMEOUT_MS
CURLOPT_HEADEROPT
CURLOPT_LOGIN_OPTIONS
CURLOPT_PATH_AS_IS
CURLOPT_PINNEDPUBLICKEY
CURLOPT_PROXYHEADER
CURLOPT_SSL_ENABLE_ALPN
CURLOPT_SSL_ENABLE_NPN
CURLOPT_SSL_FALSESTART
CURLOPT_SSL_VERIFYSTATUS
CURLOPT_UNIX_SOCKET_PATH
CURLOPT_XOAUTH2_BEARER
CURLOPT_PROXY_SERVICE_NAME
CURLOPT_SERVICE_NAME
CURLOPT_PIPEWAIT
CURLOPT_DEFAULT_PROTOCOL
CURLOPT_STREAM_WEIGHT
CURLOPT_STREAM_DEPENDS
CURLOPT_STREAM_DEPENDS_E
CURLOPT_TFTP_NO_OPTIONS
CURLOPT_CONNECT_TO
CURLOPT_TCP_FASTOPEN
CURLOPT_KEEP_SENDING_ON_ERROR
CURLOPT_PROXY_CAINFO
CURLOPT_PROXY_CAPATH
CURLOPT_PROXY_SSL_VERIFYPEER
CURLOPT_PROXY_SSL_VERIFYHOST
CURLOPT_PROXY_SSLVERSION
CURLOPT_PROXY_TLSAUTH_USERNAME
CURLOPT_PROXY_TLSAUTH_PASSWORD
CURLOPT_PROXY_TLSAUTH_TYPE
CURLOPT_PROXY_SSLCERT
CURLOPT_PROXY_SSLCERTTYPE
CURLOPT_PROXY_SSLKEY
CURLOPT_PROXY_SSLKEYTYPE
CURLOPT_PROXY_KEYPASSWD
CURLOPT_PROXY_SSL_CIPHER_LIST
CURLOPT_PROXY_CRLFILE
CURLOPT_PROXY_SSL_OPTIONS
CURLOPT_PRE_PROXY
CURLOPT_PROXY_PINNEDPUBLICKEY
CURLOPT_LASTENTRY])

dnl Defines for the CURLOPT_IPRESOLVE option.
VICARE_CONSTANT_TESTS([CURL_IPRESOLVE_WHATEVER CURL_IPRESOLVE_V4 CURL_IPRESOLVE_V6])

dnl
VICARE_CONSTANT_TESTS([CURLOPT_WRITEDATA CURLOPT_READDATA
  CURLOPT_HEADERDATA CURLOPT_RTSPHEADER])

dnl These enums are for use with the CURLOPT_HTTP_VERSION option.
VICARE_CONSTANT_TESTS([CURL_HTTP_VERSION_NONE CURL_HTTP_VERSION_1_0
  CURL_HTTP_VERSION_1_1 CURL_HTTP_VERSION_2_0
  CURL_HTTP_VERSION_2TLS CURL_HTTP_VERSION_2_PRIOR_KNOWLEDGE
  CURL_HTTP_VERSION_LAST])

dnl Public API enums for RTSP requests
VICARE_CONSTANT_TESTS([CURL_RTSPREQ_NONE CURL_RTSPREQ_OPTIONS CURL_RTSPREQ_DESCRIBE
  CURL_RTSPREQ_ANNOUNCE CURL_RTSPREQ_SETUP CURL_RTSPREQ_PLAY
  CURL_RTSPREQ_PAUSE CURL_RTSPREQ_TEARDOWN CURL_RTSPREQ_GET_PARAMETER
  CURL_RTSPREQ_SET_PARAMETER CURL_RTSPREQ_RECORD CURL_RTSPREQ_RECEIVE
  CURL_RTSPREQ_LAST])

dnl enum CURL_NETRC_OPTION
VICARE_CONSTANT_TESTS([CURL_NETRC_IGNORED CURL_NETRC_OPTIONAL CURL_NETRC_REQUIRED
  CURL_NETRC_LAST])

dnl
VICARE_CONSTANT_TESTS([CURL_SSLVERSION_DEFAULT
  CURL_SSLVERSION_TLSv1
  CURL_SSLVERSION_SSLv2
  CURL_SSLVERSION_SSLv3
  CURL_SSLVERSION_TLSv1_0
  CURL_SSLVERSION_TLSv1_1
  CURL_SSLVERSION_TLSv1_2
  CURL_SSLVERSION_LAST])

dnl enum CURL_TLSAUTH
VICARE_CONSTANT_TESTS([CURL_TLSAUTH_NONE CURL_TLSAUTH_SRP CURL_TLSAUTH_LAST])

dnl Symbols to use with CURLOPT_POSTREDIR.
VICARE_CONSTANT_TESTS([CURL_REDIR_GET_ALL CURL_REDIR_POST_301
  CURL_REDIR_POST_302 CURL_REDIR_POST_303 CURL_REDIR_POST_ALL])

dnl typedef enum curl_TimeCond
VICARE_CONSTANT_TESTS([CURL_TIMECOND_NONE CURL_TIMECOND_IFMODSINCE
  CURL_TIMECOND_IFUNMODSINCE CURL_TIMECOND_LASTMOD CURL_TIMECOND_LAST])

dnl typedef enum CURLformoption
VICARE_CONSTANT_TESTS([CURLFORM_NOTHING
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
CURLFORM_CONTENTLEN
CURLFORM_LASTENTRY])

dnl typedef enum CURLFORMcode
VICARE_CONSTANT_TESTS([CURL_FORMADD_OK
  CURL_FORMADD_MEMORY
  CURL_FORMADD_OPTION_TWICE
  CURL_FORMADD_NULL
  CURL_FORMADD_UNKNOWN_OPTION
  CURL_FORMADD_INCOMPLETE
  CURL_FORMADD_ILLEGAL_ARRAY
  CURL_FORMADD_DISABLED
  CURL_FORMADD_LAST])

dnl
VICARE_CONSTANT_TESTS([CURLINFO_STRING CURLINFO_LONG CURLINFO_DOUBLE
  CURLINFO_SLIST CURLINFO_MASK CURLINFO_TYPEMASK])

dnl typedef enum CURLINFO
VICARE_CONSTANT_TESTS([CURLINFO_NONE
  CURLINFO_EFFECTIVE_URL
  CURLINFO_RESPONSE_CODE
  CURLINFO_HTTP_CODE
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
  CURLINFO_TLS_SESSION
  CURLINFO_TLS_SSL_PTR
  CURLINFO_HTTP_VERSION
  CURLINFO_PROXY_SSL_VERIFYRESULT
  CURLINFO_PROTOCOL
  CURLINFO_SCHEME
  CURLINFO_LASTONE])

dnl For CURLINFO_PRIVATE we also need the HAVE_ define.
AS_VAR_SET([vicare_curl_have_curlinfo_private],0)
AS_IF([test "$vicare_cv_valueof_CURLINFO_PRIVATE" = "#f"],
  [AS_VAR_SET([vicare_curl_have_curlinfo_private],0)],
  [AS_VAR_SET([vicare_curl_have_curlinfo_private],1)])
AC_DEFINE_UNQUOTED([HAVE_CURLINFO_PRIVATE],
  $vicare_curl_have_curlinfo_private,
  [availability of CURLINFO_PRIVATE])

dnl typedef enum curl_closepolicy
VICARE_CONSTANT_TESTS([CURLCLOSEPOLICY_NONE
  CURLCLOSEPOLICY_OLDEST
  CURLCLOSEPOLICY_LEAST_RECENTLY_USED
  CURLCLOSEPOLICY_LEAST_TRAFFIC
  CURLCLOSEPOLICY_SLOWEST
  CURLCLOSEPOLICY_CALLBACK
  CURLCLOSEPOLICY_LAST])

dnl
VICARE_CONSTANT_TESTS([CURL_GLOBAL_SSL CURL_GLOBAL_WIN32 CURL_GLOBAL_ALL
  CURL_GLOBAL_NOTHING CURL_GLOBAL_DEFAULT CURL_GLOBAL_ACK_EINTR])

dnl typedef enum curl_lock_data
VICARE_CONSTANT_TESTS([CURL_LOCK_DATA_NONE
  CURL_LOCK_DATA_SHARE
  CURL_LOCK_DATA_COOKIE
  CURL_LOCK_DATA_DNS
  CURL_LOCK_DATA_SSL_SESSION
  CURL_LOCK_DATA_CONNECT
  CURL_LOCK_DATA_LAST])

dnl typedef enum curl_lock_access
VICARE_CONSTANT_TESTS([CURL_LOCK_ACCESS_NONE
  CURL_LOCK_ACCESS_SHARED
  CURL_LOCK_ACCESS_SINGLE
  CURL_LOCK_ACCESS_LAST])

dnl typedef enum CURLSHcode
VICARE_CONSTANT_TESTS([CURLSHE_OK
  CURLSHE_BAD_OPTION
  CURLSHE_IN_USE
  CURLSHE_INVALID
  CURLSHE_NOMEM
  CURLSHE_NOT_BUILT_IN
  CURLSHE_LAST])

dnl typedef enum CURLSHoption
VICARE_CONSTANT_TESTS([CURLSHOPT_NONE
  CURLSHOPT_SHARE
  CURLSHOPT_UNSHARE
  CURLSHOPT_LOCKFUNC
  CURLSHOPT_UNLOCKFUNC
  CURLSHOPT_USERDATA
  CURLSHOPT_LAST])

dnl typedef enum CURLversion
VICARE_CONSTANT_TESTS([CURLVERSION_FIRST
  CURLVERSION_SECOND
  CURLVERSION_THIRD
  CURLVERSION_FOURTH
  CURLVERSION_LAST
  CURLVERSION_NOW])

dnl
VICARE_CONSTANT_TESTS([CURL_VERSION_IPV6
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
  CURL_VERSION_NTLM_WB
  CURL_VERSION_GSSAPI
  CURL_VERSION_HTTP2
  CURL_VERSION_KERBEROS4
  CURL_VERSION_KERBEROS5
  CURL_VERSION_UNIX_SOCKETS
  CURL_VERSION_PSL
  CURL_VERSION_HTTPS_PROXY])

dnl
VICARE_CONSTANT_TESTS([CURLPAUSE_RECV
  CURLPAUSE_RECV_CONT
  CURLPAUSE_SEND
  CURLPAUSE_SEND_CONT
  CURLPAUSE_ALL
  CURLPAUSE_CONT])

dnl typedef enum CURLMcode
VICARE_CONSTANT_TESTS([CURLM_CALL_MULTI_PERFORM
  CURLM_OK
  CURLM_BAD_HANDLE
  CURLM_BAD_EASY_HANDLE
  CURLM_OUT_OF_MEMORY
  CURLM_INTERNAL_ERROR
  CURLM_BAD_SOCKET
  CURLM_UNKNOWN_OPTION
  CURLM_ADDED_ALREADY
  CURLM_LAST
  CURLM_CALL_MULTI_SOCKET])

dnl typedef enum CURLMSG
VICARE_CONSTANT_TESTS([CURLMSG_NONE
  CURLMSG_DONE
  CURLMSG_LAST])

dnl
VICARE_CONSTANT_TESTS([CURL_POLL_NONE
 CURL_POLL_IN
 CURL_POLL_OUT
 CURL_POLL_INOUT
 CURL_POLL_REMOVE])

dnl poll stuff
VICARE_CONSTANT_TESTS([CURL_WAIT_POLLIN
 CURL_WAIT_POLLPRI
 CURL_WAIT_POLLOUT])

dnl
VICARE_CONSTANT_TESTS([CURL_SOCKET_TIMEOUT])

dnl
VICARE_CONSTANT_TESTS([CURL_CSELECT_IN
 CURL_CSELECT_OUT
 CURL_CSELECT_ERR])

dnl typedef enum CURLMoption
VICARE_CONSTANT_TESTS([CURLMOPT_SOCKETFUNCTION
  CURLMOPT_SOCKETDATA
  CURLMOPT_PIPELINING
  CURLMOPT_TIMERFUNCTION
  CURLMOPT_TIMERDATA
  CURLMOPT_MAXCONNECTS
  CURLMOPT_LASTENTRY])

#page
#### functions

dnl AC_CHECK_FUNCS([])

AC_CHECK_FUNCS([curl_version curl_version_info])
AC_CHECK_FUNCS([curl_global_init curl_global_init_mem curl_global_cleanup])
AC_CHECK_FUNCS([curl_free])
AC_CHECK_FUNCS([curl_slist_append curl_slist_free_all])
AC_CHECK_FUNCS([curl_formadd curl_formget curl_formfree])
AC_CHECK_FUNCS([curl_easy_escape curl_escape curl_easy_unescape curl_unescape])
AC_CHECK_FUNCS([curl_getdate])
AC_CHECK_FUNCS([curl_share_init curl_share_setopt curl_share_cleanup curl_share_strerror])
AC_CHECK_FUNCS([curl_easy_init curl_easy_setopt curl_easy_perform curl_easy_cleanup curl_easy_getinfo curl_easy_duphandle curl_easy_reset curl_easy_recv curl_easy_send curl_easy_strerror curl_easy_pause])
AC_CHECK_FUNCS([curl_multi_init curl_multi_add_handle curl_multi_remove_handle curl_multi_fdset curl_multi_perform curl_multi_cleanup curl_multi_info_read curl_multi_strerror curl_multi_socket curl_multi_socket_action curl_multi_socket_all curl_multi_timeout curl_multi_setopt curl_multi_assign curl_multi_wait])

#page
#### data structures

AC_DEFUN([VICARE_CURL_HAVE_STRUCT],
  [AS_VAR_SET([vicare_curl_have_$1],0)
   AC_CHECK_TYPE([struct $1],
     [AS_VAR_SET([vicare_curl_have_$1],1)],
     [AS_VAR_SET([vicare_curl_have_$1],0)],
     VICARE_INCLUDES)
   AC_DEFINE_UNQUOTED([HAVE_$2],
     $vicare_curl_have_$1,
     [availability of $1])])

VICARE_CURL_HAVE_STRUCT([curl_tlssessioninfo],[CURL_TLSSESSIONINFO])
VICARE_CURL_HAVE_STRUCT([curl_sockaddr],[CURL_SOCKADDR])
VICARE_CURL_HAVE_STRUCT([curl_fileinfo],[CURL_FILEINFO])
VICARE_CURL_HAVE_STRUCT([curl_khkey],[CURL_KHKEY])
VICARE_CURL_HAVE_STRUCT([curl_forms],[CURL_FORMS])
VICARE_CURL_HAVE_STRUCT([curl_certinfo],[CURL_CERTINFO])
VICARE_CURL_HAVE_STRUCT([CURLMsg],[CURL_MSG])

#page
#### finish

VICARE_OUTPUT
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile]
        [lib/vicare/net/curl/constants.vicare.sls])
AC_OUTPUT

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^#page"
# End:
