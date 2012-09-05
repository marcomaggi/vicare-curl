## Process this file with automake to produce Makefile.in

vicare_curl_CURRENT	= @vicare_curl_VERSION_INTERFACE_CURRENT@
vicare_curl_REVISION	= @vicare_curl_VERSION_INTERFACE_REVISION@
vicare_curl_AGE		= @vicare_curl_VERSION_INTERFACE_AGE@
vicare_curl_VERSION	= $(vicare_curl_CURRENT):$(vicare_curl_REVISION):$(vicare_curl_AGE)

lib_LTLIBRARIES			= libvicare-curl.la
libvicare_curl_la_LDFLAGS	= -version-info $(vicare_curl_VERSION)
libvicare_curl_la_SOURCES	= \
	vicare-curl.c

### end of file
