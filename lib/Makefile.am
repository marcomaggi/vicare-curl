## Process this file with automake to produce Makefile.in

EXTRA_DIST	= \
	vicare/net/curl/constants.sls.in

# These files are hand-written.
nobase_dist_libvicare_DATA = \
	vicare/net/curl.sls			\
	vicare/net/curl/unsafe-capi.sls

# These files are created at configuration or make time.
nobase_nodist_libvicare2_DATA = \
	vicare/net/curl/constants.sls		\
	vicare/net/curl/features.sls

# if WANT_NAUSICAA
# nobase_dist_libvicare_DATA	+= nausicaa/xml/curl.vicare.sls
# endif

VICARE_LIB_INFIX	= net/curl

include $(top_srcdir)/meta/makefiles/lib.am

### end of file
