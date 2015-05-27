## dependencies.make --
#
# Automatically built.

EXTRA_DIST +=  \
	lib/vicare/net/curl/constants.vicare.sls.in

lib/vicare/net/curl.fasl: \
		lib/vicare/net/curl.vicare.sls \
		lib/vicare/net/curl/constants.fasl \
		lib/vicare/net/curl/unsafe-capi.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_net_curl_fasldir = $(bundledlibsdir)/vicare/net
lib_vicare_net_curl_vicare_slsdir  = $(bundledlibsdir)/vicare/net
nodist_lib_vicare_net_curl_fasl_DATA = lib/vicare/net/curl.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_net_curl_vicare_sls_DATA = lib/vicare/net/curl.vicare.sls
endif
EXTRA_DIST += lib/vicare/net/curl.vicare.sls
CLEANFILES += lib/vicare/net/curl.fasl

lib/vicare/net/curl/constants.fasl: \
		lib/vicare/net/curl/constants.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_net_curl_constants_fasldir = $(bundledlibsdir)/vicare/net/curl
lib_vicare_net_curl_constants_vicare_slsdir  = $(bundledlibsdir)/vicare/net/curl
nodist_lib_vicare_net_curl_constants_fasl_DATA = lib/vicare/net/curl/constants.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_net_curl_constants_vicare_sls_DATA = lib/vicare/net/curl/constants.vicare.sls
endif
CLEANFILES += lib/vicare/net/curl/constants.fasl

lib/vicare/net/curl/unsafe-capi.fasl: \
		lib/vicare/net/curl/unsafe-capi.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_net_curl_unsafe_capi_fasldir = $(bundledlibsdir)/vicare/net/curl
lib_vicare_net_curl_unsafe_capi_vicare_slsdir  = $(bundledlibsdir)/vicare/net/curl
nodist_lib_vicare_net_curl_unsafe_capi_fasl_DATA = lib/vicare/net/curl/unsafe-capi.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_net_curl_unsafe_capi_vicare_sls_DATA = lib/vicare/net/curl/unsafe-capi.vicare.sls
endif
EXTRA_DIST += lib/vicare/net/curl/unsafe-capi.vicare.sls
CLEANFILES += lib/vicare/net/curl/unsafe-capi.fasl

lib/vicare/net/curl/features.fasl: \
		lib/vicare/net/curl/features.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_net_curl_features_fasldir = $(bundledlibsdir)/vicare/net/curl
lib_vicare_net_curl_features_vicare_slsdir  = $(bundledlibsdir)/vicare/net/curl
nodist_lib_vicare_net_curl_features_fasl_DATA = lib/vicare/net/curl/features.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_net_curl_features_vicare_sls_DATA = lib/vicare/net/curl/features.vicare.sls
endif
CLEANFILES += lib/vicare/net/curl/features.fasl


### end of file
# Local Variables:
# mode: makefile-automake
# End:
