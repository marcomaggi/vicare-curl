/*
  Part of: Vicare/cURL
  Contents: Libcurl for Vicare, version functions
  Date: Wed Sep  5, 2012

  Abstract



  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either  version 3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <vicare.h>
#include <vicare-curl-internals.h>


/** --------------------------------------------------------------------
 ** Vicare/cURL version functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_vicare_curl_version_interface_current (void)
{
  return IK_FIX(vicare_curl_VERSION_INTERFACE_CURRENT);
}
ikptr
ikrt_vicare_curl_version_interface_revision (void)
{
  return IK_FIX(vicare_curl_VERSION_INTERFACE_REVISION);
}
ikptr
ikrt_vicare_curl_version_interface_age (void)
{
  return IK_FIX(vicare_curl_VERSION_INTERFACE_AGE);
}
ikptr
ikrt_vicare_curl_version (ikpcb * pcb)
{
  return ika_bytevector_from_cstring(pcb, vicare_curl_VERSION_INTERFACE_STRING);
}


/** --------------------------------------------------------------------
 ** Vicare/cURL version functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_curl_version (ikpcb * pcb)
{
#ifdef HAVE_CURL_VERSION
  return ika_bytevector_from_cstring(pcb, curl_version());
#else
  feature_failure(__func__);
#endif
}

#define IK_CURL_VERSION_INFO_DATA_AGE(OBJ)		IK_FIELD((OBJ),0)
#define IK_CURL_VERSION_INFO_DATA_VERSION(OBJ)		IK_FIELD((OBJ),1)
#define IK_CURL_VERSION_INFO_DATA_VERSION_NUM(OBJ)	IK_FIELD((OBJ),2)
#define IK_CURL_VERSION_INFO_DATA_HOST(OBJ)		IK_FIELD((OBJ),3)
#define IK_CURL_VERSION_INFO_DATA_FEATURES(OBJ)		IK_FIELD((OBJ),4)
#define IK_CURL_VERSION_INFO_DATA_SSL_VERSION(OBJ)	IK_FIELD((OBJ),5)
#define IK_CURL_VERSION_INFO_DATA_SSL_VERSION_NUM(OBJ)	IK_FIELD((OBJ),6)
#define IK_CURL_VERSION_INFO_DATA_LIBZ_VERSION(OBJ)	IK_FIELD((OBJ),7)
#define IK_CURL_VERSION_INFO_DATA_PROTOCOLS(OBJ)	IK_FIELD((OBJ),8)
#define IK_CURL_VERSION_INFO_DATA_ARES(OBJ)		IK_FIELD((OBJ),9)
#define IK_CURL_VERSION_INFO_DATA_ARES_NUM(OBJ)		IK_FIELD((OBJ),10)
#define IK_CURL_VERSION_INFO_DATA_LIBIDN(OBJ)		IK_FIELD((OBJ),11)
#define IK_CURL_VERSION_INFO_DATA_ICONV_VER_NUM(OBJ)	IK_FIELD((OBJ),12)
#define IK_CURL_VERSION_INFO_DATA_LIBSSH_VERSION(OBJ)	IK_FIELD((OBJ),13)

ikptr
ikrt_curl_version_info (ikptr s_rtd, ikptr s_version_code, ikpcb * pcb)
{
#ifdef HAVE_CURL_VERSION_INFO
  CURLversion			version_code = ik_integer_to_int(s_version_code);
  curl_version_info_data *	info;
  ikptr				rv;
  info = curl_version_info(version_code);
  rv = ika_struct_alloc_and_init(pcb, s_rtd);
  pcb->root0 = &rv;
  {
    IK_ASS(IK_CURL_VERSION_INFO_DATA_AGE(rv),
	   ika_integer_from_int(pcb, info->age));
    IK_ASS(IK_CURL_VERSION_INFO_DATA_VERSION(rv),
	   (info->version)? ika_bytevector_from_cstring(pcb, info->version) : IK_FALSE);
    IK_ASS(IK_CURL_VERSION_INFO_DATA_VERSION_NUM(rv),
	   ika_integer_from_int(pcb, info->version_num));
    IK_ASS(IK_CURL_VERSION_INFO_DATA_HOST(rv),
	   (info->host)? ika_bytevector_from_cstring(pcb, info->host) : IK_FALSE);
    IK_ASS(IK_CURL_VERSION_INFO_DATA_FEATURES(rv),
	   ika_integer_from_int(pcb, info->features));
    IK_ASS(IK_CURL_VERSION_INFO_DATA_SSL_VERSION(rv),
	   (info->ssl_version)? \
	   ika_bytevector_from_cstring(pcb, info->ssl_version) : IK_FALSE);
    IK_ASS(IK_CURL_VERSION_INFO_DATA_SSL_VERSION_NUM(rv),
	   ika_integer_from_long(pcb, info->ssl_version_num));
    IK_ASS(IK_CURL_VERSION_INFO_DATA_LIBZ_VERSION(rv),
	   (info->libz_version)?
	   ika_bytevector_from_cstring(pcb, info->libz_version) : IK_FALSE);
    {
      ikptr	s_protocols = IK_NULL;
      ikptr	s_pair = IK_NULL;
      pcb->root1 = &s_protocols;
      pcb->root2 = &s_pair;
      {
      	int	i;
      	for (i=0; i==100 || info->protocols[i]; ++i) {
      	  s_pair = ika_pair_alloc(pcb);
      	  IK_ASS(IK_CAR(s_pair), ika_bytevector_from_cstring(pcb, info->protocols[i]));
      	  IK_CDR(s_pair) = s_protocols;
      	  s_protocols = s_pair;
      	}
      }
      pcb->root1 = NULL;
      IK_ASS(IK_CURL_VERSION_INFO_DATA_PROTOCOLS(rv), s_protocols);
    }
    IK_ASS(IK_CURL_VERSION_INFO_DATA_ARES(rv),
	   (info->ares)? ika_bytevector_from_cstring(pcb, info->ares) : IK_FALSE);
    IK_ASS(IK_CURL_VERSION_INFO_DATA_ARES_NUM(rv),
	   ika_integer_from_int(pcb, info->ares_num));
    IK_ASS(IK_CURL_VERSION_INFO_DATA_LIBIDN(rv),
	   (info->libidn)? ika_bytevector_from_cstring(pcb, info->libidn) : IK_FALSE);
    IK_ASS(IK_CURL_VERSION_INFO_DATA_ICONV_VER_NUM(rv),
	   ika_integer_from_int(pcb, info->iconv_ver_num));
    IK_ASS(IK_CURL_VERSION_INFO_DATA_LIBSSH_VERSION(rv),
	   (info->libssh_version)? \
	   ika_bytevector_from_cstring(pcb, info->libssh_version) : IK_FALSE);
  }
  pcb->root0 = NULL;
  return rv;
#else
  feature_failure(__func__);
#endif
}

/* end of file */
