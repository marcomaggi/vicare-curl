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
ikptr
ikrt_curl_version_info (ikrt s_version_code, ikpcb * pcb)
{
#ifdef HAVE_CURL_VERSION_INFO
  CURLversion			version_code = ik_integer_to_int(s_version_code);
  curl_version_info_data *	info;
  info = curl_version_info(version_code);
#else
  feature_failure(__func__);
#endif
}

/* end of file */
