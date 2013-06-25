# configure.sh --
#
# Run this to configure.

set -xe

prefix=/usr/local

../configure \
    --enable-maintainer-mode                    \
    --config-cache                              \
    --cache-file=../config.cache                \
    --prefix="${prefix}"                        \
    CFLAGS='-O3'				\
    "$@"

#    --enable-debug

### end of file
