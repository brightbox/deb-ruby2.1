#!/bin/sh

if [ -z "$ruby_arch" ]; then
  ruby_arch=$(dpkg-architecture -qDEB_HOST_MULTIARCH)
fi

ruby_program=$(basename $0)

exec "${ruby_arch}-${ruby_program}" "$@"
