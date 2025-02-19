#!/usr/bin/bash
set -e

function execho {
    local prog="$1"
    shift
    echo -e "$ \e[32m$prog\e[0m $*"
    "$prog" "$@"
	return $?
}

LUAROCKS_URL="http://luarocks.github.io/luarocks/releases/luarocks-$VERSION.tar.gz"

execho curl -fL "$LUAROCKS_URL" -o /tmp/luarocks.tar.gz
execho tar -C /tmp/ -xzf /tmp/luarocks.tar.gz
builddir="/tmp/luarocks-$VERSION"

cd "$builddir"
execho ./configure --with-lua-include=/usr/local/include/luajit-2.1
execho make
execho su vscode -c 'sudo make install'
execho su vscode -c 'luarocks config local_by_default true'
