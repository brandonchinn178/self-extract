#!/bin/bash
#
# Install system dependencies.

set -eo pipefail

function is_command() {
    type "$1" &> /dev/null
}

function install_linux() {
    if is_command yum; then
        yum update -y --exclude=filesystem
        yum install -y bzip2-devel zlib-devel
    fi
}

case "$(uname)" in
    (Linux) install_linux ;;
esac
