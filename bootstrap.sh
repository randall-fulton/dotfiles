#!/bin/sh

# TODO: prompt for shell restart after installing nix

set -e

SYSTEM=$(uname -s)

msg() {
    echo "\033[1;35m$1\033[0m"
}

install_nix() {
    if command -v nix >/dev/null; then
	msg "nix already installed"
	return
    fi
    
    msg "installing nix..."

    curl -s -L https://nixos.org/nix/install > /tmp/install-nix.sh
    case "${SYSTEM}" in
	"Darwin")
	    sh /tmp/install-nix.sh
	    ;;
	"Linux")
	    sh /tmp/install-nix.sh --daemon
	    ;;
    esac
}

install_nix_darwin() {
    if command -v darwin-rebuild >/dev/null; then
	msg "nix-darwin already installed"
	return
    fi
    
    msg "installing nix-darwin..."
    nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
    ./result/bin/darwin-installer
}

install_home_manager() {
    if command -v home-manager >/dev/null; then
	msg "home-manager already installed"
    fi
    
    msg "installing home-manager..."
    nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
    nix-channel --update
}

install_nix
install_home_manager
if [[ "${SYSTEM}" -eq "Darwin" ]]; then
    install_nix_darwin
fi

if [[ "${SYSTEM}" -eq "Darwin" ]]; then
    ln -s $(pwd)/darwin-configuration.nix ~/.nixpkgs/darwin-configuration.nix
else
    ln -s $(pwd)/home.nix ~/.config/home-manager/home.nix
fi
    
