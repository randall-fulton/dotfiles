#!/bin/sh

set -e

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
    case "$(uname -s)" in
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
    ln -s $(pwd)/darwin-configuration.nix ~/.nixpkgs/darwin-configuration.nix
}

install_home_manager() {
    if command -v home-manager >/dev/null; then
	msg "home-manager already installed"
    fi
    
    msg "installing home-manager..."
    nix-channel --add https://github.com/nix-community/home-manager/archive/release-22.11.tar.gz home-manager
    nix-channel --update
}

install_nix
if [ "$(uname -s)" -eq "Darwin" ]; then
    install_nix_darwin
fi
install_home_manager
