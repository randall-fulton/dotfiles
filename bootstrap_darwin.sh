#!/bin/sh

set -e

msg() {
    echo "\033[1;35m$1\033[0m"
}

install_nix() {
    curl -s -L https://nixos.org/nix/install > /tmp/install-nix.sh
    case "$(uname -s)" in
	"Darwin")
	    sh /tmp/install-nix.sh
	    ;;
	"Linux")
	    sh /tmp/install-nix.sh --daemon
	    ;;
	*)
	    echo "OS '$(uname -s)' not supported by $0"
    esac
}

install_nix_darwin() {
    nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
    ./result/bin/darwin-installer    
}

install_home_manager() {
    nix-channel --add https://github.com/nix-community/home-manager/archive/release-22.11.tar.gz home-manager
    nix-channel --update
}

if ! command -v nix >/dev/null; then
    msg "installing nix..."
    install_nix
else
    msg "nix already installed"
fi

if ! command -v darwin-rebuild >/dev/null; then
    msg "installing nix-darwin..."
    install_nix_darwin
    ln -s $(pwd)/darwin-configuration.nix ~/.nixpkgs/darwin-configuration.nix
else
    msg "nix-darwin already installed"
fi

if ! command -v home-manager >/dev/null; then
    msg "installing home-manager..."
    install_home_manager
else
    msg "home-manager already installed"
fi
