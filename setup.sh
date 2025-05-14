#!/usr/bin/env bash

# configuration
mkdir -p \
	$HOME/.config/aerospace \
	$HOME/.config/fish \
	$HOME/.config/git

ln -sf $(pwd)/aerospace.toml	$HOME/.config/aerospace
ln -sf $(pwd)/gitconfig			$HOME/.config/git/config
ln -sf $(pwd)/gitignore			$HOME/.config/git/ignore
ln -sf $(pwd)/fish				$HOME/.config/fish
ln -sf $(pwd)/wezterm			$HOME/.config/wezterm

if [ ! -d "$HOME/.config/nvim" ]; then
	git clone git@github.com:randall-fulton/.nvim.git \
		$HOME/.config/nvim
fi

if [[ "$(uname -s)" -eq "Darwin" ]]; then
	brew bundle install --file Brewfile
	if ! [ $(grep "fish" /etc/shells) ]; then
		echo "Adding fish as a standard shell"
		sudo bash -c "echo $(which fish) >> /etc/shells"
	fi
	if ! [ "$SHELL" -eq "$(which fish)" ]; then
		chsh -s $(which fish)
	fi
else
	sudo apt install git
fi

# unified prompt
if ! command -v starship ; then
	curl -sS https://starship.rs/install.sh | sh
fi

# preferred font
if [ "$(uname)" = "Linux" ] && ! ls ~/.local/share/fonts/FiraCodeNerdFont* 1>/dev/null 2>&1; then
	echo "need font"
	fontdir=$(mktemp -d)
	fontfile="$fontdir/FiraCode.zip"
	curl -L -o "$fontfile" https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/FiraCode.zip
	unzip -d "$fontdir" "$fontfile"
	mkdir -p ~/.local/share/fonts/
	cp $fontdir/*.ttf ~/.local/share/fonts
	rm -rf "$fontdir"
fi
