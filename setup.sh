#!/usr/bin/env bash

# configuration
mkdir -p $HOME/.config/alacritty \
		 $HOME/.config/git
ln -sf $(pwd)/alacritty.yml $HOME/.config/alacritty/alacritty.yml
ln -sf $(pwd)/gitconfig		$HOME/.config/git/config
ln -sf $(pwd)/gitignore		$HOME/.config/git/ignore
ln -sf $(pwd)/nvim			$HOME/.config/nvim
ln -sf $(pwd)/zshrc			$HOME/.zshrc
ln -sf $(pwd)/tmux.conf		$HOME/.tmux.conf

# utilities
sudo apt install \
	alacritty \
	git \
	tmux

# unified prompt
if ! [ -f /usr/local/bin/starship ]; then
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
