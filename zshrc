alias refresh="source $HOME/.zshrc"
if [ "$(uname)" = "Darwin" ]; then
	alias restart-skhd="launchctl kickstart -k \"gui/\${UID}/org.nixos.skhd\""
	alias restart-spacebar="launchctl kickstart -k \"gui/\${UID}/org.nixos.spacebar\""
	alias restart-yabai="launchctl kickstart -k \"gui/\${UID}/org.nixos.yabai\""
fi

command -v pyenv >/dev/null    && eval "$(pyenv init --path)"
command -v rbenv >/dev/null    && eval "$(rbenv init - zsh)"
command -v starship >/dev/null && eval "$(starship init zsh)"
command -v zoxide >/dev/null   && eval "$(zoxide init zsh)"
[[ -f "$HOME/.cargo/env" ]]    && . "$HOME/.cargo/env"
