#!/usr/bin/env zsh
autoload -Uz compinit && compinit

bindkey '^[p' up-line-or-history
bindkey '^[n' down-line-or-history

if [ -d ~/.zsh_plugins ]; then
	for plugin in ~/.zsh_plugins/*; do
		source $plugin
	done
fi

alias tmux="TERM=screen-256color-bce tmux"

alias refresh="source $HOME/.zshrc"
if [ "$(uname)" = "Darwin" ]; then
	alias restart-skhd="launchctl kickstart -k \"gui/\${UID}/com.koekeishiya.skhd\""
	alias restart-spacebar="launchctl kickstart -k \"gui/\${UID}/homebrew.mxcl.spacebar\""
	alias restart-yabai="launchctl kickstart -k \"gui/\${UID}/com.koekeishiya.yabai\""
fi

if command -v brew >/dev/null; then
	eval "$(/opt/homebrew/bin/brew shellenv)"
	export C_INCLUDE_PATH=$(brew --prefix)/include
	export LIBRARY_PATH=$(brew --prefix)/lib
fi
command -v kafkactl>/dev/null && eval "$(kafkactl completion zsh)"

command -v direnv >/dev/null   && eval "$(direnv hook zsh)"
command -v exa >/dev/null      && alias ls="exa"
command -v pyenv >/dev/null    && eval "$(pyenv init --path)"
command -v rbenv >/dev/null    && eval "$(rbenv init - zsh)"
command -v starship >/dev/null && eval "$(starship init zsh)"
command -v zoxide >/dev/null   && eval "$(zoxide init zsh)"
[[ -f "$HOME/.cargo/env" ]]    && . "$HOME/.cargo/env"
[[ -d "$HOME/.rd/bin" ]]			 && export PATH="/Users/randall/.rd/bin:$PATH"

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/randall/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
