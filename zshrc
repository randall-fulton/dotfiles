#!/usr/bin/env zsh

### Base ###

export ZSH=$HOME/.config/zsh

autoload -Uz compinit && compinit # completion

export HISTFILE=$ZSH/history
export HISTSIZE=10000       # max history loaded into memory
export SAVEHIST=10000       # max history saved to file
setopt HIST_IGNORE_ALL_DUPS # don't save duplicates
setopt HIST_FIND_NO_DUPS    # don't show duplicates

bindkey -e # needed for Emacs-style keybinds to work on macOS

### History ###

autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
# bindkey "^[[A" history-beginning-search-backward-end
# bindkey "^[[B" history-beginning-search-forward-end
bindkey "^[p" history-beginning-search-backward-end
bindkey "^[n" history-beginning-search-forward-end
# bindkey '^[p' up-line-or-history
# bindkey '^[n' down-line-or-history

### Plugins ###

source $ZSH/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source $ZSH/plugins/zsh-fzf-history-search/zsh-fzf-history-search.zsh
fpath=($ZSH/plugins/zsh-completions/src $fpath)

###############

alias refresh="source $HOME/.zshrc"
if command -v uname>/dev/null && [ "$(uname)" = "Darwin" ]; then
	alias restart-skhd="launchctl kickstart -k \"gui/\${UID}/com.koekeishiya.skhd\""
	alias restart-spacebar="launchctl kickstart -k \"gui/\${UID}/homebrew.mxcl.spacebar\""
	alias restart-yabai="launchctl kickstart -k \"gui/\${UID}/com.koekeishiya.yabai\""
fi

alias ga="git add"
alias gcm="git commit -m"
alias gd="git diff"
alias gp="git push"
alias gs="git status"

if command -v brew >/dev/null; then
	eval "$(/opt/homebrew/bin/brew shellenv)"
	export C_INCLUDE_PATH=$(brew --prefix)/include
	export LIBRARY_PATH=$(brew --prefix)/lib
fi
command -v kafkactl>/dev/null && eval "$(kafkactl completion zsh)"

command -v direnv >/dev/null   && eval "$(direnv hook zsh)"
# command -v exa >/dev/null      && alias ls="exa"
command -v pyenv >/dev/null    && eval "$(pyenv init --path)"
command -v rbenv >/dev/null    && eval "$(rbenv init - zsh)"
command -v starship >/dev/null && eval "$(starship init zsh)"
command -v zoxide >/dev/null   && eval "$(zoxide init zsh)"
[[ -f "$HOME/.cargo/env" ]]    && . "$HOME/.cargo/env"
[[ -d "$HOME/.rd/bin" ]]			 && export PATH="$PATH:/Users/randall/.rd/bin"

export POETRY="$HOME/.local/bin/poetry"
poetry() {
	$POETRY $@
}

colors() {
	for x in {0..8}; do 
		for i in {30..37}; do 
			for a in {40..47}; do 
				echo -ne "\e[$x;$i;$a""m\\\e[$x;$i;$a""m\e[0;37;40m "
			done
			echo
		done
	done
	echo ""
}

shorten_path() {
	awk -F '/' '{if(NF > 3){print "/…/"$(NF-1)"/"$(NF)}else{print}}' < /dev/stdin
}

switch() {
	project=$(ls ~/dev | fzf)
	cd "$HOME/dev/$project"
}

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/randall/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
