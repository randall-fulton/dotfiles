# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/Users/randallfulton/.oh-my-zsh"

# load env from profile
source ~/.profile
export PATH=/usr/local/bin:$PATH

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="typewritten"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Use Vim as man-page viewer
# Solution taken from https://vim.fandom.com/wiki/Using_vim_as_a_man-page_viewer_under_Unix
# export PAGER="/bin/sh -c \"unset PAGER;col -b -x | \
#     vim -R -c 'set ft=man nomod nolist' -c 'map q :q<CR>' \
#     -c 'map <SPACE> <C-D>' -c 'map b <C-U>' \
#     -c 'nmap K :Man <C-R>=expand(\\\"<cword>\\\")<CR><CR>' -\""
export FZF_DEFAULT_COMMAND='find . -path ./cache -prune -o -path ./node_modules -prune -o -path ./ios/Pods -prune -o -print' # OSX version
# export FZF_DEFAULT_COMMAND='find -type f -not -path "./node_modules/*"' # GNU version

alias refresh="source ~/.zshrc"

auth() {
	$(aws-okta env $AWS_ACCOUNT_NAME)
}

vpn() {
	case "$1" in
	"-d")	# spin up connection in background
		sudo openvpn --config $HOME/.vpn/client.ovpn --daemon
		;;
	"-k")	# kill background vpn connection
		sudo kill -9 $(pgrep openvpn)
		;;
	*) 	# run in foreground mode
		sudo openvpn --config $HOME/.vpn/client.ovpn
		;;
	esac
}

# colors() {
# 	for i in {0..255}; do
# 	    printf "\x1b[38;5;${i}mcolour${i}\x1b[0m\n"
# 	done
# }

alias tmux="TERM=xterm-256color tmux"
alias vim="nvim"

export NPM_PACKAGES="/Users/randallfulton/.npm-packages"
export NODE_PATH="$NPM_PACKAGES/lib/node_modules${NODE_PATH:+:$NODE_PATH}"
export PATH="$NPM_PACKAGES/bin:$PATH"
unset MANPATH
export MANPATH="$NPM_PACKAGES/share/man:$(manpath)"

export JAVA_HOME=$(/usr/libexec/java_home)
export ANDROID_HOME="/Users/randallfulton/Library/Android/sdk"
export PATH="$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools"

export GOPATH="$HOME/go"
export GOPRIVATE="github.com/shipt"

# ncurses-go setup
# export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/local/Cellar/ncurses/6.2/lib/pkgconfig/"

git-clean() {
  git branch --merged | egrep -v "(^\*|master|dev)" | xargs git branch -d
}

mov2gif() {
  ffmpeg -i $1 -r 10 -pix_fmt yuv420p -f gif $2
}

alias debug_android="adb shell input keyevent 82"

kenv() {
  config=$(
    platformctl config get shipt-aviator \
      --target k8s \
      --group app \
      --environment staging \
      --role webserver \
      --region us-east-1
  )
  export $(echo $config | grep KAFKA_BROKERS)
  export $(echo $config | grep KAFKA_SECRET)
  export $(echo $config | grep KAFKA_KEY)
  export $(echo $config | grep KAFKA_TOPIC_CRUD_ORDER)
}

kcat() {
  kafkacat -b $KAFKA_BROKERS -X security.protocol=SASL_SSL -X sasl.mechanisms=PLAIN -X sasl.username=$KAFKA_KEY -X sasl.password=$KAFKA_SECRET -X api.version.request=true "$@"
}

aws_flat_env() {
  creds=$(aws-okta env $1)
  echo $creds | sed 's/export //g' | tr '\n' ';'
}

eval "$(pyenv init -)"
eval "$(rbenv init -)"
alias vim='nvim'

export PATH="$HOME/.poetry/bin:$PATH"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
