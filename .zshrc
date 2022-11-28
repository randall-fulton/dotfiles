# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
# export ZSH="/Users/randallfulton/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME=""
fpath+=$HOME/.zsh/typewritten
autoload -U promptinit; promptinit
prompt typewritten

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
plugins=(git zsh-fzf-history-search)

source $HOME/.oh-my-zsh/oh-my-zsh.sh
source $HOME/.profile

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

alias refresh="source ~/.zshrc"

# auth() {
# 	$(aws-okta env $AWS_ACCOUNT_NAME)
# }

# alias tmux="TERM=xterm-256color tmux"
# alias vim="nvim"

# git-clean() {
#   git branch --merged | egrep -v "(^\*|master|dev)" | xargs git branch -d
# }

# mov2gif() {
#   ffmpeg -i $1 -r 10 -pix_fmt yuv420p -f gif $2
# }

# alias debug_android="adb shell input keyevent 82"

# kenv() {
#   $(platformctl console exec feature-flags \
#       -c "printenv | grep KAFKA" \
#       -e staging \
#       -r webserver \
#       -g app |
#     tail -n 5 |
#     head -n 4 |
#     sed 's/^/export /g'
#   )
# }

# kcat() {
#   kafkacat -b $KAFKA_BROKERS -X security.protocol=SASL_SSL -X sasl.mechanisms=PLAIN -X sasl.username=$KAFKA_KEY -X sasl.password=$KAFKA_SECRET -X api.version.request=true "$@"
# }

# aws_flat_env() {
#   creds=$(aws-okta env $1)
#   echo $creds | sed 's/export //g' | tr '\n' ';'
# }

# eval "$(pyenv init -)"
# eval "$(rbenv init -)"
# alias vim='nvim'

# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/rfulton/opt/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/rfulton/opt/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/rfulton/opt/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/rfulton/opt/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

alias notes="$HOME/dev/notes/notes"
alias today="$HOME/dev/notes/notes today"
alias mem="$HOME/dev/notes/notes memory"
alias proj="$HOME/dev/notes/notes project"

if [[ "$OSTYPE" == "darwin"* ]]; then
	# make sure GNU coreutils takes priority on OSX
	export PATH="$(brew --prefix coreutils)/libexec/gnubin:/usr/local/bin:$PATH"
fi

# Quick cd with zoxide
eval "$(zoxide init zsh)"
# Automatically load env vars from a .envrc in a directory using direnv
# eval "$(direnv hook zsh)"
