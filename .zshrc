# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
# export ZSH="/Users/randallfulton/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME=""

plugins=(git) # zsh-fzf-history-search)

# source $HOME/.oh-my-zsh/oh-my-zsh.sh
# source $HOME/.profile

alias refresh="source ~/.zshrc"

switch() {
	rm -rf "$HOME/Applications/Home Manager Apps"
	darwin-rebuild switch
}

# auth() {
# 	$(aws-okta env $AWS_ACCOUNT_NAME)
# }

# alias tmux="TERM=xterm-256color tmux"
# alias vim="nvim"

# git-clean() {
#   git branch --merged | egrep -v "(^\*|master|dev)" | xargs git branch -d
# }

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

eval "$(starship init zsh)"
