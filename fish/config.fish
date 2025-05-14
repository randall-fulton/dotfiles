### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
set --export --prepend PATH "/Users/randall/.rd/bin"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)

set -gx GOPRIVATE "github.com/shipt"
set -gx EDITOR nvim
if echo $DOCKER_OPTS | grep -v artifactory
    set -gx DOCKER_OPTS "$DOCKER_OPTS --insecure-registry artifactory.gcp.shipttech.com"
end

alias docker=podman

function opr -a cred
    op read op://Private/$cred
end

abbr --add lg lazygit
abbr --add dla \
    'docker login \
        -u (opr Artifactory/username) \
        -p (opr Artifactory/credential) \
        artifactory.gcp.shipttech.com'
abbr --add dcd docker compose down
abbr --add dcl docker compose logs -f
abbr --add dcp docker compose ps
abbr --add dcr docker compose restart
abbr --add dcs docker compose stop
abbr --add dcu docker compose up -d
abbr --add gt  gotestsum
abbr --add gtw gotestsum --watch

fish_add_path /opt/homebrew/bin
fish_add_path /opt/homebrew/Cellar/coreutils/9.3/libexec/gnubin 
fish_add_path /usr/bin
fish_add_path /usr/sbin
fish_add_path /bin
fish_add_path /sbin 
fish_add_path /opt/homebrew/opt/postgresql@15/bin/ 
fish_add_path /usr/local/bin
fish_add_path /usr/local/go/bin
fish_add_path /Applications/Ghostty.app/Contents/MacOS
fish_add_path /Applications/WezTerm.app/Contents/MacOS
fish_add_path $HOME/.shipt
fish_add_path $HOME/.local/bin
fish_add_path $HOME/go/bin
fish_add_path $HOME/.cargo/bin
fish_add_path $HOME/.rd/bin 
fish_add_path $HOME/dev/janet/janet-v1.38.0-macos/bin

starship init fish | source

function gh-token
    op read "op://Private/Github Access Token/credential"
end

function reload
    source ~/.config/fish/config.fish
end

function direnv -v PWD
    if ! test -f .env.fish
        return
    end

    echo "loading .env.fish"
    source .env.fish
end

function switch-project
    set project (ls ~/dev | fzf)
    cd "$HOME/dev/$project"
    commandline -f repaint
end
bind ctrl-space switch-project

emit poststart
