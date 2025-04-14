### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
set --export --prepend PATH "/Users/randall/.rd/bin"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)

set -g -x GOPRIVATE "github.com/shipt"
set -g -x EDITOR nvim

abbr --add lg lazygit
abbr --add dcd docker compose down
abbr --add dcl docker compose logs -f
abbr --add dcp docker compose ps
abbr --add dcr docker compose restart
abbr --add dcs docker compose stop
abbr --add dcu docker compose up -d

fish_add_path /opt/homebrew/Cellar/coreutils/9.3/libexec/gnubin 
fish_add_path /usr/bin
fish_add_path /usr/sbin
fish_add_path /bin
fish_add_path /sbin 
fish_add_path /opt/homebrew/bin
fish_add_path /opt/homebrew/opt/postgresql@15/bin/ 
fish_add_path /usr/local/bin
fish_add_path /usr/local/go/bin
fish_add_path /Users/randall/.local/bin
fish_add_path /Users/randall/go/bin
fish_add_path /Users/randall/.cargo/bin
fish_add_path /Users/randall/.rd/bin 
fish_add_path /Applications/Ghostty.app/Contents/MacOS
fish_add_path /Applications/WezTerm.app/Contents/MacOS

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

emit poststart
