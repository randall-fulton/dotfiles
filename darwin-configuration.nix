# TODO: install Fira Code Nerd Font
# TODO: install starship
# TODO: auto-hide menu bar
# TODO: setup homebrew
# TODO: setup alfred and disable spotlight
# TODO: install emacs
# TODO: explore yabai spaces for tab-like setup https://github.com/koekeishiya/yabai/issues/203
# TODO: install kitty
# TODO: create aliases in /Applications for any homebrew GUI apps

{ config, pkgs, ... }:
let
  home-manager = import ./home-manager.nix {
    inherit pkgs;
    email = "randall@shipt.com";
    shellEnvExtra = ''
      export PATH=$PATH:$HOME/.local/bin
      export PATH=$PATH:$HOME/.rd/bin # Rancher Desktop
      export PATH=$PATH:$HOME/go/bin  # Go binaries
      export PATH=$PATH:$HOME/.npm-packages/bin
      export PATH=$PATH:$HOME/.rbenv/shims # solargraphs

      export NODE_PATH=$HOME/.npm-packages/lib/node_modules

      export PROJECT_ROOT=$HOME/dev

      export D2_LAYOUT=elk
    '';
    shellInitExtra = ''
      [[ -f "$HOME/dev/tools/profile" ]] && source "$HOME/dev/tools/profile"
      [[ -f /opt/homebrew/bin/brew ]]    && eval "$(/opt/homebrew/bin/brew shellenv)"
    '';
  };
  skhd = import ./skhd.nix;
  spacebar = import ./spacebar.nix { inherit pkgs; };
  yabai = import ./yabai.nix { inherit pkgs; };
in
{
  imports = [ <home-manager/nix-darwin> ];

  users.users.randall = {
    name = "randall";
    home = "/Users/randall";
  };

  home-manager.users.randall = home-manager;

  fonts.fonts = 
    [ pkgs.fira-code
	    pkgs.material-icons
	  ];

  nix.package = pkgs.nix;

  programs.zsh = {
    enable = true;
    enableFzfCompletion = true;
    enableFzfGit = true;
    enableFzfHistory = true;
  };
  
  services.nix-daemon.enable = true; # let nix manage itself
  services.skhd = skhd;
  services.spacebar = spacebar;
  services.yabai = yabai;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
