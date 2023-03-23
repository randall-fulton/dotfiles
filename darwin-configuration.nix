{ config, pkgs, ... }:

{
  imports = [ <home-manager/nix-darwin> ];

  users.users.rfulton = {
    name = "rfulton";
    home = "/Users/rfulton";
  };
  home-manager.users.rfulton = { config, ... }: {
    # options: https://nix-community.github.io/home-manager/options.html
    home.stateVersion = "22.11";
    home.file = {
      ".config/kitty".source = ./kitty;
      ".config/nvim".source = config.lib.file.mkOutOfStoreSymlink ./nvim;
      ".emacs.d".source = config.lib.file.mkOutOfStoreSymlink ./emacs;
      ".zshrc".source = ./.zshrc;
    };
    home.sessionVariables = {
      EDITOR = "emacs";
    };
    
    programs = {
      # emacs.enable = true;
      fzf.enable = true;
      git = {
        enable = true;
        userName = "Randall Fulton";
        userEmail = "randall.ml.fulton@gmail.com";
      };
      go.enable = true;
      jq.enable = true;
      kitty.enable = true;
      neovim.enable = true;
      zsh = { # doing this with nix-darwin would enable easy fzf support
        enable = true;
        oh-my-zsh = {
          enable = true;
          plugins = ["git"];
        };
        # injected into .zshenv
        envExtra = ''
          . "$HOME/.cargo/env"
        '';
      };
    };
  };
  
  # options: https://daiderd.com/nix-darwin/manual/index.html
 
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [ pkgs.vim
    ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  programs.fish.enable = true;
  
  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    config = {
      focus_follows_mouse          = "autoraise";
      mouse_follows_focus          = "off";
      window_placement             = "second_child";
      window_opacity               = "off";
      window_opacity_duration      = "0.0";
      active_window_border_topmost = "off";
      window_topmost               = "on";
      window_shadow                = "float";
      active_window_opacity        = "1.0";
      normal_window_opacity        = "1.0";
      split_ratio                  = "0.50";
      auto_balance                 = "on";
      mouse_modifier               = "fn";
      mouse_action1                = "move";
      mouse_action2                = "resize";
      layout                       = "bsp";
      top_padding                  = 10;
      bottom_padding               = 10;
      left_padding                 = 10;
      right_padding                = 10;
      window_gap                   = 10;
    };
    extraConfig = ''
      yabai -m rule --add label=emacs app=Emacs manage=on
      yabai -m rule --add app='System Settings' manage=off
    '';
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
