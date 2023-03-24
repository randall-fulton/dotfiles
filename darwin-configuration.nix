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
      EDITOR = "nvim";
    };
    
    programs = {
      emacs.enable = true;
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
          eval "$(pyenv init --path)"
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
  programs.zsh = {
    enable = true;
    enableFzfCompletion = true;
    enableFzfGit = true;
    enableFzfHistory = true;
  };

  services.spacebar = {
    enable = true;
    package = pkgs.spacebar;
    config = {
      position                   = "top";
      display                    = "all";
      height                     = 36;
      title                      = "on";
      spaces                     = "on";
      clock                      = "on";
      power                      = "on";
      dnd                        = "on";
      padding_left               = 20;
      padding_right              = 20;
      spacing_left               = 25;
      spacing_right              = 25;
      text_font                  = ''"Fira Code:Regular:16.0"'';
      icon_font                  = ''"Material Icons:Regular:16.0"'';
      background_color           = "0xff202020";
      foreground_color           = "0xffa8a8a8";
      power_icon_color           = "0xffcd950c";
      battery_icon_color         = "0xffd75f5f";
      dnd_icon_color             = "0xffa8a8a8";
      clock_icon_color           = "0xffa8a8a8";
      power_icon_strip           = " ";
      space_icon                 = "•";
      space_icon_strip           = "1 2 3 4 5 6 7 8 9 10";
      spaces_for_all_displays    = "on";
      display_separator          = "on";
      display_separator_icon     = "";
      space_icon_color           = "0xff458588";
      space_icon_color_secondary = "0xff78c4d4";
      space_icon_color_tertiary  = "0xfffff9b0";
      clock_icon                 = "";
      dnd_icon                   = "";
      clock_format               = ''"%d/%m/%y %R"'';
      # shell options seem to break all updates of bar state until restart: https://github.com/cmacrae/spacebar/issues/101
      # right_shell                = "on";
      # right_shell_icon           = "";
      # right_shell_command        = "whoami";
    };
  };
  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    config = {
      external_bar                 = "all:36:0";
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
