{ config, pkgs, ... }:

# TODO: install Fira Code Nerd Font
# TODO: install starship
# TODO: auto-hide menu bar
# TODO: setup homebrew
# TODO: setup alfred and disable spotlight
# TODO: install emacs
# TODO: configure yabai to ignore emacs lsp-ui popups
# TODO: explore yabai spaces for tab-like setup https://github.com/koekeishiya/yabai/issues/203
# TODO: install kitty
# TODO: create aliases in /Applications for any homebrew GUI apps

{
  imports = [ <home-manager/nix-darwin> ];

  users.users.randall = {
    name = "randall";
    home = "/Users/randall";
  };
  home-manager.users.randall = { config, lib, ... }: {
    # options: https://nix-community.github.io/home-manager/options.html
    home.stateVersion = "22.11";

	nix.extraOptions = ''
	  extra-experimental-features = nix-command flakes
	'';
	
    home.file = {
	    ".config/kitty".source = config.lib.file.mkOutOfStoreSymlink ./kitty;
      ".config/nvim".source = config.lib.file.mkOutOfStoreSymlink ./nvim;
      ".config/starship.toml".source = config.lib.file.mkOutOfStoreSymlink ./starship.toml;
      ".emacs.d".source = config.lib.file.mkOutOfStoreSymlink ./emacs;
      ".npmrc".source = ./npmrc;
    };

    home.sessionVariables = {
      EDITOR = "nvim";
    };

    home.packages =
      [ pkgs.autoconf
        pkgs.automake
        pkgs.cmake
        pkgs.confluent-platform
        pkgs.coreutils-full
        pkgs.delve
        pkgs.docker
        pkgs.findutils
        pkgs.gawk 
        pkgs.getopt
        pkgs.gh
        pkgs.gnugrep
        pkgs.gnupg
        pkgs.gnused
        pkgs.gnutar
        pkgs.gnutls
        pkgs.gopls
        pkgs.golangci-lint
        pkgs.go-swagger
        pkgs.graphviz
        pkgs.indent
        pkgs.libksba
        pkgs.libtool
        pkgs.libyaml
        # pkgs.ngrok
        pkgs.nodejs
        pkgs.openssl_1_1 # needed by brew (for rvm)
        pkgs.pandoc
        pkgs.pgcli
        pkgs.pkg-config
        pkgs.postgresql
        pkgs.protobuf
        pkgs.protoc-gen-go
        pkgs.rbenv
        pkgs.ripgrep
        pkgs.readline
        pkgs.yarn
        pkgs.zlib
	    ];
    
    programs = {
      # emacs.enable = true;
      fzf.enable = true;
      git = {
        enable = true;
        userName = "Randall Fulton";
        userEmail = "randall@shipt.com";
        ignores = [
          # emacs
          "*/.saves"
          "*~"
          "*#*"
          ".dir-locals.el"
          # python
          "*venv/"
          "*.venv/"
        ];
        extraConfig = {
          url = {
            "ssh://git@github.com/" = {
              insteadOf = "https://github.com/";
            };
          };
        };
      };
      go = {
        enable = true;
        package = pkgs.go_1_20;
        goPrivate =
          [ "github.com/shipt/*"
          ];
      };
      jq.enable = true;
    # kitty = {
	  #   enable = true;
	  #   font = {
	  #     name = "Fira Code";
	  #     size = 36.0;
	  #   };
	  #   darwinLaunchOptions =
	  #     [ "--config=/Users/randall/.config/kitty/kitty.conf"
	  #     ];
	  #   settings = {
	  #     tab_title_template = "{tab.active_wd}";
	  #     remember_window_size = "yes";
	  #     hide_window_decorations = "yes";
	  #     tab_bar_style = "powerline";
	  #     tab_switch_strategy = "right";
	  #     macos_option_as_alt = "yes";
	  #   };	
	  #   keybindings = {
	  #     "kitty_mod+enter" = "new_window_with_cwd";
	  #     "kitty_mod+f" = "goto_layout fat";
	  #     "kitty_mod+s" = "goto_layout stack";
	  #     "kitty_mod+," = "load_config_file";
	  #   };
	  #   # extraConfig = ''
	  #   #   include ./theme.conf
	  #   #   include current-theme.conf
	  #   # '';
	  # };
      neovim.enable = true;
      zsh = { # doing this with nix-darwin would enable easy fzf support
        enable = true;
        oh-my-zsh = {
          enable = true;
          plugins = ["git"];
        };
        shellAliases = {
          refresh = "source $HOME/.zshrc";
          restart-skhd = "launchctl kickstart -k \"gui/\${UID}/org.nixos.skhd\"";
          restart-spacebar = "launchctl kickstart -k \"gui/\${UID}/org.nixos.spacebar\"";
          restart-yabai = "launchctl kickstart -k \"gui/\${UID}/org.nixos.yabai\"";
          switch = "darwin-rebuild switch";
        };
		    envExtra = ''
          export NIX_BUILD_SHELL=zsh

          export PATH=$PATH:$HOME/.rd/bin # Rancher Desktop
          export PATH=$PATH:$HOME/go/bin  # Go binaries
          export PATH=$PATH:$HOME/.npm-packages/bin

          export NODE_PATH=$HOME/.npm-packages/lib/node_modules

          export PROJECT_ROOT=$HOME/dev

          # orthogonal connections; significantly more readable graphs
          export D2_LAYOUT=elk
        '';
        initExtra = ''
          if [[ -d "$HOME/.cargo/" ]]; then
             . "$HOME/.cargo/env"
          fi

          if command -v pyenv >/dev/null; then
             eval "$(pyenv init --path)"
          fi

          if command -v starship >/dev/null; then
             eval "$(starship init zsh)"
          fi

          if command -v rbenv >/dev/null; then
             eval "$(rbenv init - zsh)"
          fi

          if [ -f $HOME/dev/tools/profile ]; then
             source "$HOME/dev/tools/profile"
          fi

          eval "$(/opt/homebrew/bin/brew shellenv)"
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

  fonts.fonts = 
    [ pkgs.fira-code
	    pkgs.material-icons
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

  # services.emacs.enable = true;
  
  services.skhd = {
    enable = true;
    skhdConfig = ''
      hyper - q : open -a Slack
      hyper - w : open -a Firefox
      hyper - e : open -a Emacs
      hyper - r : open -a Kitty
      hyper - t : open -a Wally

      # float active window and center
      hyper - f : yabai -m window --toggle float; yabai -m window --grid 7:5:1:1:3:5
    '';
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
      focus_follows_mouse          = "autofocus";
      mouse_follows_focus          = "off";
      window_placement             = "first_child";
      window_opacity               = "off";
      window_opacity_duration      = "0.0";
      active_window_border_topmost = "off";
      window_topmost               = "on";
      window_shadow                = "float";
      active_window_opacity        = "1.0";
      normal_window_opacity        = "1.0";
      split_ratio                  = "0.50";
      auto_balance                 = "off";
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
      yabai -m rule --add app=emacs manage=on
      yabai -m rule --add app=emacs-28.2 manage=on
      yabai -m rule --add app=Emacs manage=on
      yabai -m rule --add app='System Settings' manage=off
    '';
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
