{
  pkgs,
  email ? "randall@randallfulton.com",
  # shellAliases ? [],
  shellEnvExtra ? "",
  shellInitExtra ? "",
  ...
}:
let
  git = import ./git.nix { email = email; };
in
{ config, lib, ... }: {
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

  # TODO: make usage/dependencies clear
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
      pkgs.ispell
      pkgs.kcat
      pkgs.libksba
      pkgs.libtool
      pkgs.libyaml
      # pkgs.ngrok
      pkgs.nodejs
      pkgs.nodePackages.pyright
      pkgs.openssl_1_1 # needed by brew (for rvm)
      pkgs.pandoc
      pkgs.pgcli
      pkgs.pkg-config
      pkgs.postgresql
      pkgs.protobuf
      pkgs.protoc-gen-go
      pkgs.rbenv
      pkgs.readline
      pkgs.ripgrep
      pkgs.rustup
      pkgs.yarn
      pkgs.zlib
      pkgs.zoxide
	  ];
  
  programs = {
    inherit git;
    
    fzf.enable = true;
    
    go = {
      enable = true;
      package = pkgs.go_1_20;
      goPrivate =
        [ "github.com/shipt/*"
        ];
    };
    
    jq.enable = true;
    
    kitty = {
	    enable = false;
	    font = {
	      name = "Fira Code";
	      size = 36.0;
	    };
	    darwinLaunchOptions =
	      [ "--config=/Users/randall/.config/kitty/kitty.conf"
	      ];
	    settings = {
	      tab_title_template = "{tab.active_wd}";
	      remember_window_size = "yes";
	      hide_window_decorations = "yes";
	      tab_bar_style = "powerline";
	      tab_switch_strategy = "right";
	      macos_option_as_alt = "yes";
	    };	
	    keybindings = {
	      "kitty_mod+enter" = "new_window_with_cwd";
	      "kitty_mod+f" = "goto_layout fat";
	      "kitty_mod+s" = "goto_layout stack";
	      "kitty_mod+," = "load_config_file";
	    };
	    # extraConfig = ''
	    #   include ./theme.conf
	    #   include current-theme.conf
	    # '';
	  };
    
    neovim.enable = true;
    
    zsh = {
      enable = true;
      oh-my-zsh = {
        enable = true;
        plugins = ["git"];
      };
      envExtra = shellEnvExtra;
      initExtra = ''
        command -v pyenv >/dev/null        && eval "$(pyenv init --path)"
        command -v rbenv >/dev/null        && eval "$(rbenv init - zsh)"
        command -v starship >/dev/null     && eval "$(starship init zsh)"
        command -v zoxide >/dev/null       && eval "$(zoxide init zsh)"
        [[ -f "$HOME/.cargo/env" ]]        && . "$HOME/.cargo/env"
        ${shellInitExtra}
      '';
      # TODO: allow passing in additional aliases
      shellAliases = {
        refresh = "source $HOME/.zshrc";
        restart-skhd = "launchctl kickstart -k \"gui/\${UID}/org.nixos.skhd\"";
        restart-spacebar = "launchctl kickstart -k \"gui/\${UID}/org.nixos.spacebar\"";
        restart-yabai = "launchctl kickstart -k \"gui/\${UID}/org.nixos.yabai\"";
        switch = "darwin-rebuild switch";
      };
    };
  };
}
