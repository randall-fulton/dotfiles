{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "randall";
  home.homeDirectory = "/Users/randall";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "25.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    pkgs.aerospace
    pkgs.lua
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # ''
    # ".config/sketchybar".source = ./config/sketchybar;
  };

  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  programs = {
    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    aerospace = {
      enable = true;
      launchd.enable = true;
      userSettings = {
        exec-on-workspace-change = [
          "/bin/bash"
          "-c"
          "sketchybar --trigger aerospace_workspace_change FOCUSED_WORKSPACE=$AEROSPACE_FOCUSED_WORKSPACE"
        ];
        gaps = {
          inner.horizontal = 8;
          inner.vertical = 8;
          outer.left = 8;
          outer.right = 8;
          outer.bottom = 8;
          outer.top = 8;
        };
        mode.main.binding = {
          # Workspace switching
          ctrl-1 = "workspace 1";
          ctrl-2 = "workspace 2";
          ctrl-3 = "workspace 3";
          ctrl-4 = "workspace 4";
          ctrl-5 = "workspace 5";
          ctrl-6 = "workspace 6";
          ctrl-7 = "workspace 7";
          ctrl-8 = "workspace 8";
          ctrl-9 = "workspace 9";
          ctrl-0 = "workspace 10";

          ctrl-shift-1 = "move-node-to-workspace 1";
          ctrl-shift-2 = "move-node-to-workspace 2";
          ctrl-shift-3 = "move-node-to-workspace 3";
          ctrl-shift-4 = "move-node-to-workspace 4";
          ctrl-shift-5 = "move-node-to-workspace 5";
          ctrl-shift-6 = "move-node-to-workspace 6";
          ctrl-shift-7 = "move-node-to-workspace 7";
          ctrl-shift-8 = "move-node-to-workspace 8";
          ctrl-shift-9 = "move-node-to-workspace 9";
          ctrl-shift-0 = "move-node-to-workspace 10";

          # Window focus and arrangement
          alt-ctrl-h = "focus --boundaries-action wrap-around-the-workspace left";
          alt-ctrl-j = "focus --boundaries-action wrap-around-the-workspace down";
          alt-ctrl-k = "focus --boundaries-action wrap-around-the-workspace up";
          alt-ctrl-l = "focus --boundaries-action wrap-around-the-workspace right";
          alt-shift-h = "move left";
          alt-shift-j = "move down";
          alt-shift-k = "move up";
          alt-shift-l = "move right";

          alt-ctrl-y = "join-with left";
          alt-ctrl-u = "join-with down";
          alt-ctrl-i = "join-with up";
          alt-ctrl-o = "join-with right";

          alt-ctrl-f = "fullscreen";
          alt-ctrl-s = "layout v_accordion";
          alt-ctrl-w = "layout h_accordion";
          alt-ctrl-e = "layout tiles horizontal vertical";
          alt-shift-space = "layout floating tiling";
        };
      };
    };

    sketchybar = {
      enable = true;
      configType = "lua";
      config = {
        source = ./config/sketchybar;
        recursive = true;
      };
      service = {
        outLogFile = "/Users/randall/.config/sketchybar/sketchybar.log";
        errorLogFile = "/Users/randall/.config/sketchybar/sketchybar.log";
      };
    };
  };

  # TODO: launchctl setenv PATH "/usr/local/bin:/usr/bin:/bin:/Users/randall/.nix-profile/bin"
  # TODO: launchctl setenv LUA_PATH /Users/randall/.config/sketchybar/?.lua
}
