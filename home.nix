{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "randall";
  home.homeDirectory = "/home/randall";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home = {
    file = {
      ".emacs.d".source =
        config.lib.file.mkOutOfStoreSymlink ./emacs;
      ".config/kitty".source = ./kitty;
      ".zshrc".source = ./.zshrc;
    };
    sessionVariables = {
      EDITOR = "emacs";
    };
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
    zsh = {
      enable = true;
      oh-my-zsh = {
        enable = true;
        plugins = ["git"];
      };
    };
  };
}
