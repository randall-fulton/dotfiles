# inspo: https://www.reddit.com/r/unixporn/comments/vtfy73/yabaiskhd_my_macos_workflow_with_a_tiling_window/
{ pkgs, ... }:
{
  enable = true;
  package = pkgs.yabai;
  config = {
    external_bar                 = "all:36:0";
    focus_follows_mouse          = "off"; # autofocus
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
}
