let
  appMod = "hyper";
  yabaiMod = "shift+alt";
in
{
  enable = true;
  skhdConfig = ''
    ${appMod} - q : open -a Slack
    ${appMod} - w : open -a Firefox
    ${appMod} - e : open -a Emacs
    ${appMod} - r : open -a Kitty
    ${appMod} - t : open -a Wally

    ${yabaiMod} - f     : yabai -m window --toggle float; yabai -m window --grid 7:5:1:1:3:5
    ${yabaiMod} - p     : yabai -m window --toggle zoom-fullscreen
    ${yabaiMod} - h     : yabai -m window --swap west
    ${yabaiMod} - j     : yabai -m window --swap south
    ${yabaiMod} - k     : yabai -m window --swap north
    ${yabaiMod} - l     : yabai -m window --swap east
    ${yabaiMod} - y     : yabai -m window --focus west
    ${yabaiMod} - u     : yabai -m window --focus south
    ${yabaiMod} - i     : yabai -m window --focus north
    ${yabaiMod} - o     : yabai -m window --focus east
    ${yabaiMod} - b     : yabai -m space --balance
    ${yabaiMod} - t     : yabai -m space --toggle padding; yabai -m space --toggle gap
    ${yabaiMod} - s     : yabai -m window --toggle split
    ${yabaiMod} - left  : yabai -m window --resize left:-100:0
    ${yabaiMod} - down  : yabai -m window --resize bottom:0:100
    ${yabaiMod} - up    : yabai -m window --resize bottom:0:-100
    ${yabaiMod} - right : yabai -m window --resize left:100:0
  '';
}
