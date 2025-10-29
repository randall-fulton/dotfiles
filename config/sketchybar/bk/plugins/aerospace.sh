#!/usr/bin/env bash

# make sure it's executable with:
# chmod +x ~/.config/sketchybar/plugins/aerospace.sh

source "$(dirname "$0")/../theme.sh"

if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set $NAME \
               background.drawing=on \
               label.color=$ACCENT \
               icon.color=$ACCENT
else
    sketchybar --set $NAME \
               background.drawing=off \
               label.color=$FG \
               icon.color=$FG
fi
