#!/bin/sh
xrandr --output HDMI1 --off --output LVDS1 --mode 1280x800 --pos 2560x800 --rotate normal --output DP1 --off --output VGA1 --off
bash ~/bin/make_monitor_primary
xmodmap ~/.xmodmap
