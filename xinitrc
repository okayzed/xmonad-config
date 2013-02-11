#!/usr/bin/env bash

export DMENU_OPTIONS='-b -nb black -nf white'

xmodmap ~/.xmodmap
bash ~/.xmonad/hooks startup
