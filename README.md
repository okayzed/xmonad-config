# about

this is my xmonad+xfce4 config.

## installation

```
apt install xmobar xmonad rofi xfce4
git clone https://github.com/okayzed/xmonad-config ~/.xmonad
ln -s ~/.xmonad/xmobarrc ~/.xmobarrc
ln -s ~/.xmonad/xmodmap ~/.Xmodmap

```

## troubleshooting

if xfce4 desktop keeps taking over, uninstall xfce4-desktop package

## keybindings

```
super+[0+9]: switch to desktop
shift+super+[0+9]: move window to desktop
super+[j,k]: switch window
shift+super+[j,k]: move window
super+[h,l]: resize window
super+[a,s,d,f,g]: switch between layouts: floating, twopane, tiled, fullscreen, grid
ctrl+space: bring up rofi command launcher
super+semi-colon: dmenu command launcher
super+m: minimize window
shift+super+min: unminimize window
super+b: hide/unhide status bars
super+q: restart xmonad
shift+super+q: exit xmonad
super+t: place window into tiling
super+mouse drag: move window
super+right click drag: resize window
```
