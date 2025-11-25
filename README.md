# about

this is my xmonad+xfce4 config.

## installation

```
apt install xmobar xmonad rofi xfce4
git clone https://github.com/okayzed/xmonad-config ~/.xmonad
mkdir -p ~/.config/polybar
ln -s ~/.xmonad/polybar.ini ~/.config/polybar/config.ini
ln -s ~/.xmonad/xmodmap ~/.Xmodmap

```

## troubleshooting

if xfce4 desktop keeps taking over, uninstall xfce4-desktop package

## keybindings

```
mod+[0+9]: switch to desktop
shift+mod+[0+9]: move window to desktop
mod+[j,k]: switch window
shift+mod+[j,k]: move window
mod+[h,l]: move pointer to monitor
shift+mod+[h,l]: move window to monitor
mod+[a,s,d,f,g]: switch between layouts: floating, twopane, tiled, fullscreen, grid
ctrl+space: bring up rofi command launcher
mod+semi-colon: dmenu command launcher
mod+m: minimize window
shift+mod+min: unminimize window
mod+b: hide/unhide status bars
mod+q: restart xmonad
shift+mod+q: exit xmonad
mod+t: place window into tiling
mod+mouse drag: move window
mod+right click drag: resize window
```
