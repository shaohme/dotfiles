#!/bin/bash
set -e
set -x
VAR_FILE="${XDG_RUNTIME_DIR}/theme"
# 0 = dark
# 1 = light

LIGHT=1

CONFIG_DIR=$HOME/.config

if [ -f "$VAR_FILE" ]; then
	LIGHT=$(<${VAR_FILE})
fi

if [ -z $LIGHT ]; then
	LIGHT=1
fi

if [ "$1" = "t" ]; then
	# invert
	if [ $LIGHT -eq 0 ]; then
		LIGHT=1
	else
		LIGHT=0
	fi
fi

echo -n $LIGHT > $VAR_FILE

cp $HOME/.Xresources.base $HOME/.Xresources
cp $HOME/.config/i3/base.config $HOME/.config/i3/config
cp $HOME/.config/gtk-3.0/settings.base.ini $HOME/.config/gtk-3.0/settings.ini
rm -f $HOME/.config/kitty/theme.conf

if [ $LIGHT -eq 0 ]; then
	# set dark themes
    ln -s $HOME/.config/kitty/dark.conf $HOME/.config/kitty/theme.conf 
    cat $HOME/.Xresources.dark-colors >> $HOME/.Xresources
    cat $HOME/.config/i3/dark-colors.config >> $HOME/.config/i3/config
    cp $HOME/.xsettingsd-dark-colors $HOME/.xsettingsd
    cat $HOME/.config/gtk-3.0/dark-colors.ini >> $HOME/.config/gtk-3.0/settings.ini
	WALLPAPER=default-dark
else
	# set light themes
    ln -s $HOME/.config/kitty/light.conf $HOME/.config/kitty/theme.conf 
    cat $HOME/.Xresources.light-colors >> $HOME/.Xresources
    cat $HOME/.config/i3/light-colors.config >> $HOME/.config/i3/config
    cp $HOME/.xsettingsd-light-colors $HOME/.xsettingsd
    cat $HOME/.config/gtk-3.0/light-colors.ini >> $HOME/.config/gtk-3.0/settings.ini
	WALLPAPER=default-light
fi

feh --bg-fill "$HOME/pics/wallpapers/$WALLPAPER"

i3-msg reload

pkill xsettingsd || true

xrdb -load $HOME/.Xresources

if [ -S $XDG_RUNTIME_DIR/emacs/server ]; then
    if [ $LIGHT -eq 0 ]; then
	    emacsclient --eval "(load-dark-theme)"
    else
        emacsclient --eval "(load-light-theme)" || true
    fi
fi

# settings empty values first and wait for a while seems to make some
# gtk apps like firefox behave on change
xsettingsd -c $HOME/.xsettingsd.empty &

sleep 1

pkill xsettingsd

xsettingsd &

sleep 1
