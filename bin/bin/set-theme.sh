#!/bin/bash
set -x
set -e
VAR_FILE="${XDG_RUNTIME_DIR}/theme"
# 0 = dark
# 1 = neutral/default
# 2 = light

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

if [ $LIGHT -eq 0 ]; then
	# set dark themes
	GTK_PREFER_DARK_THEMES=true
	GTK_THEME_NAME=NumixSolarizedDarkMagenta
	GTK_ICON_NAME=Numix
	EMACS_THEME=solarized-dark
	X_BACKGROUND=S_base03
	X_FOREGROUND=S_base0
	X_FADECOLOR=S_base03
	X_CURSORCOLOR=S_base1
	X_POINTERCOLORBACKGROUND=S_base01
	X_POINTERCOLORFOREGROUND=S_base1
	I3_BACKGROUND=#002b36
	I3_SEPARATOR=#073642
	I3_STATUSLINE=#eee8d5
	I3_TEXT_ACTIVE=#eee8d5
	I3_INACTIVE_BACKGROUND=#073642
	I3_BORDER_INACTIVE=#073642#
	I3_TEXT_INACTIVE=#93a1a1
	I3_FOCUSED_BACKGROUND=#d33682
	I3_FOCUSED_BORDER=#d33682
	WALLPAPER=default-dark
else
	# set light themes
	GTK_PREFER_DARK_THEMES=false
	GTK_THEME_NAME=NumixSolarizedLightCyan
	GTK_ICON_NAME=Numix-Light
	EMACS_THEME=solarized-light
	X_BACKGROUND=S_base3
	X_FOREGROUND=S_base00
	X_FADECOLOR=S_base3
	X_CURSORCOLOR=S_base01
	X_POINTERCOLORBACKGROUND=S_base1
	X_POINTERCOLORFOREGROUND=S_base01
	I3_BACKGROUND=#fdf6e3
	I3_SEPARATOR=#073642
	I3_STATUSLINE=#073642
	I3_TEXT_ACTIVE=#eee8d5
	I3_INACTIVE_BACKGROUND=#eee8d5
	I3_BORDER_INACTIVE=#93a1a1
	I3_TEXT_INACTIVE=#073642
	I3_FOCUSED_BACKGROUND=#2aa198
	I3_FOCUSED_BORDER=#2aa198
	WALLPAPER=default-light
fi

feh --bg-fill "$HOME/pics/wallpapers/$WALLPAPER"

sed -e "\
	s/SR_BACKGROUND/${X_BACKGROUND}/g;
	s/SR_FOREGROUND/${X_FOREGROUND}/g;
	s/SR_FADECOLOR/${X_FADECOLOR}/g;
	s/SR_CURSORCOLOR/${X_CURSORCOLOR}/g;
	s/SR_POINTERCOLORBACKGROUND/${X_POINTERCOLORBACKGROUND}/g;
	s/SR_POINTERCOLORFOREGROUND/${X_POINTERCOLORFOREGROUND}/g;
" $HOME/.template.Xresources > $HOME/.Xresources

xrdb -load $HOME/.Xresources

sed -e "\
	s/SR_BACKGROUND/${I3_BACKGROUND}/g;
	s/SR_SEPARATOR/${I3_SEPARATOR}/g;
	s/SR_STATUSLINE/${I3_STATUSLINE}/g;
	s/SR_TEXT_ACTIVE/${I3_TEXT_ACTIVE}/g;
	s/SR_INACTIVE_BACKGROUND/${I3_INACTIVE_BACKGROUND}/g;
	s/SR_BORDER_INACTIVE/${I3_BORDER_INACTIVE}/g;
	s/SR_TEXT_INACTIVE/${I3_TEXT_INACTIVE}/g;
	s/SR_FOCUSED_BACKGROUND/${I3_FOCUSED_BACKGROUND}/g;
	s/SR_FOCUSED_BORDER/${I3_FOCUSED_BORDER}/g;
" $CONFIG_DIR/i3/template.config > $CONFIG_DIR/i3/config

i3-msg reload

if [ -S $XDG_RUNTIME_DIR/emacs/server ]; then
	emacsclient --eval "(load-theme '${EMACS_THEME} t)"
fi

sed -e "\
	s/SR_PREFER_DARK_THEMES/${GTK_PREFER_DARK_THEMES}/g;
	s/SR_THEME_NAME/${GTK_THEME_NAME}/g;
	s/SR_ICON_NAME/${GTK_ICON_NAME}/" $CONFIG_DIR/gtk-3.0/template.settings.ini > $CONFIG_DIR/gtk-3.0/settings.ini

sed -e "\
	s/SR_THEME_NAME/${GTK_THEME_NAME}/g;
	s/SR_ICON_NAME/${GTK_ICON_NAME}/" $HOME/.template.xsettingsd > $HOME/.xsettingsd


# settings empty values first and wait for a while seems to make some
# gtk apps like firefox behave on change
pkill xsettingsd || true

xsettingsd -c $HOME/.empty.xsettingsd &

sleep 1

pkill xsettingsd

xsettingsd  &
