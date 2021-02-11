#!/bin/bash

pkill dunst
pkill picom
pkill steam
pkill syncthing-gtk
pkill nm-applet
# pkill ssh-agent
pkill blueman-applet
pkill xss-lock
pkill xscreensaver
pkill xsettingsd
pkill redshift

# TODO: for some reason xresources aren't loaded
xrdb -load "$HOME/.Xresources"

# if command -v ssh-agent &> /dev/null; then
#    eval $(ssh-agent -s)
# fi

if command -v picom &> /dev/null; then
    picom -CGb
fi

if command -v xsetroot  &> /dev/null; then
    xsetroot -cursor_name left_ptr
fi

if command -v xset &> /dev/null; then
	xset s on s 1200 0
	xset dpms 0 1200 1203
fi

if command -v dunst &> /dev/null; then
    dunst &
fi

if command -v ibus-daemon &> /dev/null; then
    ibus-daemon -rdx --panel /usr/libexec/ibus-ui-gtk3
fi

if command -v diodon &> /dev/null; then
    diodon &
fi

if command -v syncthing-gtk &> /dev/null; then
    syncthing-gtk &
fi

if command -v nm-applet &> /dev/null; then
    nm-applet &
fi

if command -v radeon-profile &> /dev/null; then
    radeon-profile &
fi

if command -v flameshot &> /dev/null; then
    flameshot &
fi

if command -v blueman-applet &> /dev/null; then
    blueman-applet &
fi

if command -v xsettingsd &> /dev/null; then
    xsettingsd &
fi

# set here and masked in systemd --user to try avoid segfaults
# because systemd --user tries to start it to early
if command -v redshift &> /dev/null; then
	redshift &
fi

if [ "$HOSTNAME" = "ent" ]; then
	if command -v xset &> /dev/null; then
		xset -dpms
	fi
else
	if command -v xss-lock &> /dev/null; then
		xss-lock --transfer-sleep-lock -- i3lock -c 000000 --nofork &
	fi
fi

exec i3
