#!/bin/bash

pkill picom
pkill urxvtd
pkill steam
pkill syncthing-gtk
pkill nm-applet
pkill redshift-gtk
pkill pasystray
pkill ssh-agent
pkill blueman-applet
pkill xscreensaver

if command -v ssh-agent &> /dev/null; then
    eval $(ssh-agent -s)
fi

if command -v picom &> /dev/null; then
    picom -CGb
fi

if command -v urxvtd &> /dev/null; then
    urxvtd -q -o -f
fi

if command -v xsetroot  &> /dev/null; then
    xsetroot -cursor_name left_ptr
fi

if command -v xset &> /dev/null; then
    xset s off
fi

if command -v ibus-daemon &> /dev/null; then
    ibus-daemon -rdx
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

if command -v redshift-gtk  &> /dev/null; then
    redshift-gtk &
fi

if command -v pasystray &> /dev/null; then
    pasystray &
fi

if command -v blueman-applet &> /dev/null; then
    blueman-applet &
fi

if command -v xss-lock &> /dev/null; then
    xss-lock --transfer-sleep-lock -- i3lock -c 000000 --nofork &
fi

exec i3
