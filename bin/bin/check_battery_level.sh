#!/bin/bash
set -e
warn_hit_file=/dev/shm/bat_warn_hit
crit_hit_file=/dev/shm/bat_crit_hit

status=Charging
if [ -f /sys/class/power_supply/BAT0/status ]; then
    status=$(</sys/class/power_supply/BAT0/status)
fi
capacity=Discharging
if [ -f /sys/class/power_supply/BAT0/capacity ]; then
    capacity=$(</sys/class/power_supply/BAT0/capacity)
fi
emerg_level=3
crit_level=5
warn_level=15

if [ ! "$status" == "Discharging" ]; then
    echo 0 > $warn_hit_file
    echo 0 > $crit_hit_file
    exit 0
fi

if [ $capacity -le $emerg_level ]; then
    systemctl suspend
fi

crit_hit=0
if [ -f $crit_hit_file ]; then
  crit_hit=$(<"$crit_hit_file")
fi
warn_hit=0
if [ -f $warn_hit_file ]; then
    warn_hit=$(<"$warn_hit_file")
fi
if [ $capacity -le $warn_level ] && [ $warn_hit -eq 0 ]; then
    echo 1 > $warn_hit_file
    notify-send "low battery" "warning: capacity is ${capacity}" -u normal
fi
if [ $capacity -le $crit_level ] && [ $crit_hit -eq 0 ]; then
    echo 1 > $crit_hit_file
    notify-send "low battery" "critical: capacity is ${capacity}" -u critical
fi
if [ $capacity -gt $warn_level ]; then
    echo 0 > $warn_hit_file
    echo 0 > $crit_hit_file
fi
