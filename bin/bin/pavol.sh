#!/bin/dash
default_sink=$(pactl info | grep -i "default\ sink" | awk ' {print $3}')

if [ "$1" = "toggle" ]
then
    pactl set-sink-mute @DEFAULT_SINK@ toggle

elif [ "$1" = "mictoggle" ]
then
    pactl set-source-mute @DEFAULT_SOURCE@ toggle
else
    volume=0
    sink_hit=0
    found_default=0
    is_default=0
    prev_was_name=0
    found_vol=0
    found_left=0
    for word in $(pactl list sinks)
    do
        if [ $word = "Sink" ]
        then
            sink_hit=1
            is_default=0
            prev_was_name=0
            found_vol=0
            found_left=0
        fi
        if [ $found_left = 1 ]
        then
            # | grep -Fqe "$2\%"
            v=$(printf '%s\n' "$word" | grep "\%" | sed "s/\%//g")
            if [ $v ]
            then
                volume=$v
            fi
        fi
        if [ $found_vol = 1 ] && [ $word = "front-left:" ]
        then
            found_left=1
        fi
        if [ $is_default = 1 ] && [ $word = "Volume:" ]
        then
            found_vol=1
            found_left=0
        fi
        if [ $prev_was_name = 1 ]
        then
            prev_was_name=0
            if [ $word = $default_sink ]
            then
                is_default=1
                found_default=1
            fi
        fi
        if [ $word = "Name:" ]
        then
            prev_was_name=1
        fi
        # if [ $sink_hit = 1 ] && [ $is_default = 1 ]
        # then
        #     # echo "hit"
        # fi
        # printf 'W:%s\n' "$word"
    done

    if [ "$1" = "up" ]
    then
        volume=$((volume + 3))
    fi
    if [ "$1" = "down" ]
    then
        volume=$((volume - 3))
    fi

    if [ $found_default = 0 ]
    then
        exit 1
    fi
    if [ ! $volume ]
    then
        exit 2
    fi
    volume=$((volume > 100 ? 100 : volume))
    volume=$((volume < 0 ? 0 : volume))
    # echo "OUT:"$volume
    # pactl set-sink-volume @DEFAULT_SINK@ $volume%
    pactl set-sink-volume $default_sink $volume%
fi
