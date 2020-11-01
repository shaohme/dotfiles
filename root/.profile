#!/bin/sh

if [ -d "/store0/android-sdk" ]; then
    export ANDROID_HOME="/store0/android-sdk"
else
    export ANDROID_HOME="${HOME}/.local/lib/android-sdk"
fi
export ANDROID_NDK_HOME="${ANDROID_HOME}/ndk"
export RUST_SRC_PATH="/usr/lib/rustlib/src/rust"
export GOPATH="${HOME}/gocode"
export NPM_PACKAGES="${HOME}/.local"

export PATH="${HOME}/bin:${HOME}/.local/bin:/usr/sbin:/usr/games/bin:${ANDROID_HOME}/cmdline-tools/latest/bin:${ANDROID_HOME}/emulator:${ANDROID_HOME}/platform-tools:${HOME}/.cask/bin:/usr/local/games:${HOME}/.cargo/bin:${HOME}/.cabal/bin:${HOME}/.ghcup/bin:$GOROOT/bin:$GOPATH/bin:${HOME}/.gem/ruby/2.5.0/bin:${HOME}/.gem/ruby/2.7.0/bin:${HOME}/.local/lib/idea/bin:$PATH"

export PROJECTS_HOME="${HOME}/dev"
export SDL_AUDIODRIVER="pulse"
export DOOMWADDIR="${HOME}/gms/doom"
export LIBVIRT_DEFAULT_URI=qemu:///system
export QEMU_AUDIO_DRV=pa
export JAVA_HOME=/usr/lib/jvm/default-java
export NAME="Martin Kjær Jørgensen"

HN=$(hostname -s)
if [ "$HN" = "mkjws" ] || [ "$HN" = "rw" ]; then
    export GPGKEY=FD439D24F94DAA68631957D3C57BA9E100588495
    export EMAIL="mkj@gotu.dk"
fi
if [ "$HN" = "dev6" ]; then
    export GPGKEY=7CBFA3101F57BA3890AC00F353A48C3B78F2850E
    export EMAIL="martin.jorgensen@shijigroup.com"
fi

export PYTHON_VIRTUALENV_DIR="${HOME}/.virtualenvs"
export WINEARCH=win32
export WINEPREFIX="${HOME}/.wine32"

# vaapi on GST shows no features on introspection running on
# amd hardware. try override this for now, and trust mesa vaapi driver quality
export GST_VAAPI_ALL_DRIVERS=1

export VISUAL="${HOME}/bin/emacs"
export EDITOR="${HOME}/bin/emacs"

if [ -f ${HOME}/.profile.local ]; then
    . ${HOME}/.profile.local
fi
