#!/bin/sh

# make man pages easier to read on a wide display
export MANWIDTH=100

if [ -d "/store0/android-sdk" ]; then
    export ANDROID_HOME="/store0/android-sdk"
else
    export ANDROID_HOME="${HOME}/.local/lib/android-sdk"
fi
export ANDROID_NDK_HOME="${ANDROID_HOME}/ndk"
export RUST_SRC_PATH="/usr/lib/rustlib/src/rust"
export GOPATH="${HOME}/gocode"
export NPM_PACKAGES="${HOME}/.local"

export PATH="${HOME}/bin:\
${HOME}/.local/bin:\
${HOME}/dev/pyenv/bin:\
${ANDROID_HOME}/cmdline-tools/latest/bin:\
${ANDROID_HOME}/emulator:\
${ANDROID_HOME}/platform-tools:\
${HOME}/.cask/bin:\
${HOME}/.cargo/bin:\
${HOME}/.cabal/bin:\
${HOME}/.ghcup/bin:\
$GOPATH/bin:\
${HOME}/.gem/ruby/2.5.0/bin:\
${HOME}/.gem/ruby/2.7.0/bin:\
${HOME}/.local/lib/idea/bin:\
/usr/sbin:\
$PATH"

# make sure most shell sessions gets more complete PATH.
# this seems needed especially for Emacs started in an X session
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi

if [ ! -z "${ANDROID_NDK_HOME}" ]; then
    if [ -d "${ANDROID_NDK_HOME}" ]; then
        LATEST_NDK_DIR=$(ls -td ${ANDROID_NDK_HOME}/*/ | head -1)
        if ! [ -z "${LATEST_NDK_DIR}" ] && ! [ "${LATEST_NDK_DIR}" = "/tmp" ]; then
            PATH="$PATH:${LATEST_NDK_DIR}"
        fi
    fi
fi


export PROJECTS_HOME="${HOME}/dev"
export SDL_AUDIODRIVER="pulse"
export DOOMWADDIR="${HOME}/gms/doom"
export LIBVIRT_DEFAULT_URI=qemu:///system
export QEMU_AUDIO_DRV=pa
export JAVA_HOME=/usr/lib/jvm/default-java
export STUDIO_JDK=/usr/lib/jvm/default-java
export NAME="Martin Kjær Jørgensen"
export GOPRIVATE=bitbucket.shijidev.com
export MPD_HOST="${HOME}/.mpd/socket"
export MAVEN_OPTS="-Xmx8192m -Xms2048m -Djava.awt.headless=true"
export PYENV_ROOT="${HOME}/dev/pyenv"

HN=$(hostname -s)
if [ "$HN" = "ws" ] || [ "$HN" = "rw" ]; then
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
export ALTERNATE_EDITOR=/usr/bin/emacs

if [ -f ${HOME}/.profile.local ]; then
    . ${HOME}/.profile.local
fi
