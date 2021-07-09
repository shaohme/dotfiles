#!/bin/sh

# make man pages easier to read on a wide display
export MANWIDTH=100

if [ -d "/store0/android-sdk" ]; then
    export ANDROID_HOME="/store0/android-sdk"
else
    export ANDROID_HOME="${HOME}/.local/lib/android-sdk"
fi
export ANDROID_NDK_HOME="${ANDROID_HOME}/ndk"
export GOPATH="${HOME}/gocode"
export NPM_PACKAGES="${HOME}/.local"

export PATH="${HOME}/bin:\
${HOME}/.local/bin:\
${HOME}/dev/pyenv/bin:\
${HOME}/dev/pyenv/shims:\
${HOME}/.sdkman/candidates/visualvm/current/bin:\
${HOME}/.sdkman/candidates/springboot/current/bin:\
${HOME}/.sdkman/candidates/scala/current/bin:\
${HOME}/.sdkman/candidates/sbt/current/bin:\
${HOME}/.sdkman/candidates/pomchecker/current/bin:\
${HOME}/.sdkman/candidates/maven/current/bin:\
${HOME}/.sdkman/candidates/kotlin/current/bin:\
${HOME}/.sdkman/candidates/java/current/bin:\
${HOME}/.sdkman/candidates/groovy/current/bin:\
${HOME}/.sdkman/candidates/grails/current/bin:\
${HOME}/.sdkman/candidates/gradle/current/bin:\
${HOME}/.sdkman/candidates/btrace/current/bin:\
${HOME}/.sdkman/candidates/ant/current/bin:\
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
${HOME}/.local/share/gem/ruby/2.7.0/bin:\
${HOME}/.local/lib/clojure/bin:\
${HOME}/.local/lib/idea/bin:\
${HOME}/.local/lib/go/bin:\
${HOME}/.local/lib/conduktor/bin:\
${HOME}/.yarn/bin:\
${HOME}/dev/kotlin-language-server/server/build/install/server/bin:\
${HOME}/dev/kotlin-debug-adapter/adapter/build/install/adapter/bin:\
/usr/sbin:\
$PATH"

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
# export STUDIO_JDK=/usr/lib/jvm/default-java
export NAME="Martin Kjær Jørgensen"
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
if [ "$HN" = "nws" ]; then
    export GPGKEY=62B2146BFB6047DBB89A80E11EED4573F176E739
    export EMAIL="mkj@nine.dk"
fi

# export PYTHON_VIRTUALENV_DIR="${HOME}/.virtualenvs"
export WINEARCH=win32
export WINEPREFIX="${HOME}/.wine32"

# vaapi on GST shows no features on introspection running on
# amd hardware. try override this for now, and trust mesa vaapi driver quality
export GST_VAAPI_ALL_DRIVERS=1

export VISUAL="${HOME}/bin/emacs"
export EDITOR="${HOME}/bin/emacs"
export ALTERNATE_EDITOR=/usr/bin/emacs

# make sure most shell sessions gets more complete PATH.
# this seems needed especially for Emacs started in an X session

if [ -f "$HOME/.cargo/env" ]; then
    . ${HOME}/.cargo/env
fi

if [ -f ${HOME}/.profile.local ]; then
    . ${HOME}/.profile.local
fi
