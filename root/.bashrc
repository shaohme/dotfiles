# return if non-interactive
[[ $- == *i* ]] || return

# return if not executed in terminal
# if ! [ -t 1 ] ; then
#     return
# fi

# don't care about history duplicates
HISTCONTROL=ignoreboth:erasedups
HISTSIZE=10000
HISTFILESIZE=20000

shopt -s histappend
shopt -s checkwinsize

# do not set tab width. break emacs when deleting below column 4

# tabs 4

if [ "$(id -u)" -eq 0 ]; then
  PS1='\w # '
else
  PS1='\w $ '
fi

# make sure most shell sessions gets more complete PATH and enable completions
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

# if [ ! -z "${ANDROID_NDK_HOME}" ]; then
# 	 if [ -d "${ANDROID_NDK_HOME}" ]; then
# 		 LATEST_NDK_DIR=$(ls -td ${ANDROID_NDK_HOME}/*/ | head -1)
# 		 if ! [ -z "${LATEST_NDK_DIR}" ] && ! [ "${LATEST_NDK_DIR}" = "/tmp" ]; then
# 			 PATH="$PATH:${LATEST_NDK_DIR}"
# 		 fi
# 	 fi
# fi

if [ -f "$HOME/.sdkman/bin/sdkman-init.sh" ]; then
    . $HOME/.sdkman/bin/sdkman-init.sh
fi

export GPG_TTY="$( tty )"

# case "$TERM" in
#     xterm-color|*-256color) color_prompt=yes;;
# esac

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

if command -v kubectl &> /dev/null; then
    source <(kubectl completion bash)
fi

if command -v kind &> /dev/null; then
    source <(kind completion bash)
fi

if command -v minikube &> /dev/null; then
    source <(minikube completion bash)
fi

if [ -f "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
fi

if command -v aws_completer &> /dev/null; then
    complete -C 'aws_completer' aws
fi

for bcfile in ~/.bash_completion.d/* ; do
  [ -f "$bcfile" ] && . $bcfile
done

if [ -x $HOME/.local/bin/bashmarks.sh ]; then
    . $HOME/.local/bin/bashmarks.sh
fi

if [ -f /usr/share/virtualenvwrapper/virtualenvwrapper_lazy.sh ]; then
    . /usr/share/virtualenvwrapper/virtualenvwrapper_lazy.sh
fi

export PROMPT_COMMAND=append_prompt

ORIG_PS1=$PS1

append_prompt() {
  # Capture exit code of last command
  local ex=$?

  # Set original prompt content
  PS1="$ORIG_PS1"
  # If exit code of last command is non-zero, prepend this code to the prompt
  [[ "$ex" -ne 0 ]] && PS1="$ex|$PS1"
}

case "$TERM" in
xterm*|rxvt*)
    # PROMPT_COMMAND='echo -ne "\033]0;${PWD}\007"'

    # # Show the currently running command in the terminal title:
    # # http://www.davidpashley.com/articles/xterm-titles-with-bash.html
    # show_command_in_title_bar()
    # {
    #     case "$BASH_COMMAND" in
    #         "*\\033]0*"")
    #             # The command is trying to set the title bar as well;
    #             # this is most likely the execution of $PROMPT_COMMAND.
    #             # In any case nested escapes confuse the terminal, so don't
    #             # output them.
    #             ;;
    #         *)
    #             echo -ne "\033]0;${BASH_COMMAND}\007"
    #             ;;
    #     esac
    # }
    # trap show_command_in_title_bar DEBUG
    ;;
dumb)
	# probably Emacs shell mode
	# disable pagers as it confuses the emacs "terminal"
	# emacs M-x shell
    alias less='cat'
    alias more='cat'
    export PAGER=cat
	export EDITOR=emacsclient
	;;
*)
    ;;
esac


if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi

  if [ -d ~/.bash_completion.d ]; then
      for bcfile in ~/.bash_completion.d/* ; do
          . $bcfile
      done
  fi
fi

decode_base64_url() {
  local len=$((${#1} % 4))
  local result="$1"
  if [ $len -eq 2 ]; then result="$1"'=='
  elif [ $len -eq 3 ]; then result="$1"'='
  fi
  echo "$result" | tr '_-' '/+' | openssl enc -d -base64
}

decode_jwt(){
   decode_base64_url $(echo -n $2 | cut -d "." -f $1) | jq .
}

# Decode JWT header
alias jwth="decode_jwt 1"

# Decode JWT Payload
alias jwtp="decode_jwt 2"

alias cpma="quake3 +nosplash +set fs_game cpma"

alias ssh-add-pkcs11="ssh-add -s /usr/lib/x86_64-linux-gnu/pkcs11/opensc-pkcs11.so"
alias ssh-add-pkcs11-del="ssh-add -e /usr/lib/x86_64-linux-gnu/pkcs11/opensc-pkcs11.so"

# kitty terminfo is usually not installed remotely
alias ssh='TERM=xterm-color ssh'

if [ -f ${HOME}/.bash_aliases ]; then
    . ${HOME}/.bash_aliases
fi
