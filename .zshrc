autoload -Uz compinit promptinit
compinit
promptinit

# This will set the default prompt to the walters theme
PROMPT="%n@%m:%2d$ "

export PATH=/Users/hth/.cargo/bin:/Users/hth/.ghcup/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/texlive/2023/bin/universal-darwin/

export HISTSIZE=100
export SAVEHIST=1
export HISTFILE=~/.zsh-history

alias config='git --git-dir=$HOME/.cfg --work-tree=$HOME'

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

[ -f "/Users/hth/.ghcup/env" ] && source "/Users/hth/.ghcup/env" # ghcup-env
