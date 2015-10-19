
# Sets the pointre to be more usualy
 
# keymap rebindings, here it sets caps lock to be escape

# uses feh to set my background image
#sh ~/.fehbg &
/home/skye/dotfiles/bin/keyBoardOptions.sh
# Start up programs

# Personal script files
 
#Xresources

xrdb -merge ~/.Xresources

xset -dpms
xset s off

envfile="$HOME/.gnupg/gpg-agent.env"
if [[ -e "$envfile" ]] && kill -0 $(grep GPG_AGENT_INFO "$envfile" | cut -d: -f 2) 2>/dev/null; then
    eval "$(cat "$envfile")"
else
    eval "$(gpg-agent --daemon --enable-ssh-support --write-env-file "$envfile")"
fi
export GPG_AGENT_INFO  # the env file does not contain the export statement
export SSH_AUTH_SOCK   # enable gpg-agent for ssh

# run the window manager
xmonad
