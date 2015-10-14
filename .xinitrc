
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


# run the window manager
xmonad
