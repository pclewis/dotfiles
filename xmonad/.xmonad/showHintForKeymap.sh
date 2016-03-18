#!/bin/sh
KEYMAP=$1
FW=7
LH=14
X=$2
W=$4
COLS=$(($W / $FW))
INFO=$(cat ~/.xmonad/xmonad.hs \
     | awk '/^'"$KEYMAP"'/,/^\s*\]$/' \
     | head -n -1 \
     | sed -r 's/^.*?[,\[] \("([^"]+)",(\s*)(.*)\)/^fg(purple)\1\2^fg(white)\3/' \
     | column -c $COLS \
     | expand \
     | sed 's/^/    /' )
echo "$INFO"
N_LINES=$(wc -l <<< "$INFO")
Y=$(($3 + $5 - ($LH * ($N_LINES+2))))
(echo "^fg(purple)$KEYMAP ($2 , $3 , $4 , $5, $LH, $X, $Y, $W, $N_LINES, $COLS )"
 echo "$INFO"
 echo ""
 sleep 1
 echo '^unhide()'
 echo '^uncollapse()'
 cat) | dzen2 -l $(($N_LINES+1)) -h $LH -x $X -y $Y -w $W -e "onstart=hide"
