#!/bin/sh
##########################################################
## Look through ~/.xmonad/xmonad.hs for the given key submap
## in EZ-Config format.
## Parse each line for the keys, commands and trailing comments.
## Use comments for the description when available.
## format into columns to fit the screen width.
## send to dzen2 along with font colors and font.
##########################################################
KEYMAP=$1
# make FW bigger if the columns don't fit on your screen
FW=350
LH=24
X=$2
W=$4
KEYCOLOR=$6
CMDCOLOR=$7
FONT=$8
COLS=$(($W / $FW))

INFO=$(awk -v cmdcolor=$CMDCOLOR -v keycolor=$KEYCOLOR -v cols=$COLS \
           '/^'"$KEYMAP"'/,/^\s*\].*$/ {
                if ($0 ~ /^ *--+/) next

                # get the key entry and any following comment.
                split($0, splitline, " --", seps)
                comma_loc=index(splitline[1], "\",")
                match(substr(splitline[1], 1, comma_loc-1), /^.*\"(.*)/, keys)
                match(substr(splitline[1], comma_loc+2), /^ *(.*)\)/, command)

                # remove any leading spaces from the comment
                gsub(/^[ \t]/, "", splitline[2])

                # skip any empty records.
                if (length(command[1]) > 0){
                    # if there is a comment use that for the description.
                    if (length(splitline[2]) > 0) {
                            desc=splitline[2]
                        } else {
                            desc=command[1]
                    }
                    key_hint[i++] = sprintf (" ^fg(%s)%14.14s ^fg(%s)%-30.30s", keycolor, keys[1], cmdcolor, desc)
                }
            }
            END {
                rows = int( ((i+1) / cols) +1)
                for (j=0; j<=i;) {
                    for (k=0; k < rows; k++) {
                         row[k] = row[k] key_hint[j++]
                    }
                }
                for (k=0; k <= rows; k++) {print row[k]}
            }' \
           ~/.xmonad/xmonad.hs)

## echo "30\n22\n20\n15\n3\n" | awk '{print $1 " : " int( (($1+1) / 10) + 1)}'
echo "$INFO"
N_LINES=$(wc -l <<< "$INFO")
Y=$(($3 + $5 - ($LH * ($N_LINES+3))))
sleep 1
# $KEYMAP ($2 , $3 , $4 , $5, $LH, $X, $Y, $W, $N_LINES, $COLS
(echo "^fg($KEYCOLOR)$KEYMAP"
 echo ""
 echo "$INFO"
 echo ""
 cat) | dzen2 -l $(($N_LINES+2)) -fn "${FONT}" -h $LH -x $X -y $Y -w $W -e onstart=uncollapse
