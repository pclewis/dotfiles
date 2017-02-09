#!/bin/sh
echo -e "SETPROMPT sudo password\nGETPIN" | pinentry-gtk-2 | grep "^D" | tail -c +3
