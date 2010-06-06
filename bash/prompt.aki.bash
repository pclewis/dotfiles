#!/bin/bash

BTL="╭"
BAR="─"
BCL="┤"
BCR="├"
BBL="╰"
BBR="╯"

USERCOLOR=$EMC
HOSTCOLOR=$EMC

PROMPT_1L="^K^╭──_b_┤^C^\$USER@\${HOSTNAME}^K^├_k_─\$STATUS"
PROMPT_2L="^K^╰──┴──►^G^"
PROMPT_1LD="^K^╭──┤^c^_k_\$USER@\${HOSTNAME}_k_^K^├─"
PROMPT_1R="^K^_b_┤^M^\${PPWD}^K^├_k_───_b_┤^C^\${DATE}^K^├_k_╼"
PROMPT_1RD="^K^┤^m^\$PPWD^K^├───┤^K^\$DATE^K^├╼╮"
PROMPT_2LS=`echo "$PROMPT_2L" | ~/color strip`

# Function to set tab title
function set_title()
{
	echo -ne "\e]0;" > /dev/stderr
	#[[ $USER == root ]] && echo -nE "su -- " > /dev/stderr
	echo -nE  "<< ${BASH_COMMAND} >> ${USER}@${HOSTNAME}: ${PWD/$HOME/~}" > /dev/stderr
	echo -ne "\a" > /dev/stderr
}

FILLSRC="───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────"

function prompt_command()
{
	if [ $? != 0 ]
	then
		STATUS="┤^R^╳^K^├"
	else
		STATUS=""
	fi

	case "$TERM" in
		xterm*|rxvt*) echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007" ;;
	esac

	PPWD=${PWD/$HOME/\~}

	DATE=$(date +'%H:%M:%S')
	#   Add all the accessories below ...
	local LL=`eval echo "$PROMPT_1L"`
	local L=`echo $LL | ~/color strip`
	local RR=`eval echo "$PROMPT_1R"`
	local R=`echo $RR | ~/color strip`

	STATUS=`echo "$STATUS" | ~/color`

	let fillsize=${COLUMNS}-${#L}-${#R}-1
	if [ "$fillsize" -gt "0" ]
	then
		fill="${FILLSRC:0:${fillsize}}"
	fi
}

function precmd() {
	prompt_command
}

function preexec () {
	tput sc

	local x=$@
	local lines=$((2 + ${#x}/${COLUMNS}))
	tput cuu $lines
	DATE=$(date +'%H:%M:%S')
	tput hpa 0
	local LLD=`eval echo "$PROMPT_1LD"`
	echo -n $LLD | ~/color
	local RRD=`eval echo "$PROMPT_1RD"`
	local RD=`echo $RRD | ~/color strip`
	local c=$(($COLUMNS-${#RD}))
	tput hpa $c
	echo -n $RRD | ~/color
	tput cud1

	if [[ ${#x} -lt ${COLUMNS} ]]
	then
		local fillsize=$((${COLUMNS}-${#x}-${#PROMPT_2LS}-2))
		if [ "$fillsize" -gt "0" ]
		then
			fill="${FILLSRC:0:${fillsize}}"
		fi
		local c=$(($COLUMNS-$fillsize-2))
		tput hpa $c
		#echo -n "$c-${COLUMNS}-${x}-${#PROMPT_2LS}"
		echo -n "◄$fill╯"
	fi
	tput rc
	echo -n -e $CLR
  set_title
}

export PS1="\[$(echo $PROMPT_1L | ~/color)\${fill}$(echo $PROMPT_1R | ~/color)\]\n$(echo $PROMPT_2L | ~/color bash)"
preexec_install
