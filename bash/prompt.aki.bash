#!/bin/bash

# ╒╡ \$USER@\$HOSTNAME ╞═
PROMPT_1L="<15>╒<22><_001>╡ <M>\$USER<c>@<G>\$HOSTNAME<22> ╞<_k><15>═<11>"
PROMPT_1LD="<15>╒<22>╡ <m>\$USER<c>@<g>\$HOSTNAME<22> ╞<15>═<11>"
PROMPT_2L="<15>└<22>┶<18>━<16>━<12>╾<8>╼ <x>"
PROMPT_1R="<15>═<22><_001>╡ <M>\$PPWD <22>╞<_k><15>═<22><_001>╡ <C>\$DATE <22>╞<_k><15>╸"
PROMPT_1RD="<15>═<22>╡ <m>\$PPWD <22>╞<_k><15>═<22>╡ <c>\$DATE <22>╞<_k><15>╕"
PROMPT_2R="╾╼━━┵┘"
FILLSRC="═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════"

# Fix tput for screen
TPUT="tput"
if [ $TERM == 'screen' ]; then
	TPUT="tput -Trxvt"
fi

# Function to set tab title
function set_title()
{
	echo -ne "\e]0;" > /dev/stderr
	#[[ $USER == root ]] && echo -nE "su -- " > /dev/stderr
	echo -nE  "<< ${BASH_COMMAND} >> ${USER}@${HOSTNAME}: ${PWD/$HOME/~}" > /dev/stderr
	echo -ne "\a" > /dev/stderr
}


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
	local L=$(eval echo $(colorize strip "$PROMPT_1L"))
	local R=$(eval echo $(colorize strip "$PROMPT_1R"))


	STATUS=`echo "$STATUS" | ~/color`

	let fillsize=${COLUMNS}-${#L}-${#R}
	if [ "$fillsize" -gt "0" ]
	then
		fill="${FILLSRC:0:${fillsize}}"
	fi
}

function precmd() {
	prompt_command
}

function preexec () {
	# Save cursor position
	$TPUT sc

	# Determine how many lines down the command input put us
	local x=$@
	local lines=$((2 + ${#x}/${COLUMNS}))
	$TPUT cuu $lines

	DATE=$(date +'%H:%M:%S')

	# Fade out old prompt
	$TPUT hpa 0
	echo -n $(colorize $(eval echo "\"$PROMPT_1LD\""))
	local RD=$(colorize strip $(eval echo "\"$PROMPT_1RD\""))
	local c=$(($COLUMNS-${#RD}))
	$TPUT hpa $c
	echo -n $(colorize $(eval echo "\"$PROMPT_1RD\""))
	$TPUT cud1

	# If command was less than one line, fill in space to the right
	if [[ ${#x} -lt ${COLUMNS} ]]
	then
		#local fillsize=$((${COLUMNS}-${#x}-${#PROMPT_2R}-2))
		#if [ "$fillsize" -gt "0" ]
		#then
		#	fill="${FILLSRC:0:${fillsize}}"
		#fi
		local c=$(($COLUMNS-${#PROMPT_2R}))
		$TPUT hpa $c
		echo -n $PROMPT_2R
	fi

	# Restore cursor
	$TPUT rc

  set_title

	# Re-output cmd invisibly for copy+paste ease
	PPWD=${PWD/$HOME/\~}
	echo -en $(clr 1)
	echo "($(date +'%Y-%m-%d %H:%M:%S')) $PPWD% $@"

	# Reset color
	echo -ne $CLR


}

export PS1="\[\n$(colorize "$PROMPT_1L")\${fill}$(colorize "$PROMPT_1R")\]\n$(colorize strip "$PROMPT_2L")"
preexec_install
