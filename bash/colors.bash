#!/bin/bash
#
# COLORS #
ESC=`echo -e '\033'`
CLR="\033[0m"    # unsets color to term's fg color

# regular colors
FGK="\033[22;30m"    # black
FGR="\033[22;31m"    # red
FGG="\033[22;32m"    # green
FGY="\033[22;33m"    # yellow
FGB="\033[22;34m"    # blue
FGM="\033[22;35m"    # magenta
FGC="\033[22;36m"    # cyan
FGW="\033[22;37m"    # white

# emphasized (bolded) colors
EMK="\033[1;30m"
EMR="\033[1;31m"
EMG="\033[1;32m"
EMY="\033[1;33m"
EMB="\033[1;34m"
EMM="\033[1;35m"
EMC="\033[1;36m"
EMW="\033[1;37m"

# background colors
BGK="\033[40m"
BGR="\033[41m"
BGG="\033[42m"
BGY="\033[43m"
BGB="\033[44m"
BGM="\033[45m"
BGC="\033[46m"
BGW="\033[47m"

# 256 color support
# clr
#   reset to default color
# clr w
#   set foreground to grayscale color (0-23)
# clr 'bg' w
#   set background to grayscale color
# clr r g b
#   set foreground to specified color (r g b are 0-5)
# clr 'bg' r g b
#   set background to specified color
clr() {
	local bkg=$1
	local type=38
	if [ x$bkg == x ]; then
		echo -ne $CLR
	elif [ $bkg == bg ]; then
		type=48
		shift
	fi

	local r=$1 g=$2 b=$3
	local t=-1
	if [ x$g != x ]; then
		t=$(($r+$g+$b))
	fi

	if [ $t == 0 ]; then
		clr=0
	elif [ $t == 15 ]; then
		clr=15
	elif [ $t == -1 ]; then
		# 232+$r doesnt work??
		clr=$(($r+232))
	else
		clr=$((16+($r*36)+($g*6)+$b))
	fi

	echo -ne "\x1b[${type};5;${clr}m"
}

# colorize a string using special color codes:
#   <x> - reset colors
#   <$> - set foreground. lowercase is normal, upper is em(bold).
#   <#>, <##> - set foreground to grayscale (0-23)
#   <###> - set foreground to rgb (0-5 each)
#   <_...> - set background instead
#   ex: <R>bright red fg<_w>white bg<16>gray fg<045>light blue fg<x>reset
# colorize str
#   echo colorized string
# colorize prompt str
#   echo colorized string with colors surrounded in \[ \]
# colorize strip str
#   strip color codes from string
colorize() {
	local mode=normal

	if [ "$1" == strip ]; then
		mode=strip
		shift
	elif [ "$1" == prompt ]; then
		mode=prompt
		shift
	fi

	local result=$*
	local fo="<" fc=">" bo="<_" bc=">"
	local PRE=$(echo -ne '\033[')
	local BOLD='1;' NRM='22;' FG='3' BG='4' EXT='8;5;' END='m' RST='0'

	if [ $mode == 'strip' ]; then
		unset PRE BOLD NRM FG BG EXT END RST
	elif [ $mode == 'prompt' ]; then
		PRE="\\[$PRE"
		END="$END\\]"
	fi

	local i=0
	for l in k r g y b m c w; do
		if [ $mode == 'strip' ]; then unset i; fi
		result=${result//$fo${l^}$fc/$PRE$BOLD$FG$i$END}
		result=${result//$fo$l$fc/$PRE$NRM$FG$i$END}
		result=${result//$bo$l$bc/$PRE$BG$i$END}
		i=$(($i+1))
	done
	
	unset n

	for r in 0 1 2 3 4 5; do
		for g in 0 1 2 3 4 5; do
			for b in 0 1 2 3 4 5; do
				if [ $mode != 'strip' ]; then
					n=$((16+($r*36)+($g*6)+$b))
				fi
				result=${result//$fo$r$g$b$fc/$PRE$FG$EXT$n$END}
				result=${result//$bo$r$g$b$bc/$PRE$BG$EXT$n$END}
			done
		done
	done
	
	for w in {0..23}; do
		if [ $mode != 'strip' ]; then 
			n=$(($w+232))
		fi
		result=${result//$fo$w$fc/$PRE$FG$EXT$n$END}
		result=${result//$bo$w$bc/$PRE$BG$EXT$n$END}
	done

	result=${result//${fo}x${fc}/$PRE$RST$END}

	echo -n "$result"
}
