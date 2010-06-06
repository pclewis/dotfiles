#!/bin/bash

hostname=`hostname`
hostname=${hostname%.*}

uname=`uname`
case $uname in
	Darwin)
		os=macos
		os_distro=x
		os_version=`sw_vers -productVersion`
		;;
	Linux)
		os=linux
		os_distro=`echo /etc/*[-_]{release,version}`
		os_distro=${os_distro#/etc/}
		os_distro=${os_distro%[-_]*}
		os_distro=${os_distro/lsb/ubuntu}
		os_version=`uname -r`
		;;
	FreeBSD)
		os=bsd
		os_distro=freebsd
		os_version=`uname -r`
		;;
esac
