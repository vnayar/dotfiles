# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

set -o emacs

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

PATH="/usr/sbin:$PATH"

# Java Options
export JAVA_HOME="/usr/lib/jvm/default-java/jre"
export CLASSPATH="$HOME/java/share:$JAVA_HOME"

# Custom WGEN Ant Options
export ANT_HOME="$HOME/wgen/ant.git/ant-1.8.1"

# GTAGS
export GTAGSLABEL=pygments

export EDITOR='emacs'
