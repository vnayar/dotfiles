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

##
# Oracle Environment
##

# 32-bit install
export ORACLE_HOME="/usr/lib/oracle/10.2.0.3/client"
# 64-bit install
# export ORACLE_HOME=/usr/lib/oracle/10.2.0.3/client64

export PATH="$ORACLE_HOME/bin:$PATH"
export LD_LIBRARY_PATH="$ORACLE_HOME/lib:$LD_LIBRARY_PATH"
export TNS_ADMIN="$HOME/oracle"

##
# Web Assessment Environment
##

export ASSESS_HOME=$HOME/wgen/web-assessment

##
# Android SDK Environment
##

export ANDROID_SDK="$HOME/share/android-sdk-linux_x86"
export PATH="$ANDROID_SDK/tools:$PATH"

# My Own Settings
export EDITOR=/usr/bin/vim

alias pretty_json='python -mjson.tool'
alias ack='ack-grep'

##
# BCFG2 Parameters
##
export PYTHON_24_BIN="${HOME}/wgen/bcfg2Env/bin/python"
export BCFG2_INFO_BIN="${HOME}/wgen/bcfg2Env/bin/bcfg2-info"
export BCFG2_REPO_ROOT="${HOME}/wgen/bcfg2/central/bcfg2/"

##
# Ruby Options
##
export RUBYOPT="-rubygems"

##
# Skylight Options
##

export SKYLIGHT_HOME="$HOME/wgen/skylight"
export APACHE_CONFIG_DIR="${SKYLIGHT_HOME}/conf/base/config"
export APACHE_MODULE_DIR="/usr/lib/apache2/modules"

alias invoke-apache='/usr/sbin/apache2ctl -f $SKYLIGHT_HOME/conf/dev-local-lb-httpd-skylight.conf -k'

# Java Options
export JAVA_HOME="/usr/lib/jvm/default-java/jre"
export CLASSPATH="$HOME/java/share:$JAVA_HOME"

# Tomcat Server Options
export CATALINA_OPTS="-Dskylight.env.config.dir=$SKYLIGHT_HOME/conf -Dwgspring.infrastructure.wsclient.enrollment.dataSource='Live'"

# Custom WGEN Ant Options
export ANT_HOME="$HOME/wgen/ant.git/ant-1.8.1"
