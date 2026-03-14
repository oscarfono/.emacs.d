#!/bin/bash
# emacs-server-on-startup.sh
#
# Starts the Emacs daemon if it is not already running.
# Intended for use as a login item or via launchd (macOS) / systemd (Linux).
#
# The preferred approach on Linux is a systemd user service — see README.
# This script is kept for macOS compatibility and manual invocation.

if ! emacsclient --eval "(server-running-p)" > /dev/null 2>&1; then
    emacs --daemon
fi
