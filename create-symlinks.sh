#!/bin/sh
set -e

# Create symlinks in the Emacs directory that point to the 'emacs' directory in
# the current repository.

EMACSDIR="${HOME}/.emacs.d"
REPODIR="$(pwd)/emacs"

symlink_item() {
    ln -s "${REPODIR}/$1" "${EMACSDIR}/$1"
}

read -p "Symlinking from '$REPODIR' to '$EMACSDIR'. Press any key..."
set -x

# Directories
symlink_item "gdb-layouts"
symlink_item "my-media"
symlink_item "themes"

# Files
symlink_item "eshell/alias"
symlink_item "emms/streams.emms"
symlink_item "straight/versions/default.el"
symlink_item "config.el"
symlink_item "config.org"
symlink_item "custom.el"
symlink_item "early-init.el"
symlink_item "init.el"
