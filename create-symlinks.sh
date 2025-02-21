#!/usr/bin/env bash
set -e

# Create symlinks in the Emacs directory that point to the 'emacs' directory in
# the current repository.

EMACSDIR="${HOME}/.emacs.d"
REPODIR="$(pwd)/emacs"

symlink_item() {
    target_dir="${EMACSDIR}/$(dirname "$1")"

    # First, create the destination directory if it doesn't exist.
    mkdir --parents "$target_dir"

    # It's important to use the '--target-directory' option: when trying to
    # symlink to a directory "foo": if it already exists, a "foo/foo" symlink
    # is created instead, even with '--force'.
    ln --symbolic --force "${REPODIR}/$1" --target-directory="$target_dir"
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
