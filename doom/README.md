# Doom
**My configuration files for doom emacs**

## Requirements
### Doom emacs
Install the following packages:
```bash
emerge -a '>=app-editors/emacs-27.0' '>=dev-vcs/git-2.23' '>=sys-apps/ripgrep-11.0' sys-apps/findutils '>=sys-apps/fd-7.3.0'
```

And finally, doom emacs.
```bash
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
```

### Misc
My dotfiles require some packages.
```bash
# Fonts
emerge -a media-fonts/dina media-fonts/fira-sans media-fonts/fira-code

# For emms (media player)
emerge -a media-video/mpv

# For spelling. Optional dicts with aspell-*
emerge -a app-text/aspell app-dicts/aspell-en

# For sly (common-lisp)
emerge -a dev-lisp/sbcl
```

You will also need `youtube-dl` from [here](https://github.com/yt-dlp/yt-dlp/releases/tag/2023.03.04)
and move `yt-dlp` to `/usr/local/bin/youtube-dl`.

### Downgrading doom
If you are scared of change like me, and you want to use an older version of
doom emacs, you can follow these steps:
```bash
# Your doom emacs install, where bin/doom is
cd ~/.config/emacs/

# You might need to do this if it's an older commit. 1672534861 is 1/1/2023
git fetch --shallow-since=1672534861

# Where COMMIT-HASH is the commit you want from https://github.com/doomemacs/doomemacs/commits/master
# If you have another installation with the version you want, you can do
# `git log` to get the current commit hash.
git checkout COMMIT-HASH

# Re-build doom emacs
~/.config/emacs/bin/doom clean && ~/.config/emacs/bin/doom build && ~/.config/emacs/bin/doom sync -u
```

The current commit I use is `4e105a95af9c4c7e86471e5566eb7a5ff776ec92`.

## Installation
Move the contents of this folder to `~/.config/doom/` (or `~/.doom.d/`)

