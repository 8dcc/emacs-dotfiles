# emacs dotfiles
**My configuration files for doom emacs**

### Requirements
#### Doom emacs
Install the following packages:
```bash
emerge -a '>=app-editors/emacs-27.0' '>=dev-vcs/git-2.23' '>=sys-apps/ripgrep-11.0' sys-apps/findutils '>=sys-apps/fd-7.3.0'
```

And finally, doom emacs.
```bash
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
```

#### Misc
My dotfiles require some packages.
```bash
# Fonts
emerge -a media-fonts/dina media-fonts/fira-sans media-fonts/fira-code

# For emms (media player)
emerge -a media-video/mpv
```

You will also need `youtube-dl` from [here](https://github.com/yt-dlp/yt-dlp/releases/tag/2023.03.04)
and move `yt-dlp` to `/usr/local/bin/youtube-dl`.

### Installation
#### `doom/`
Move the contents of this folder to `~/.config/doom/` (or `~/.doom.d/`)

#### `emacs/`
Move the contents of this folder to `~/.config/emacs/` (`~/.emacs.d/`)
