
DOOM_DIR=~/.config/doom/

.PHONY: update

# Update the real doom files with the ones in the repo. Used for consistency.
update:
	cp -r doom/* $(DOOM_DIR)
