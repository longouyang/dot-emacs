# directory structure

this repo should live in `~/dot-emacs`, which `~/.emacs.d` should symlink to

within this repo, `init.el` should symlink to `emacs.el`, which is the tangled version of `emacs.org`

the `elpa/` directory is for ELPA packages (i.e., the directory for `M-x list-packages`)

the `vendor/` directory is for bigger packages (usually git submodules) and one-off files i use

# git

the repository on corn is "bare", which makes the solarized submodule a pain to work with. to get that working to begin with, i temporarily set "bare = false" in .git/config on corn, then ran "git submodule init" and "git submodule update" to pull in solarized, and then switched back to "bare = true".
