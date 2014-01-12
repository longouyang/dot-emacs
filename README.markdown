# directory structure

this repo should live in `~/dot-emacs`, which `~/.emacs.d` should symlink to

within this repo, `init.el` should symlink to `emacs.el`, which is the tangled version of `emacs.org`

the `elpa/` directory is for ELPA packages (i.e., the directory for `M-x list-packages`)

the `vendor/` directory is for bigger packages (usually git submodules) and one-off files i use

# stuff from old readme

org-mode is installed from macports
ESS is installed from website

solarized theme is installed from my fork of https://github.com/sellout/emacs-color-theme-solarized
- i added a few lines of my own customization to solarized-definitions.el to make org-mode eaasier to read


