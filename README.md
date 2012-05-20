Emacs-Kit
---
Emacs is an awesome editor, but the process of figuring out one useful Emacs configuration is long and difficult. This kit is about to ease the procedure, I try to make it more configurable, more efficient, and more readable.

I don't use any scripts which I don't know what for, so, this kit doesn't have any 'garbage' script in it. But in case that some functionalities that might be not needed, most of the packages are lazy loaded.

Also, I adopt emacs packaging mechanism, all packages are downloaded and installed during the first start of Emacs from [MELPA](http://melpa.milkbox.net/).

Other packages are using `git submodule` which fetches packages from [github](http://github.com), please use `git submodule init` and `git submodule update` after you clone it.

Please use Emacs 24 and put a soft link in $USER to `.emacs` as well as changing `emacs-d` in `init.el`.
