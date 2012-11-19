Emacs-Kit
---
Emacs is the best editor, but figuring out one workable Emacs configuration takes time, and often mess up everything.

This kit is aimed to make it easy to configure, also, more efficient, and more readable.

I didn't use any script which I don't know what for, so, this kit doesn't contain any 'garbage' script. But in case that some functions might not be needed, most of the packages are [Lazy loading](http://en.wikipedia.org/wiki/Lazy_loading).

Also, I adopted the Emacs packaging mechanism, all packages will be downloaded and installed during the first start of Emacs from [MELPA](http://melpa.milkbox.net/).

There are still some packages missing in MELPA are fetched from [github](http://github.com) using `git submodule`, please use `git submodule init` and `git submodule update` after you clone it.

Please use Emacs 24 and put a soft link of `.emacs` in `$USER` home directory, and change location of `emacs-d` in `init.el`.
