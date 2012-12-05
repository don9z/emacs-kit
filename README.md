Emacs-Kit
---
Emacs is the best editor, but figuring out one workable Emacs configuration takes time, and often messes up everything.

This kit is aimed to make it easy to configure, also, more efficient, and more readable. It relies on some packages, in case that some functions might not be needed, most of the packages are [Lazy loaded](http://en.wikipedia.org/wiki/Lazy_loading).

Also, Emacs packaging mechanism is adopted in this kit, all packages are downloaded and installed from [MELPA](http://melpa.milkbox.net/) during the first launch of Emacs.

There are still some packages missing in MELPA are stored in the `extensions` folder, some are fetched from [github](http://github.com), hope it will be removed soon. Please run `git submodule init` and `git submodule update` after you've cloned it.

Please use Emacs 24, and load `init.el` from your `.emacs` file.
