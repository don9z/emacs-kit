Emacs-Kit
---
Emacs is an awesome editor, but the process of figuring out one useful Emacs configuration is long and difficult. This kit is about to ease the procedure, I try to make it more configurable, more efficient, and more readable.

I don't use any scripts which I don't know what for, so, this kit doesn't have any 'garbage' script in it. But in case that some functionalities that might be not needed, only few packages are loaded at emacs start time, most of them are all lazy loaded.

Also, I adopt emacs packaging mechanism, all packages available in `elpa` which is needed by this kit is downloaded and installed during the first start of Emacs.

Other packages are using `git submodule` which benefit from github, please use `git submodule init` and `git submodule update` when you clone it.

Please use Emacs 24.
