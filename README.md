Emacs-Kit
---

Emacs is the best text editor, but it takes time to configure it.

This kit aims to make it easy, with efficient and readable scripts.

It depends on some packages, but in case that some functions might not be needed, most of the packages are [lazy loaded](http://en.wikipedia.org/wiki/Lazy_loading).

Also, Emacs packaging mechanism is adopted in this kit, all packages are downloaded and installed from [MELPA](http://melpa.milkbox.net/) during the first launch of Emacs.

There are still some packages missing in MELPA are stored in the `extensions` folder, some are fetched from [github](http://github.com), hope it will be removed soon.

Please run `git submodule init` and `git submodule update` after you've cloned it.

Please use Emacs 24+, and load `init.el` in your `.emacs` file.


macOS native-compilation compatibility
---

Keep the existing `.emacs` entry that loads the kit's `init.el`.
It now loads the shared `early-init.el` before the kit's other modules.
This does not create or modify any user startup files, change warning display,
or disable native compilation.

The workaround is limited to the reported combination of macOS 27 and
libgccjit 14.3.0, with native compilation available. The bundled compiler can
incorrectly infer a macOS 18.0 deployment target, which clang rejects. The kit
adds `-mmacosx-version-min=27.0` to the native compiler driver options for this
combination. Other OS/compiler combinations are left unchanged. Existing
`MACOSX_DEPLOYMENT_TARGET`, driver-option or compiler-option target settings
are respected. To opt out, set `emacs-kit-macos-native-comp-workaround` to nil
before loading the kit.

Upstream report: [jimeh/emacs-builds#71](https://github.com/jimeh/emacs-builds/issues/71).
This is a targeted workaround, not a claim that all macOS or Emacs builds fail.
The condition should be revisited when the bundled compiler is fixed.

Automatic package activation happens before the regular `.emacs` file. If the
error occurs during that earlier phase, run `M-x emacs-kit-install-early-init`
once, then restart Emacs. This optional command creates a local loader only
when no early-init file or symlink exists; it never overwrites an existing one.
If you already have custom early settings, load this checkout's `early-init.el`
from them instead. The checkout path is computed at runtime and is not stored
in the public repository. If the checkout moves, update the local loader.

Run the compatibility checks without loading your personal configuration:

```sh
emacs --batch -Q -l tests/startup-tests.el -f ert-run-tests-batch-and-exit
```
