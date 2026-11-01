;;; startup-tests.el --- Shared startup regression tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'cl-lib)
(require 'comp nil t)
(setq native-comp-enable-subr-trampolines nil)
(setq native-comp-jit-compilation nil)
(defvar emacs-kit-macos-native-comp-workaround)
(defconst emacs-kit-test--source
  (expand-file-name "../early-init.el" (file-name-directory load-file-name)))
(let ((emacs-kit-macos-native-comp-workaround nil))
  (load emacs-kit-test--source nil t))

(defmacro emacs-kit-test--platform (system os gcc native &rest body)
  "Run BODY with isolated compiler settings and simulated platform versions."
  (declare (indent 4))
  `(progn
     (skip-unless (featurep 'comp))
     (let ((system-type ,system)
           (native-comp-driver-options '("-Wl,-w"))
           (native-comp-compiler-options nil)
           (process-environment (copy-sequence process-environment))
           (emacs-kit-macos-native-comp-workaround t)
           (os-calls 0))
       (setenv "MACOSX_DEPLOYMENT_TARGET" nil)
       (cl-letf (((symbol-function 'native-comp-available-p) (lambda () ,native))
                 ((symbol-function 'comp-libgccjit-version) (lambda () ,gcc))
                 ((symbol-function 'call-process)
                  (lambda (&rest _args)
                    (setq os-calls (1+ os-calls))
                    (insert ,os)
                    0)))
         ,@body))))

(ert-deftest emacs-kit-targeted-workaround ()
  (emacs-kit-test--platform 'darwin "27.2\n" '(14 3 0) t
    (should (emacs-kit-configure-native-comp))
    (should (equal native-comp-driver-options
                   '("-mmacosx-version-min=27.0" "-Wl,-w")))))

(ert-deftest emacs-kit-keeps-explicit-driver-target ()
  (emacs-kit-test--platform 'darwin "27.2\n" '(14 3 0) t
    (setq native-comp-driver-options '("-Wl,-w" "-mmacosx-version-min=27.2"))
    (should-not (emacs-kit-configure-native-comp))
    (should (equal native-comp-driver-options
                   '("-Wl,-w" "-mmacosx-version-min=27.2")))))

(ert-deftest emacs-kit-keeps-explicit-compiler-target ()
  (emacs-kit-test--platform 'darwin "27.2\n" '(14 3 0) t
    (setq native-comp-compiler-options '("-mmacosx-version-min=26.0"))
    (should-not (emacs-kit-configure-native-comp))
    (should (equal native-comp-driver-options '("-Wl,-w")))))

(ert-deftest emacs-kit-keeps-environment-target ()
  (emacs-kit-test--platform 'darwin "27.2\n" '(14 3 0) t
    (setenv "MACOSX_DEPLOYMENT_TARGET" "27.0")
    (should-not (emacs-kit-configure-native-comp))
    (should (equal (getenv "MACOSX_DEPLOYMENT_TARGET") "27.0"))
    (should (equal native-comp-driver-options '("-Wl,-w")))))

(ert-deftest emacs-kit-opt-out ()
  (emacs-kit-test--platform 'darwin "27.2\n" '(14 3 0) t
    (setq emacs-kit-macos-native-comp-workaround nil)
    (should-not (emacs-kit-configure-native-comp))
    (should (= os-calls 0))))

(ert-deftest emacs-kit-other-systems-unchanged ()
  (dolist (system '(gnu/linux windows-nt))
    (emacs-kit-test--platform system "27.2\n" '(14 3 0) t
      (should-not (emacs-kit-configure-native-comp))
      (should (= os-calls 0))
      (should (equal native-comp-driver-options '("-Wl,-w"))))))

(ert-deftest emacs-kit-other-os-versions-unchanged ()
  (dolist (os '("26.5\n" "28.0\n"))
    (emacs-kit-test--platform 'darwin os '(14 3 0) t
      (should-not (emacs-kit-configure-native-comp))
      (should (equal native-comp-driver-options '("-Wl,-w"))))))

(ert-deftest emacs-kit-other-compilers-unchanged ()
  (dolist (gcc '((14 2 0) (15 1 0) nil))
    (emacs-kit-test--platform 'darwin "27.2\n" gcc t
      (should-not (emacs-kit-configure-native-comp))
      (should (= os-calls 0)))))

(ert-deftest emacs-kit-without-native-compilation ()
  (emacs-kit-test--platform 'darwin "27.2\n" '(14 3 0) nil
    (should-not (emacs-kit-configure-native-comp))
    (should (= os-calls 0))))

(ert-deftest emacs-kit-version-detection-failure-is-harmless ()
  (emacs-kit-test--platform 'darwin "27.2\n" '(14 3 0) t
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _args) (signal 'file-error '("unavailable")))))
      (should-not (emacs-kit-configure-native-comp)))
    (should (equal native-comp-driver-options '("-Wl,-w")))))

(ert-deftest emacs-kit-workaround-is-idempotent ()
  (emacs-kit-test--platform 'darwin "27.2\n" '(14 3 0) t
    (emacs-kit-configure-native-comp)
    (emacs-kit-configure-native-comp)
    (should (= (length native-comp-driver-options) 2))))

(defmacro emacs-kit-test--startup-directory (&rest body)
  "Run BODY in an isolated startup directory and remove only that fixture."
  (declare (indent 0))
  `(let* ((root (make-temp-file "emacs-kit-test-" t))
          (user-emacs-directory (file-name-as-directory root))
          (early-init-file (expand-file-name "early-init.el" root)))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(ert-deftest emacs-kit-default-load-has-no-user-file-or-warning-changes ()
  (emacs-kit-test--startup-directory
    (let ((warning-suppress-types '((existing)))
          (emacs-kit-macos-native-comp-workaround nil))
      (load emacs-kit-test--source nil t)
      (should-not (file-exists-p early-init-file))
      (should (equal warning-suppress-types '((existing)))))))

(ert-deftest emacs-kit-explicit-installer-and-fresh-process-loader ()
  (skip-unless (version<= "27.1" emacs-version))
  (emacs-kit-test--startup-directory
    (should (eq (emacs-kit-install-early-init) 'installed))
    (should (eq (emacs-kit-install-early-init) 'already-configured))
    ;; A fresh process verifies that the generated loader actually executes,
    ;; rather than passing because this test process already loaded the kit.
    (with-temp-buffer
      (should (= 0 (call-process
                    (expand-file-name invocation-name invocation-directory)
                    nil t nil "--batch" "-Q" "--load" early-init-file
                    "--eval"
                    "(unless (featurep 'emacs-kit-early-init) (error \"Loader did not execute\"))"))))))

(ert-deftest emacs-kit-installer-preserves-custom-startup ()
  (skip-unless (version<= "27.1" emacs-version))
  (emacs-kit-test--startup-directory
    (write-region ";; user settings\n" nil early-init-file nil 'silent)
    (should-error (emacs-kit-install-early-init) :type 'user-error)
    (should (equal (with-temp-buffer
                     (insert-file-contents early-init-file) (buffer-string))
                   ";; user settings\n"))))

(ert-deftest emacs-kit-installer-preserves-broken-symlink ()
  (skip-unless (version<= "27.1" emacs-version))
  (emacs-kit-test--startup-directory
    (condition-case nil
        (make-symbolic-link (concat early-init-file ".missing") early-init-file)
      (file-error (ert-skip "Symlinks unavailable")))
    (should-error (emacs-kit-install-early-init) :type 'user-error)
    (should (file-symlink-p early-init-file))))

(ert-deftest emacs-kit-installer-refuses-concurrent-overwrite ()
  (skip-unless (version<= "27.1" emacs-version))
  (emacs-kit-test--startup-directory
    (let ((write (symbol-function 'write-region)))
      (cl-letf (((symbol-function 'write-region)
                 (lambda (&rest args)
                   (funcall write ";; concurrent file\n" nil early-init-file nil 'silent)
                   (apply write args))))
        (should-error (emacs-kit-install-early-init) :type 'file-already-exists)))
    (should (equal (with-temp-buffer
                     (insert-file-contents early-init-file) (buffer-string))
                   ";; concurrent file\n"))))


(ert-deftest emacs-kit-existing-entry-loads-shared-settings-without-installing ()
  (emacs-kit-test--startup-directory
    (let* ((checkout (expand-file-name "checkout/" root))
           (features (copy-sequence features))
           (warning-suppress-types '((existing)))
           (emacs-kit-macos-native-comp-workaround nil)
           (emacs-kit-early-init-source
            (expand-file-name "early-init.el" checkout)))
      (make-directory checkout)
      (copy-file emacs-kit-test--source emacs-kit-early-init-source)
      (copy-file (expand-file-name "init.el"
                                   (file-name-directory emacs-kit-test--source))
                 (expand-file-name "init.el" checkout))
      (dolist (directory '("kits" "extensions" "org"))
        (make-directory (expand-file-name directory checkout)))
      (setq features (delq 'emacs-kit-early-init features))
      ;; Isolate unrelated package installation and server creation; exercise
      ;; the same load-file entry used by existing .emacs configurations.
      (dolist (feature '(packages-kit builtin-kit core-utilities ui-kit
                        key-bindings mode-hooks osx-kit))
        (provide feature))
      (cl-letf (((symbol-function 'server-start) (lambda (&rest _args) nil)))
        (load-file (expand-file-name "init.el" checkout)))
      (should (featurep 'emacs-kit-early-init))
      (should-not (file-exists-p early-init-file))
      (should (equal warning-suppress-types '((existing)))))))

;;; startup-tests.el ends here
