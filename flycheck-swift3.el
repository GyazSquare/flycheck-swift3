;;; flycheck-swift3.el --- Flycheck: Swift support for Apple swift-mode

;; Copyright (c) 2016-2017 GyazSquare Inc.

;; Author: Goichi Hirakawa <gooichi@gyazsquare.com>
;; URL: https://github.com/GyazSquare/flycheck-swift3
;; Version: 1.1.0
;; Keywords: convenience, languages, tools
;; Package-Requires: ((emacs "24.4") (flycheck "26"))

;; This file is not part of GNU Emacs.

;; MIT License

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Add Swift support to Flycheck using Swift compiler frontend.
;;
;; Flycheck-swift3 is designed to work with Apple swift-mode.el in the main
;; Swift repository <https://github.com/apple/swift/>.
;;
;; Features:
;;
;; - Apple swift-mode.el support
;; - Apple Swift 3.1 support
;;   If you use the toolchain option, you can use Swift 2.x.
;; - The `xcrun' command support (only on macOS)
;;
;; Usage:
;;
;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-swift3-setup))

;;; Code:

(require 'flycheck)

(flycheck-def-option-var flycheck-swift3-xcrun-sdk nil swift
  "Specifies which SDK to search for tools.

When non-nil, set the SDK name to find the tools, via `--sdk'.
The option is available only on macOS.

Use `xcodebuild -showsdks' to list the available SDK names."
  :type 'string
  :safe #'stringp)

(flycheck-def-option-var flycheck-swift3-xcrun-toolchain nil swift
  "Specifies which toolchain to use to perform the lookup.

When non-nil, set the toolchain identifier or name to use to
perform the lookup, via `--toolchain'.
The option is available only on macOS."
  :type 'string
  :safe #'stringp)

(flycheck-def-option-var flycheck-swift3-conditional-compilation-flags nil swift
  "Specifies conditional compilation flags marked as true.

When non-nil, add the specified conditional compilation flags via
`-D'."
  :type '(repeat (string :tag "Conditional compilation flag"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-swift3-framework-search-paths nil swift
  "Add directory to framework search paths.

When non-nil, add the specified directory to the search path for
framework include files, via `-F'."
  :type '(repeat (directory :tag "Framework directory"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-swift3-import-search-paths nil swift
  "Add directory to the import search paths.

When non-nil, add the specified directory to the search path for
import files, via `-I'."
  :type '(repeat (directory :tag "Import directory"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-swift3-module-name nil swift
  "Specifies name of the module to build.

When non-nil, set the name of the module to build, via
`-module-name'."
  :type 'string
  :safe #'stringp)

(flycheck-def-option-var flycheck-swift3-sdk-path nil swift
  "Default SDK if one cannot be inferred from the current buffer's Xcode project.

When non-nil, set the SDK path to compile against, via `-sdk'."
  :type '(directory :tag "SDK path")
  :safe #'stringp)

(flycheck-def-option-var flycheck-swift3-target nil swift
  "Default target if one cannot be inferred from the current buffer's Xcode project."
  :type 'string
  :safe #'stringp)

(flycheck-def-option-var flycheck-swift3-import-objc-header nil swift
  "Implicitly imports an Objective-C header file.

When non-nil, import an Objective-C header file via
`-import-objc-header'.

See URL
`https://developer.apple.com/library/content/documentation/Swift/Conceptual/BuildingCocoaApps/MixandMatch.html'
for more information about an Objective-C bridging header."
  :type '(file :tag "Objective C bridging header file")
  :safe #'stringp)

(flycheck-def-option-var flycheck-swift3-xcc-args nil swift
  "Pass <arg> to the C/C++/Objective-C compiler.

When non-nil, pass the specified arguments to the
C/C++/Objective-C compiler, via `-Xcc'."
  :type '(repeat (string :tag "Argument"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-swift3-inputs nil swift
  "Specifies input files to parse.

When non-nil, set the input files to parse."
  :type '(repeat (string :tag "Input file"))
  :safe #'flycheck-string-list-p)

(defun flycheck-swift3--swiftc-version (xcrun-path)
  "Return the swiftc version.

If `XCRUN-PATH' exists, return the swiftc version using
`'${XCRUN-PATH} swiftc --version'."
  (let* ((command
          (if xcrun-path
              (mapconcat #'identity `(,xcrun-path "swiftc" "--version") " ")
            (mapconcat #'identity `("swiftc" "--version") " ")))
         (version-info-list (delete "" (split-string
                                        (shell-command-to-string command)
                                        "[ \f\t\n\r\v():]+")))
         (versions (seq-filter
                    (lambda (elt) (string-match "^[0-9][-.0-9A-Za-z]*$" elt))
                    version-info-list)))
    (car versions)))

(defun flycheck-swift3--swift-sdk-path (xcrun-path xcrun-sdk)
  "Return the swift SDK path.

If `flycheck-swift3-sdk-path' is nil and xcrun exists, return the
swift SDK path using `${XCRUN-PATH} --sdk ${XCRUN-SDK}
--show-sdk-path'."
  (or flycheck-swift3-sdk-path
      (and xcrun-path
           (let ((command
                  (if xcrun-sdk
                      (mapconcat
                       #'identity
                       `(,xcrun-path "--sdk" ,xcrun-sdk "--show-sdk-path")
                       " ")
                    (mapconcat
                     #'identity
                     `(,xcrun-path "--show-sdk-path")
                     " "))))
             (string-trim (shell-command-to-string command))))))

(defun flycheck-swift3--list-swift-files (directory)
  "Return list of full paths to swift files in the specified DIRECTORY."
  (seq-filter
   (lambda (elt) (eq 0 (string-match-p "[^\.].*" (file-name-nondirectory elt))))
   (directory-files directory t ".*\.swift$")))

(defun flycheck-swift3--find-xcodeproj (directory-or-file)
  "Search DIRECTORY-OR-FILE and parent directories for an Xcode project file.
Returns the path to the Xcode project, or nil if not found."
  (if directory-or-file
      (let (xcodeproj
            (directory (if (file-directory-p directory-or-file)
                           directory-or-file
                         (file-name-directory directory-or-file))))
        (setq directory (expand-file-name directory))
        (while (and (eq xcodeproj nil) (not (equal directory "/")))
          (setq xcodeproj (directory-files directory t ".*\.xcodeproj$"))
          (setq directory (file-name-directory (directory-file-name directory))))
        (car xcodeproj))))

(defvar-local flycheck-swift3--xcode-build-settings-cache nil
  "An alist mapping Xcode projects (path) to build settings.
This avoids called 'xcodebuild' more than once.
To force a reload of the cache just set this variable to nil.")

(defun flycheck-swift3--xcodeproj-modtime (xcodeproj)
  "Return the modification time for XCODEPROJ."
  (setq xcodeproj (expand-file-name xcodeproj))
  (if (file-directory-p xcodeproj)
      (nth 5 (file-attributes (concat (file-name-as-directory xcodeproj) "project.pbxproj")))))

(defun flycheck-swift3--get-xcode-build-settings-cache (xcodeproj)
  "Return the build settings cache for XCODEPROJ.
Return nil if the cache is not found, or the modification time of the project has changed."
  (setq xcodeproj (expand-file-name xcodeproj))
  (if (file-directory-p xcodeproj)
      (when-let ((cache (cadr (assoc xcodeproj flycheck-swift3--xcode-build-settings-cache))))
        (let ((modtime (flycheck-swift3--xcodeproj-modtime xcodeproj)))
          (if (equal modtime (car cache))
              (cadr cache))))))

(defun flycheck-swift3--set-xcode-build-settings-cache (xcodeproj build-settings)
  "Set the build settings cache for XCODEPROJ to BUILD-SETTINGS."
  (setq xcodeproj (expand-file-name xcodeproj))
  (if (file-directory-p xcodeproj)
      (let ((modtime (flycheck-swift3--xcodeproj-modtime xcodeproj)))
        (push (list xcodeproj (list modtime build-settings)) flycheck-swift3--xcode-build-settings-cache))))

(defun flycheck-swift3--get-xcode-build-settings (xcodeproj xcrun-path)
  "Return the build settings for the specified XCODEPROJ file."
  (setq xcodeproj (expand-file-name xcodeproj))
  (let ((build-settings (flycheck-swift3--get-xcode-build-settings-cache xcodeproj)))
    (unless build-settings
      (if (and (file-directory-p xcodeproj) xcrun-path)
          ;; Note: closing stderr so we don't have to deal with error messages.
          (let* ((command (format "%s xcodebuild -showBuildSettings -project \"%s\" 2>&- | grep -F '='" xcrun-path xcodeproj))
                 (lines (shell-command-to-string command)))
              (if (string-empty-p lines)
                  (setq lines nil)
                (setq lines (split-string (string-trim lines) "\n")))
              (while lines
                (let* ((kv (split-string (car lines) "="))
                       (key (string-trim (car kv)))
                       (value (string-trim (cadr kv))))
                  (if (and key value)
                      ;; Use 'list' to evaluate key & value
                      (push (list key value) build-settings)))
                (setq lines (cdr lines)))
              (if build-settings
                  (flycheck-swift3--set-xcode-build-settings-cache xcodeproj build-settings)))))
    build-settings))

(defun flycheck-swift3--get-xcode-target (xcodeproj xcrun-path)
  "Return the platform target for XCODEPROJ.
If no valid target is found, return flycheck-swift3-target."
  (let* ((target flycheck-swift3-target)
         (build-settings (flycheck-swift3--get-xcode-build-settings xcodeproj xcrun-path))
         (swift-platform-target (cadr (assoc "SWIFT_PLATFORM_TARGET_PREFIX" build-settings)))
         ;; NATIVE_ARCH_ACTUAL will be "x86_64" for macOS and for the iOS Simulator.
         ;; We never want "arm*" for flycheck.
         (native-arch (cadr (assoc "NATIVE_ARCH_ACTUAL" build-settings)))
         (macos-target (cadr (assoc "MACOSX_DEPLOYMENT_TARGET" build-settings)))
         (iphoneos-target (cadr (assoc "IPHONEOS_DEPLOYMENT_TARGET" build-settings)))
         (deployment-target (if macos-target
                                macos-target
                              iphoneos-target)))
    (if (and native-arch swift-platform-target deployment-target)
        (setq target (format "%s-apple-%s%s" native-arch swift-platform-target deployment-target)))
    target))

(defun flycheck-swift3--get-xcode-sdk-path (xcodeproj xcrun-path)
  "Return the platform sdk for XCODEPROJ.
If no valid sdk is found, return flycheck-swift3-sdk-path."
  (let* (sdk-path
         (build-settings (flycheck-swift3--get-xcode-build-settings xcodeproj xcrun-path))
         (swift-platform-target (cadr (assoc "SWIFT_PLATFORM_TARGET_PREFIX" build-settings))))
    (if (equal swift-platform-target "ios")
        (setq sdk-path (cadr (assoc "CORRESPONDING_SIMULATOR_SDK_DIR" build-settings)))
      (setq sdk-path (cadr (assoc "SDK_DIR" build-settings))))
    (if sdk-path
        sdk-path
      (flycheck-swift3--swift-sdk-path xcrun-path flycheck-swift3-sdk-path))))

(defun flycheck-swift3--expand-inputs (inputs &optional directory)
  "Return the expanded inputs.

If input files `INPUTS' is not nil, return the list of expanded
input files using `DIRECTORY' as the default directory."
  (let (expanded-inputs)
    (dolist (input inputs expanded-inputs)
      (if (file-name-absolute-p input)
          (setq expanded-inputs
                (append expanded-inputs (file-expand-wildcards input t)))
        (setq expanded-inputs
              (append expanded-inputs
                      (file-expand-wildcards
                       (expand-file-name input directory) t)))))))

(defun flycheck-swift3--syntax-checking-command ()
  "Return the command to run for Swift syntax checking."
  (let* ((xcrun-path (executable-find "xcrun"))
         (command
          `("swiftc"
            "-frontend"
            (eval (if (version<
                       (flycheck-swift3--swiftc-version ,xcrun-path) "3.1")
                      "-parse" "-typecheck"))
            (option-list "-D" flycheck-swift3-conditional-compilation-flags)
            (option-list "-F" flycheck-swift3-framework-search-paths)
            (option-list "-I" flycheck-swift3-import-search-paths)
            (option "-module-name" flycheck-swift3-module-name)
            (eval (let* ((file-name (or load-file-name buffer-file-name))
                         (xcodeproj (flycheck-swift3--find-xcodeproj file-name))
                         (swift-sdk-path (flycheck-swift3--get-xcode-sdk-path xcodeproj ,xcrun-path)))
                    (when swift-sdk-path `("-sdk" ,swift-sdk-path))))
            (eval (let* ((file-name (or load-file-name buffer-file-name))
                         (xcodeproj (flycheck-swift3--find-xcodeproj file-name))
                         (target (flycheck-swift3--get-xcode-target xcodeproj ,xcrun-path)))
                    (when target `("-target" ,target))))
            (option "-import-objc-header" flycheck-swift3-import-objc-header)
            (option-list "-Xcc" flycheck-swift3-xcc-args)
            (eval (let* ((file-name (or load-file-name buffer-file-name))
                         (directory-name (file-name-directory file-name)))
                    (remove file-name (or (flycheck-swift3--expand-inputs flycheck-swift3-inputs directory-name)
                                          (flycheck-swift3--list-swift-files directory-name)))))
            "-primary-file"
            ;; Read from standard input
            "-")))
    (if xcrun-path
        (let ((xcrun-command
               `(,xcrun-path
                 (option "--sdk" flycheck-swift3-xcrun-sdk)
                 (option "--toolchain" flycheck-swift3-xcrun-toolchain))))
          (append xcrun-command command))
      command)))

(flycheck-define-command-checker 'swift3
  "A Swift syntax checker using Swift compiler frontend.

See URL `https://swift.org/'."
  :command (flycheck-swift3--syntax-checking-command)
  :standard-input t
  :error-patterns
  '((error line-start "<unknown>:" line
           ": " "error: " (optional (message)) line-end)
    (info line-start (or "<stdin>" (file-name)) ":" line ":" column
          ": " "note: " (optional (message)) line-end)
    (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
             ": " "warning: " (optional (message)) line-end)
    (error line-start (or "<stdin>" (file-name)) ":" line ":" column
           ": " "error: " (optional (message)) line-end))
  :modes 'swift-mode)

;;;###autoload
(defun flycheck-swift3-setup ()
  "Set up Flycheck for Swift."
  (add-to-list 'flycheck-checkers 'swift3))

(provide 'flycheck-swift3)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-swift3.el ends here
