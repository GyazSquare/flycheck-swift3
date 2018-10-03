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

;; Debug:
;; In flycheck.el:flycheck-start-command-checker, add (when (equal checker 'swift3) (message "%s %s" checker args))

;;; Code:

(require 'flycheck)
(require 'xcode-project)

(defcustom flycheck-swift3-use-xcode-project t
  "Specifies whether to use the `xcode-project' package to parse project settings.

When non-nil, project settings (SDK, compilation flags, source files etc) will
be obtained from the Xcode project associated with the current buffer's source
file.

If nil, or no Xcode project can be found, then fall back to the `flycheck-swift3' defaults."
  :group 'flycheck-swift3
  :type 'bool
  :safe #'booleanp)

(defcustom flycheck-swift3-xcode-build-config "Debug"
  "Build configuration to use when extracting build settings from the Xcode project."

  :group 'flycheck-swift3
  :type 'string
  :safe #'stringp)

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
  "Default SDK path if one cannot be inferred from the current buffer's Xcode project.

When non-nil, set the SDK path to compile against, via `-sdk'."
  :type '(directory :tag "SDK path")
  :safe #'stringp)

(flycheck-def-option-var flycheck-swift3-platform-target nil swift
  "Default platform target if one cannot be inferred from the current buffer's Xcode project."
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

(defun flycheck-swift3--xcrun-sdk-path (xcrun-path &optional xcrun-sdk)
  "Return the swift SDK path using `${XCRUN-PATH} --sdk ${XCRUN-SDK} --show-sdk-path'."
  (when xcrun-path
    (let ((command
           (if xcrun-sdk
               (mapconcat #'identity `(,xcrun-path "--sdk" ,xcrun-sdk "--show-sdk-path") " ")
             (mapconcat #'identity `(,xcrun-path "--show-sdk-path") " "))))
      (string-trim (shell-command-to-string command)))))

(defun flycheck-swift3--list-swift-files (directory)
  "Return list of full paths to swift files in the specified DIRECTORY."
  (seq-filter
   (lambda (elt) (eq 0 (string-match-p "[^\.].*" (file-name-nondirectory elt))))
   (directory-files directory t ".*\.swift$")))

(defvar flycheck-swift3--cache-directory nil
  "The cache directory for `flycheck-swift3'.")

(defun flycheck-swift3--cache-location ()
  "Get the cache location for `flycheck-swift3'.

If no cache directory exists yet, create one and return it.
Otherwise return the previously used cache directory."
  (setq flycheck-swift3--cache-directory
        (or flycheck-swift3--cache-directory
            ;; Note: we can't use flycheck-temp-dir-system because the temp dir
            ;; will be destroyed after each run of the checker...
            (make-temp-file flycheck-temp-prefix 'directory))))

(defun flycheck-swift3--cache-cleanup ()
  "Cleanup `flycheck-swift3' cache directory."
  (when (and flycheck-swift3--cache-directory
             (file-directory-p flycheck-swift3--cache-directory))
    (flycheck-safe-delete flycheck-swift3--cache-directory))
  (setq flycheck-swift3--cache-directory nil))

;; Clean-up cache directories
(add-hook 'kill-emacs-hook #'flycheck-swift3--cache-cleanup)

(defun flycheck-swift3--xcodeproj-modtime (xcproj-path)
  "Return the modification time for XCPROJ-PATH."
  (setq xcproj-path (expand-file-name xcproj-path))
  (if (and xcproj-path (file-directory-p xcproj-path))
      (nth 5 (file-attributes (xcode-project-concat-path xcproj-path "project.pbxproj")))))

(defun flycheck-swift3--xcodeproj-cache-path (xcproj-path)
  "Return the cache path for the specified XCPROJ-PATH."
  (when (and xcproj-path (file-directory-p xcproj-path))
    (format "%s/%s-%s" (flycheck-swift3--cache-location)
            (file-name-nondirectory xcproj-path)
            (time-to-seconds (flycheck-swift3--xcodeproj-modtime xcproj-path)))))

(defun flycheck-swift3--read-xcode-project-cache (xcproj-path)
  "Return the project cache for XCPROJ-PATH.
Return nil if the cache is not found, or the modification time of the project has changed."
  (when-let (cache-path (flycheck-swift3--xcodeproj-cache-path xcproj-path))
    (when (file-exists-p cache-path)
      (xcode-project-deserialize cache-path))))

(defun flycheck-swift3--write-xcode-project-cache (proj xcproj-path)
  "Cache the parsed project PROJ for XCPROJ-PATH."
  (when-let (cache-path (flycheck-swift3--xcodeproj-cache-path xcproj-path))
    (ignore-errors (xcode-project-serialize proj cache-path))))

(defun flycheck-swift3--load-xcode-project (xcproj-path)
  "Load and return the Xcode project found at XCPROJ-PATH.

If the parsed Xcode project is found in our cache, return that.
Otherwise read from disk, cache and return the project."
  (setq xcproj-path (expand-file-name xcproj-path))
  (when (and xcproj-path (file-directory-p xcproj-path))
    (or (flycheck-swift3--read-xcode-project-cache xcproj-path)
        (when-let (proj (xcode-project-read xcproj-path))
          (flycheck-swift3--write-xcode-project-cache proj xcproj-path)
          proj))))

(defun flycheck-swift3--xcode-build-settings (xcproj target-name)
  "Return the build settings for the specified XCPROJ and TARGET-NAME."
  (let ((config-name (or flycheck-swift3-xcode-build-config "Debug")))
    (when xcproj
      (alist-get 'buildSettings (xcode-project-build-config xcproj
                                                            config-name
                                                            target-name)))))

(defun flycheck-swift3--platform-target (build-settings)
  "Return the platform target for BUILD-SETTINGS.
If no valid target is found, return flycheck-swift3-platform-target."
  (if build-settings
      (let ((macos-target (alist-get 'MACOSX_DEPLOYMENT_TARGET build-settings))
            (iphoneos-target (alist-get 'IPHONEOS_DEPLOYMENT_TARGET build-settings)))
        (cond (macos-target
               (format "x86_64-apple-macosx%s" macos-target))
              (iphoneos-target
               ;; We never want "arm*" for flycheck.
               (format "x86_64-apple-ios%s" iphoneos-target))))
    flycheck-swift3-platform-target))

(defun flycheck-swift3--swift-version (build-settings)
  "Return the swift version for BUILD-SETTINGS."
  (when-let (swift-version (alist-get 'SWIFT_VERSION build-settings))
    ;; -swift-version appears to require integers (4 not 4.0 etc).
    ;; Major versions, such as 4.2, are however valid.
    (if (equal (fround swift-version) swift-version)
        (number-to-string (truncate swift-version))
      (number-to-string swift-version))))

(defun flycheck-swift3--target-build-dir (target-name)
  "Return the target build dir for TARGET-NAME.
Uses heuristics to locate the build dir in ~/Library/Developer/Xcode/DerivedData/."
  (let* ((build-root (expand-file-name "~/Library/Developer/Xcode/DerivedData/"))
         (results (directory-files-and-attributes build-root t (concat target-name "\\-[a-z]+"))))
    (car (seq-reduce (lambda (result item)
                       (if (time-less-p (nth 6 result) (nth 6 item))
                           item
                         result)) results (car results)))))

(defun flycheck-swift3--sdk-path (build-settings xcrun-path)
  "Return the platform sdk for BUILD-SETTINGS.
If no valid sdk is found, return flycheck-swift3--xcrun-sdk-path using XCRUN-PATH.

If BUILD-SETTINGS is nil return flycheck-swift3-sdk-path else flycheck-swift3--xcrun-sdk-path."
  (if build-settings
      (let ((sdk-root (alist-get 'SDKROOT build-settings)))
        (when (equal sdk-root "iphoneos")
          (setq sdk-root "iphonesimulator"))
        (flycheck-swift3--xcrun-sdk-path xcrun-path sdk-root))
    (or flycheck-swift3-sdk-path
        (flycheck-swift3--xcrun-sdk-path xcrun-path))))

(defun flycheck-swift3--source-files (xcproj target-name)
  "Return the swift source files associated with the current buffer.

If XCPROJ and TARGET-NAME are non-nil then returns the source files
for the specified Xcode target.

If flycheck-swift3-inputs is non-nil returns these inputs.

Otherwise returns all .swift file found in the current buffer's directory."
  (if xcproj
      (xcode-project-build-file-paths xcproj
                                      target-name
                                      "PBXSourcesBuildPhase"
                                      (lambda (file)
                                        (xcode-project-file-ref-extension-p file "swift"))
                                      'absolute)
    (let* ((file-name (or load-file-name buffer-file-name))
           (directory-name (file-name-directory file-name)))
      (or (flycheck-swift3--expand-inputs flycheck-swift3-inputs directory-name)
          (flycheck-swift3--list-swift-files directory-name)))))

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

(defun flycheck-swift3--search-paths (key build-settings)
  "Return a list of resolved search paths for KEY in BUILD-SETTINGS."
  (when-let (search-paths (alist-get key build-settings))
    (if (stringp search-paths)
        (seq-into (split-string search-paths) 'vector)
      search-paths)))

(defun flycheck-swift3--gcc-compilation-flags (build-settings)
  "Return a list of GCC conditional compilation flags found in BUILD-SETTINGS."
  (when-let (preprocessor-defs (alist-get 'GCC_PREPROCESSOR_DEFINITIONS build-settings))
    (when (stringp preprocessor-defs)
      (setq preprocessor-defs (vector preprocessor-defs)))
    (seq-concatenate 'vector
                     preprocessor-defs
                     (alist-get 'GCC_PREPROCESSOR_DEFINITIONS_NOT_USED_IN_PRECOMPS build-settings))
    ))

(defun flycheck-swift3--swiftc-options (file-name xcrun-path)
  "Return a list of swiftc command line options for FILE-NAME.

If flycheck-swift3-use-xcode-project is t then use the associated
Xcode project's build settings to determine command line options.

The XCRUN-PATH is used to locate tools and sdks.

Otherwise fall back to the flycheck-swift3 custom options."
  (let* ((xcproj (when flycheck-swift3-use-xcode-project
                   (flycheck-swift3--load-xcode-project (xcode-project-find-xcodeproj file-name))))
         (target-name (when xcproj
                        (car (xcode-project-target-names-for-file xcproj file-name "PBXSourcesBuildPhase"))))
         (build-settings (flycheck-swift3--xcode-build-settings xcproj target-name))
         (build-products-dir (xcode-project-concat-path (flycheck-swift3--target-build-dir target-name)
                                                                 "Build/Products"
                                                                 (or flycheck-swift3-xcode-build-config "Debug"))))
    `(
       ,(if (version< (flycheck-swift3--swiftc-version xcrun-path) "3.1")
            "-parse" "-typecheck")
       ,@(flycheck-prepend-with-option "-module-name" (list target-name))
       ,@(flycheck-prepend-with-option "-sdk"
                                       (list (flycheck-swift3--sdk-path build-settings xcrun-path)))
       ,@(flycheck-prepend-with-option "-target"
                                       (list (flycheck-swift3--platform-target build-settings)))
       ,@(flycheck-prepend-with-option "-swift-version"
                                       (list (flycheck-swift3--swift-version build-settings)))
       ,@(flycheck-prepend-with-option "-F" (flycheck-swift3--search-paths 'FRAMEWORK_SEARCH_PATHS
                                                                           build-settings))
       ,@(flycheck-prepend-with-option "-I" (flycheck-swift3--search-paths 'HEADER_SEARCH_PATHS
                                                                           build-settings))
       ,@(flycheck-prepend-with-option "-I" (flycheck-swift3--search-paths 'USER_HEADER_SEARCH_PATHS
                                                                           build-settings))
       ,@(flycheck-prepend-with-option "-I" (flycheck-swift3--search-paths 'SYSTEM_HEADER_SEARCH_PATHS
                                                                           build-settings))
       ,@(flycheck-prepend-with-option "-I" (flycheck-swift3--search-paths 'SWIFT_INCLUDE_PATHS
                                                                           build-settings))
       ;; Add target build dir to ensure that any framework dependencies are found
       ,@(when build-products-dir
           (flycheck-prepend-with-option "-F" (list build-products-dir)))
       ,@(when build-products-dir
           (flycheck-prepend-with-option "-I" (list build-products-dir)))
       ,@(flycheck-prepend-with-option "-D" (flycheck-swift3--gcc-compilation-flags build-settings))
       ,(when-let (opt-level (alist-get 'SWIFT_OPTIMIZATION_LEVEL build-settings))
          opt-level)
       ,@(when-let (source-files (flycheck-swift3--source-files xcproj target-name))
           (remove file-name source-files)))))

(defun flycheck-swift3--syntax-checking-command ()
  "Return the command to run for Swift syntax checking."
  (let* ((xcrun-path (executable-find "xcrun"))
         (command
    `("swiftc"
      "-frontend"
      (option-list "-D" flycheck-swift3-conditional-compilation-flags)
      (option-list "-F" flycheck-swift3-framework-search-paths)
      (option-list "-I" flycheck-swift3-import-search-paths)
      (option "-import-objc-header" flycheck-swift3-import-objc-header)
      (option-list "-Xcc" flycheck-swift3-xcc-args)
      ;; Options which require an Xcode project are evaluated together to avoid
      ;; loading the project more than once during a check.
      (eval (let* ((file-name (or load-file-name buffer-file-name)))
              (flycheck-swift3--swiftc-options file-name ,xcrun-path)))
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

;; (flycheck-swift3--swiftc-options "~/Projects/nhojb/metaltest/MetalTest/MetalView.swift" "xcrun")

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
