;;; flycheck-swift3.el --- Flycheck: Swift support

;; Copyright (c) 2016 GyazSquare Inc.

;; Author: Goichi Hirakawa <gooichi@gyazsquare.com>
;; URL: https://github.com/GyazSquare/flycheck-swift3
;; Version: 0.0.0
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
;; Usage:
;;
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-swift3-setup))

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
  "Specifies which SDK to compile against.

When non-nil, set the SDK path to compile against, via `-sdk'."
  :type '(directory :tag "SDK path")
  :safe #'stringp)

(flycheck-def-option-var flycheck-swift3-target nil swift
  "Generate code for the given target."
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

(flycheck-def-option-var flycheck-swift3-inputs nil swfit
  "Specifies input files to parse.

When non-nil, set the input files to parse."
  :type '(repeat (string :tag "Input file"))
  :safe #'flycheck-string-list-p)

(defun flycheck-swift3--swift-sdk-path (xcrun-path xcrun-sdk)
  "Return the the swift SDK path.

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
            "-parse"
            (option-list "-D" flycheck-swift3-conditional-compilation-flags)
            (option-list "-F" flycheck-swift3-framework-search-paths)
            (option-list "-I" flycheck-swift3-import-search-paths)
            (option "-module-name" flycheck-swift3-module-name)
            (eval (let ((swift-sdk-path (flycheck-swift3--swift-sdk-path
                                         ,xcrun-path
                                         flycheck-swift3-xcrun-sdk)))
                    (when swift-sdk-path `("-sdk" ,swift-sdk-path))))
            (option "-target" flycheck-swift3-target)
            (option "-import-objc-header" flycheck-swift3-import-objc-header)
            (option-list "-Xcc" flycheck-swift3-xcc-args)
            (eval (let ((file-name (or load-file-name buffer-file-name)))
                    (remove file-name (flycheck-swift3--expand-inputs
                                       flycheck-swift3-inputs
                                       (file-name-directory file-name)))))
            "-primary-file" source)))
    (if xcrun-path
        (let ((xcrun-command
               `(,xcrun-path
                 (option "--sdk" flycheck-swift3-xcrun-sdk)
                 (option "--toolchain" flycheck-swift3-xcrun-toolchain))))
          (append xcrun-command command))
      command)))

(flycheck-define-command-checker 'swift
  "A Swift syntax checker using Swift compiler frontend.

See URL `https://swift.org/'."
  :command (flycheck-swift3--syntax-checking-command)
  :standard-input t
  :error-patterns
  '((info line-start (or "<stdin>" (file-name)) ":" line ":" column
          ": note: " (optional (message)) line-end)
    (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
             ": warning: " (optional (message)) line-end)
    (error line-start (or "<stdin>" (file-name)) ":" line ":" column
           ": " (or "fatal error" "error") ": " (optional (message)) line-end))
  :modes 'swift-mode)

;;;###autoload
(defun flycheck-swift3-setup ()
  "Set up Flycheck for Swift."
  (add-to-list 'flycheck-checkers 'swift))

(provide 'flycheck-swift3)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-swift3.el ends here
