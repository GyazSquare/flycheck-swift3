;;; flycheck-swift3-test.el --- Flycheck Swift: Test cases

;; Copyright (c) 2016-2018 GyazSquare Inc.

;; Author: Goichi Hirakawa <gooichi@gyazsquare.com>
;; URL: https://github.com/GyazSquare/flycheck-swift3

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

;; Test cases Flycheck Swift.

;;; Code:

(require 'flycheck-ert)
(require 'flycheck-swift3)

(message "Running tests on Emacs %s" emacs-version)

(defconst flycheck-swift3-test-directory
  (let ((filename (if load-in-progress load-file-name (buffer-file-name))))
    (expand-file-name "test/" (locate-dominating-file filename "Cask")))
  "Test suite directory, for resource loading.")

(flycheck-ert-def-checker-test swift3 swift error
  (let ((flycheck-checkers '(swift3))
        (flycheck-swift3-inputs '("A.swift")))
    (flycheck-ert-should-syntax-check
     "broken.swift" 'swift-mode
     '(1 11 error "use of undeclared type 'X'" :checker swift3))))

(flycheck-ert-def-checker-test swift3 swift error-info
  (let ((flycheck-checkers '(swift3))
        (flycheck-swift3-inputs '("A.swift")))
    (flycheck-ert-should-syntax-check
     "A.swift" 'swift-mode
     '(5 18 info "protocol requires nested type 'Assoc'; do you want to add it?"
         :checker swift3)
     '(8 8 error "type 'A' does not conform to protocol 'P'" :checker swift3)
     '(9 13 info "possibly intended match 'Assoc' (aka 'Int') does not conform to 'PHelper'"
         :checker swift3))))

(flycheck-ert-def-checker-test swift3 swift error-unknown
  (let ((flycheck-checkers '(swift3))
        (flycheck-swift3-import-objc-header "hello-bridge-header.h"))
    (flycheck-ert-should-syntax-check
     "hello.swift" 'swift-mode
     '(0 nil error "failed to import bridging header 'hello-bridge-header.h'"
         :checker swift3)
     `(1 2 error "\"error message\"" :checker swift3
         :filename ,(expand-file-name "hello-bridge-header.h"
                                      flycheck-swift3-test-directory)))))

(flycheck-ert-def-checker-test swift3 swift error-warning-info
  (let ((flycheck-checkers '(swift3))
        (flycheck-swift3-inputs '("unknowable.swift")))
    (flycheck-ert-should-syntax-check
     "unknowable.swift" 'swift-mode
     '(8 3 warning "result of 'Int' initializer is unused" :checker swift3)
     '(17 6 info "found this candidate" :checker swift3)
     '(18 6 info "found this candidate" :checker swift3)
     '(22 29 error "ambiguous use of 'ovlLitB'" :checker swift3)
     '(46 12 error "argument type 'Double' does not conform to expected type 'CanWibble'"
          :checker swift3))))

(flycheck-ert-def-checker-test swift3 swift warning
  (let ((flycheck-checkers '(swift3))
        (flycheck-swift3-inputs '("strange-characters.swift")))
    (flycheck-ert-should-syntax-check
     "strange-characters.swift" 'swift-mode
     '(4 5 warning "nul character embedded in middle of file" :checker swift3)
     '(5 5 warning "nul character embedded in middle of file" :checker swift3)
     '(6 15 warning "nul character embedded in middle of file"
         :checker swift3))))

(flycheck-ert-def-checker-test swift3 swift warning-info
  (let ((flycheck-checkers '(swift3))
        (flycheck-swift3-inputs '("diag_unreachable_after_return.swift")))
    (flycheck-ert-should-syntax-check
     "diag_unreachable_after_return.swift" 'swift-mode
     '(7 3 warning "expression following 'return' is treated as an argument of the 'return'"
         :checker swift3)
     '(7 3 info "indent the expression to silence this warning"
         :checker swift3)
     '(13 3 warning "expression following 'return' is treated as an argument of the 'return'"
          :checker swift3)
     '(13 3 info "indent the expression to silence this warning"
          :checker swift3)
     '(19 5 warning "expression following 'return' is treated as an argument of the 'return'"
          :checker swift3)
     '(19 5 info "indent the expression to silence this warning"
          :checker swift3))))

(flycheck-ert-def-checker-test swift3 swift swift-version
  (let ((flycheck-checkers '(swift3))
        (flycheck-swift3-inputs '("objc-inference.swift"))
        (flycheck-swift3-swift-version "3"))
    (flycheck-ert-should-syntax-check
     "objc-inference.swift" 'swift-mode)))

(flycheck-ert-def-checker-test swift3 swift swift3-objc-inference-default
  (let ((flycheck-checkers '(swift3))
        (flycheck-swift3-inputs '("objc-inference.swift")))
    (flycheck-ert-should-syntax-check
     "objc-inference.swift" 'swift-mode
     '(6 10 info "add '@objc' to make this declaration overridable"
         :checker swift3)
     '(10 19 error "overriding non-@objc declarations from extensions is not supported"
          :checker swift3))))

(flycheck-ert-def-checker-test swift3 swift swift3-objc-inference-on
  (let ((flycheck-checkers '(swift3))
        (flycheck-swift3-inputs '("objc-inference.swift"))
        (flycheck-swift3-swift3-objc-inference 'on))
    (flycheck-ert-should-syntax-check
     "objc-inference.swift" 'swift-mode
     '(6 10 info "add '@objc' to expose this instance method to Objective-C"
         :checker swift3)
     '(10 19 warning "override of instance method 'extMethod()' from extension of 'MySuperclass' depends on deprecated inference of '@objc'"
          :checker swift3))))

(flycheck-ert-def-checker-test swift3 swift swift3-objc-inference-off
  (let ((flycheck-checkers '(swift3))
        (flycheck-swift3-inputs '("objc-inference.swift"))
        (flycheck-swift3-swift3-objc-inference 'off))
    (flycheck-ert-should-syntax-check
     "objc-inference.swift" 'swift-mode
     '(6 10 info "add '@objc' to make this declaration overridable"
         :checker swift3)
     '(10 19 error "overriding non-@objc declarations from extensions is not supported"
          :checker swift3))))

(flycheck-ert-initialize flycheck-swift3-test-directory)

(provide 'flycheck-swift3-test)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-swift3-test.el ends here
