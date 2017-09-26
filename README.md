# flycheck-swift3

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![MELPA](https://melpa.org/packages/flycheck-swift3-badge.svg)](https://melpa.org/#/flycheck-swift3)
[![MELPA Stable](https://stable.melpa.org/packages/flycheck-swift3-badge.svg)](https://stable.melpa.org/#/flycheck-swift3)
[![Build Status](https://api.travis-ci.org/GyazSquare/flycheck-swift3.svg?branch=master)](https://travis-ci.org/GyazSquare/flycheck-swift3)

A Swift syntax checker using Swift compiler frontend.

Flycheck-swift3 is designed to work with Apple [swift-mode.el](https://github.com/apple/swift/blob/master/utils/swift-mode.el) in the [main Swift repository](https://github.com/apple/swift/).

## Features

* Apple [swift-mode.el](https://github.com/apple/swift/blob/master/utils/swift-mode.el) support
* Apple Swift 4 support  
  If you use the toolchain option, you can use the old version of Swift.
* The `xcrun` command support (only on macOS)

## Requirements

* Apple Emacs Lisp files for Swift
  * [swift-mode.el](https://raw.githubusercontent.com/apple/swift/master/utils/swift-mode.el)
  * [swift-project-settings.el](https://raw.githubusercontent.com/apple/swift/master/utils/swift-project-settings.el) (optional)
  * [inferior-swift-mode.el](https://raw.githubusercontent.com/apple/swift/master/utils/inferior-swift-mode.el) (optional)
  * [sil-mode.el](https://raw.githubusercontent.com/apple/swift/master/utils/sil-mode.el) (optional)
* [Flycheck](http://www.flycheck.org/)

## Installation

You can install `flycheck-swift3.el` from the [MELPA](https://melpa.org/) or the [MELPA Stable](https://stable.melpa.org/) repository with `package.el`.

In your `init.el`:

```elisp
(require 'flycheck-swift3) ; Not necessary if using ELPA package
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-swift3-setup))
```

## License

This software is licensed under the MIT License.

See the LICENSE file for details.
