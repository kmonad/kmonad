;;; kbd-mode.el --- Font locking for kmonad's .kbd files. -*- lexical-binding: t -*-

;; Copyright 2020  slotThe

;; Author: slotThe <soliditsallgood@mailbox.org>
;; URL: TBD
;; Version: 0.0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds basic font locking support for `.kbd' configuration files.  As
;; the configuration language is a tiny lisp, we inherit from `lisp-mode' in
;; order to get good parenthesis handling for free.

;; To use this file, move it to a directory within your `load-path' and then
;; associate the `.kbd' file ending with `kbd-mode'.  For example --- assuming
;; that this file was placed within the `~/.config/emacs/elisp' directory:
;;
;;     (add-to-list 'load-path "~/.config/emacs/elisp/")
;;     (require 'kbd-mode)
;;     (add-to-list 'auto-mode-alist '("\\.kbd\\'" . kbd-mode))
;;
;; If you use `use-package', you can express the above as
;;
;;     (use-package kbd-mode
;;       :load-path "~/.config/emacs/elisp/"
;;       :mode "\\.kbd\\'")

;; By default we highlight all keywords; you can change this by customizing the
;; `kbd-mode-' variables.  For example, to disable the highlighting of already
;; defined macros (i.e. of "@macro-name"), you can set `kbd-mode-show-macros' to
;; `nil'.

;;; Code:

;;; Custom variables

(defcustom kbd-mode-kexpr
  '("defcfg" "defsrc" "defalias")
  "A K-Expression"
  :type '(repeat string))

;; TODO: There's probably a nicer way to do this.
(defcustom kbd-mode-function-one
  '("deflayer")
  "Tokens that are treated as functions with one argument"
  :type '(repeat string))

(defcustom kbd-mode-tokens
  '(;; input tokens
    "uinput-sink" "send-event-sink" "kext"
    ;; output tokens
    "device-file" "low-level-hook" "iokit-name")
  "Input and output tokens"
  :type '(repeat string))

(defcustom kbd-mode-defcfg-options
  '("input" "output" "cmp-seq" "init" "fallthrough" "allow-cmd")
  "Options to give to `defcfg'"
  :type '(repeat string))

(defcustom kbd-mode-button-modifiers
  '("around" "multi-tap" "tap-hold" "tap-hold-next" "tap-next-release"
    "tap-hold-next-release" "tap-next" "layer-toggle" "layer-switch"
    "layer-add" "layer-rem" "layer-delay" "layer-next" "around-next"
    "tap-macro" "cmd-button")
  "Button modifiers"
  :type '(repeat string))

(defcustom kbd-mode-show-string
  '("uinput-sink" "device-file" "cmd-button")
  "Syntax highlight strings in the S-expressions defined by these
keywords"
  :type '(repeat string))

(defcustom kbd-mode-show-macros t
  "Whether to syntax highlight macros inside layout definitions.
Default: t"
  :type 'boolean)

;;; Faces

(defface kbd-mode-kexpr-face
  '((t :inherit font-lock-keyword-face))
  "Face for a K-Expression")

(defface kbd-mode-token-face
  '((t :inherit font-lock-function-name-face))
  "Face for input and output tokens")

(defface kbd-mode-defcfg-option-face
  '((t :inherit font-lock-builtin-face))
  "Face for options one may give to `defcfg'")

(defface kbd-mode-button-modifier-face
  '((t :inherit font-lock-function-name-face))
  "Face for all the button modifiers")

(defface kbd-mode-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for a variables, i.e. layer names, macros in layers,...")

(defface kbd-mode-string-face
  '((t :inherit font-lock-string-face))
  "Face for strings")

;;; Vars

(defvar kbd-mode-syntax-table nil
  "Use ;; for regular comments and #| |# for line comments")
(setq kbd-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments.
    (modify-syntax-entry ?\; ". 12b" table)
    (modify-syntax-entry ?\n "> b"   table)
    (modify-syntax-entry ?\# ". 14"  table)
    (modify-syntax-entry ?\| ". 23"  table)

    ;; We don't need to highlight brackets, as they're only used inside layouts.
    (modify-syntax-entry ?\[ "."     table)
    (modify-syntax-entry ?\] "."     table)

    ;; We highlight the necessary strings ourselves.
    (modify-syntax-entry ?\" "."     table)
    table))

(defvar kbd-mode--font-lock-keywords nil
  "Keywords to be syntax highlighted")
(setq kbd-mode--font-lock-keywords
  (let ((kexpr-regexp            (regexp-opt kbd-mode-kexpr            'words))
        (token-regexp            (regexp-opt kbd-mode-tokens           'words))
        (defcfg-options-regexp   (regexp-opt kbd-mode-defcfg-options   'words))
        (button-modifiers-regexp (regexp-opt kbd-mode-button-modifiers 'words))
        (function-one-regexp
         (concat "\\(?:\\("
                 (regexp-opt kbd-mode-function-one)
                 "\\)\\([[:space:]]+[[:word:]]+\\)\\)"))
        ;; Only highlight these strings; configuration files may use a single "
        ;; to emit a quote, so we can't trust `lisp-mode's string highlighting.
        (string-regexp
         (concat "\\(['\(]"
                 (regexp-opt kbd-mode-show-string)
                 "\\)\\(\\S)+\\)\)")))

    `((,token-regexp            (1 'kbd-mode-token-face          ))
      (,kexpr-regexp            (1 'kbd-mode-kexpr-face          ))
      (,button-modifiers-regexp (1 'kbd-mode-button-modifier-face))
      (,defcfg-options-regexp   (1 'kbd-mode-defcfg-option-face  ))
      (,function-one-regexp
       (1 'kbd-mode-kexpr-face        )
       (2 'kbd-mode-variable-name-face))
      (,string-regexp
       ("\"[^}]*?\""
        (progn (goto-char (match-beginning 0)) (match-end 0))
        (goto-char (match-end 0))
        (0 font-lock-string-face t))))))

;;; Define Major Mode

(define-derived-mode kbd-mode lisp-mode "Kbd"
  "Major mode for editing `.kbd' files"

  (set-syntax-table kbd-mode-syntax-table)
  (font-lock-add-keywords 'kbd-mode kbd-mode--font-lock-keywords)

  ;; TODO: There *has* to be a better way of doing this
  (let ((macro-regexp '(("\\(:?\\(@[^[:space:]]+\\)\\)"
                         (1 'kbd-mode-variable-name-face)))))
    (if kbd-mode-show-macros
        (font-lock-add-keywords 'kbd-mode macro-regexp)
      (font-lock-remove-keywords 'kbd-mode macro-regexp)))

  (font-lock-flush))

(provide 'kbd-mode)
;;; kbd-mode.el ends here
