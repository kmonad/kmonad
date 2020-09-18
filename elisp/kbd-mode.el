;;; kbd-mode.el --- Syntax colouring for kmonad's .kbd files. -*- lexical-binding: t; -*-

;; Copyright © 2020, slotThe
;; License: GPL 3 or any later version

;;; Commentary:

;; TODO

;;; Code:

(defvar kbd-mode-kexpr
  '("defcfg" "defsrc" "defalias")
  "A K-Expression")

(defvar kbd-mode-function-one
  '("deflayer")
  "Tokens that are treated as functions with one argument")

(defface kbd-mode-kexpr-face
  '((t :inherit font-lock-keyword-face))
  "Face for a K-Expression")

(defvar kbd-mode-token
  '(;; input tokens
    "uinput-sink" "send-event-sink" "kext"
    ;; output tokens
    "device-file" "low-level-hook" "iokit-name")
  "Input and output tokens")

(defface kbd-mode-token-face
  '((t :inherit font-lock-function-name-face))
  "Face for input and output tokens")

(defvar kbd-mode-defcfg-options
  '("input" "output" "cmp-seq" "init" "fallthrough" "allow-cmd")
  "Options to give to `defcfg'")

(defface kbd-mode-defcfg-option-face
  '((t :inherit font-lock-builtin-face))
  "Face for options one may give to `defcfg'")

(defcustom kbd-mode-button-modifiers
  '("around" "multi-tap" "tap-hold" "tap-hold-next" "tap-next-release"
    "tap-hold-next-release" "tap-next" "layer-toggle" "layer-switch"
    "layer-add" "layer-rem" "layer-delay" "layer-next" "around-next"
    "tap-macro" "cmd-button")
  "Button modifiers")

(defface kbd-mode-button-modifier-face
  '((t :inherit font-lock-function-name-face))
  "Face for all the button modifiers")

(defface kbd-mode-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for a variables, i.e. layer names, macros in layers,...")

(defvar kbd-mode-show-string
  '("uinput-sink" "device-file" "cmd-button")
  "Syntax highlight strings in the S-expressions defined by these keywords")

(defface kbd-mode-string-face
  '((t :inherit font-lock-string-face))
  "Face for strings")

(defcustom kbd-mode-show-macros t
  "Whether to syntax highlight macros inside layout definitions.
Default: t"
  :type 'boolean)

(defvar kbd-mode--comments-table
  (let ((comments-table (make-syntax-table)))
    (modify-syntax-entry ?\; ". 12b" comments-table)
    (modify-syntax-entry ?\n "> b"   comments-table)
    (modify-syntax-entry ?\# ". 14"  comments-table)
    (modify-syntax-entry ?\| ". 23"  comments-table)
    comments-table)
  "Use ;; for regular comments and #| |# for line comments")

(defvar kbd-mode--font-lock-keywords
  (let ((kexpr-regexp            (regexp-opt kbd-mode-kexpr            'words))
        (token-regexp            (regexp-opt kbd-mode-tokens           'words))
        (defcfg-options-regexp   (regexp-opt kbd-mode-defcfg-options   'words))
        (button-modifiers-regexp (regexp-opt kbd-mode-button-modifiers 'words))
        (function-one-regexp     (concat "\\(?:\\("
                                         (regexp-opt kbd-mode-function-one)
                                         "\\)\\([[:space:]]+[[:word:]]+\\)\\)"))
        (string-regexp           (concat "\\(['\(]"
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
        (0 font-lock-string-face t)))))
  "Keywords to be syntax coloured")

(define-derived-mode kbd-mode lisp-mode "kbd"
  "Major mode for editing .kbd files"
  (font-lock-add-keywords 'kbd-mode kbd-mode--font-lock-keywords)
  (set-syntax-table kbd-mode--comments-table)

  ;; TODO: There *has* to be a better way of doing this
  (let ((macro-regexp '(("\\(:?\\(@[^[:space:]]+\\)\\)"
                         (1 'kbd-mode-variable-name-face)))))
    (if kbd-mode-show-macros
        (font-lock-add-keywords 'kbd-mode macro-regexp)
      (font-lock-remove-keywords 'kbd-mode macro-regexp)))

  (font-lock-flush))

(defun kbd--bracket-hack ()
  (modify-syntax-entry ?\[ ".")
  (modify-syntax-entry ?\] "."))
(add-hook 'kbd-mode-hook #'kbd--bracket-hack)

(add-hook 'kbd-mode-hook #'kbd--string-hack)
(defun kbd--string-hack ()
  (modify-syntax-entry ?\" "."))

(provide 'kbd-mode)
;;; kbd-mode.el ends here
