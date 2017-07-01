;;; init.el --- My init.el

;; Copyright (C) 2017 El Pin Al

;;; Commentary:

;; The following rules are originally derived from
;; Kana's vimrc <https://github.com/kana/config>.
;;
;;
;; * This file consists of "sections".
;;
;;   - The name each section should be single word.
;;
;; * Each section consists of zero or more "subsections".
;;
;;   - There is no rule for the name of each subsection.
;;
;; * The last subsection in a section should be named as "Misc.".
;;
;; * Whenever new subsection is inserted,
;;   it should be inserted just before "Misc." subsection.
;;
;; * If a setting can be categorized into two or more sections,
;;   it should be put into the most bottom section in this file.
;;
;;   For example, key mappings for a specific plugin should be put into the
;;   "Plugins" section.
;;
;;
;; Coding Rule
;;
;; * Separate sections with 8 blank lines.
;;
;; * Separate subsections with 4 blank lines.
;;
;; * Character Encoding: Use UTF-8 for this file and other files such as
;;   plugins.
;;
;; * Line length Limit:
;;
;;   - Non-Comment: There are no line length limit.  If a line feels too long,
;;     wrap it and indent with an extra spaces.
;;
;;   - Comment: Limit all lines to a maximum of 79 characters.







;;; Code:

;;;; Basic

;; Show "East Asian Ambiguous Width" as single width.
(set-language-environment "English")

;; For package compatibility.
(custom-set-variables '(shell-file-name "/bin/sh"))

;; Disable menu bar.
(menu-bar-mode 0)

;; Show column number.
(column-number-mode t)








;;;; Mappings

;; Synonym for delete key.
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)

;; For consistency with C-h.
(global-set-key (kbd "M-h") 'backward-kill-word)

;; Alternative for the original action of C-h (help).  In my environment,
;; suspension with C-z does not work (it's intentionally) and I don't use it.
;; If C-z were to fail, use <F1>.
(global-set-key (kbd "C-z") help-map)

;; Alternative for M-h.
(global-set-key (kbd "C-x M-h") 'mark-paragraph)

;; Alternative for C-a.
;; C-a is used for a prefix key of GNU Screen.
(global-set-key (kbd "C-q") 'move-beginning-of-line)

;; Alternative for the original action of C-q.
(global-set-key (kbd "C-x M-q") 'quoted-insert)

;; Load init file quickly.
(defun load-current-file ()
  "Load current file."
  (interactive)
  (load-file (buffer-file-name)))

(defun load-init-file ()
  "Load user init file."
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-x RET RET") 'load-current-file)
(global-set-key (kbd "C-x RET .") 'load-init-file)








;;;; Packages

(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)
;;(package-refresh-contents)

(defconst packages-to-install
  '(
    auto-complete
    cider
    clojure-mode
    flycheck
    ghc
    go-autocomplete
    go-mode
    haskell-mode
    magit
    smartparens
    sml-mode
    ))

(dolist (package packages-to-install)
  (unless (package-installed-p package)
    (package-install package)))


;;; Auto-Complete
(ac-config-default)

;;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Smartparens
(smartparens-global-mode t)








;;;; Filetypes
;;; Clojure

(require 'smartparens-config)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)


;;; Go

(add-hook 'before-save-hook 'gofmt-before-save)
(custom-set-variables '(gofmt-command "goimports"))
(require 'go-autocomplete)


;;; Haskell
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))


;;; OCaml

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (load-library "tuareg-site-file")
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))








;;;; Color Theme

(load-theme 'monotone t)
(enable-theme 'monotone)








;;; init.el ends here
