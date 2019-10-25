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

;; Keep a newline at the end of the file.
(setq require-final-newline t)

;; Don't use tabs.
(custom-set-variables '(indent-tabs-mode nil))








;;;; Mappings
;;; Windows

(global-set-key (kbd "<left>") 'windmove-left)
(global-set-key (kbd "<down>") 'windmove-down)
(global-set-key (kbd "<up>") 'windmove-up)
(global-set-key (kbd "<right>") 'windmove-right)

;; Synonim for 'C-x 0'.
(global-set-key (kbd "M-c") 'delete-window)




;;; Minibuffer

;;; Disable <up> and <down> since I want to use their global maps to
;;; move around windows. The original actions are almost performed by
;;; <M-p> / <M-n> / C-p / C-n.
(define-key minibuffer-local-map (kbd "<up>") nil)
(define-key minibuffer-local-map (kbd "<down>") nil)




;;; Editing

;; To reserve M-l / M-u / M-c for other use.
;; Like Vim's gu / gU.
(global-set-key (kbd "C-x g u") 'downcase-word)
(global-set-key (kbd "C-x g U") 'upcase-word)
;; "t" stands for "t"itlecase.
(global-set-key (kbd "C-x g t") 'capitalize-word)




;;; Misc.

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

;; Alternatives for C-a and C-M-a.
;; C-a is used for a prefix key of GNU Screen.
(global-set-key (kbd "C-q") 'move-beginning-of-line)
(global-set-key (kbd "C-M-q") 'beginning-of-defun)

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

;; Open init file quickly.
(global-set-key (kbd "C-x ,") (lambda ()
				"Open init file."
				(interactive) (find-file "~/src/github.com/elpinal/emacs-config/init.el")))

;; Launch Eshell in another window.
(global-set-key (kbd "C-x !") 'eshell)

(global-set-key (kbd "M-e") (lambda (n)
			      "Scroll up N lines."
			      (interactive "p") (scroll-up n)))
(global-set-key (kbd "M-y") (lambda (n)
			      "Scroll down N lines."
			      (interactive "p") (scroll-down n)))

;; Alternatives for M-e / M-y.
(global-set-key (kbd "C-x }") 'forward-sentence)
(global-set-key (kbd "C-x C-y") 'yank-pop)

;; Alternatives for C-x } / C-x {.
;; -- the default action of C-x > / C-x < is almost never used.
(global-set-key (kbd "C-x >") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x <") 'shrink-window-horizontally)

;; Scroll by the half of window height.
(global-set-key (kbd "C-v") (lambda (n)
			      "Scroll up the half of window height."
			      (interactive "p") (scroll-up (* n (max 1 (/ (window-body-height) 2))))))
(global-set-key (kbd "M-v") (lambda (n)
			      "Scroll down the half of window height."
			      (interactive "p") (scroll-down (* n (max 1 (/ (window-body-height) 2))))))

(global-set-key (kbd "M-l") 'move-to-window-line-top-bottom)

;; Swap M-z and C-x z.
(global-set-key (kbd "M-z") 'repeat)
(global-set-key (kbd "C-x z") 'zap-to-char)

;; Switch to alternative buffer.
(global-set-key (kbd "C-^") (lambda ()
			      "Switch to alternative buffer."
			      (interactive) (switch-to-buffer nil)))

(global-set-key (kbd "M-g C-l") 'linum-mode)








;;;; Packages
;;; Settings

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))




;;; Package installation

(defconst packages-to-install
  '(
    auctex
    auto-complete
    cider
    clojure-mode
    company-coq
    ddskk
    docker-tramp
    flycheck
    fsharp-mode
    ghc
    go-autocomplete
    go-mode
    haskell-mode
    hindent
    kibit-helper
    magit
    markdown-mode
    proof-general
    (satysfi :type git :host github :repo "gfngfn/satysfi.el"
	     :fork (:host github :repo "elpinal/satysfi.el" :branch "no-background"))
    shm
    smartparens
    sml-mode
    twittering-mode
    undo-tree
    yaml-mode
    ))

(dolist (package packages-to-install)
  (straight-use-package package))




;;; Auto-Complete

(ac-config-default)




;;; DDSKK

(setq default-input-method "japanese-skk")




;;; Flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)




;;; Smartparens

(smartparens-global-mode t)

;; Alternative for the original action of "C-x -"
(global-set-key (kbd "C-x M--") 'shrink-window-if-larger-than-buffer)

(define-key smartparens-mode-map (kbd "C-x - u") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-x - b u") 'sp-backward-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-x - f s") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-x - b s") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-x - d") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-x - b d") 'sp-backward-kill-sexp)
(define-key smartparens-mode-map (kbd "C-x - y") 'sp-copy-sexp)
(define-key smartparens-mode-map (kbd "C-x - b y") 'sp-backward-copy-sexp)
(define-key smartparens-mode-map (kbd "C-x - t") 'sp-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-x - U") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-x - o") 'sp-splice-sexp-killing-around)
(define-key smartparens-mode-map (kbd "C-x - |") 'sp-split-sexp)
(define-key smartparens-mode-map (kbd "C-x - j") 'sp-join-sexp)
(define-key smartparens-mode-map (kbd "C-x - c") 'sp-rewrap-sexp)
(define-key smartparens-mode-map (kbd "C-x - ~") 'sp-swap-enclosing-sexp)
(define-key smartparens-mode-map (kbd "C-x - e") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-x - b e") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-x - v") 'sp-select-next-thing)
(define-key smartparens-mode-map (kbd "C-x - b v") 'sp-select-previous-thing)

(define-key smartparens-mode-map (kbd "C-x - s") 'sp-slurp-hybrid-sexp)
(define-key smartparens-mode-map (kbd "C-x - k") 'sp-kill-hybrid-sexp)




;;; Magit

(require 'magit)

;; Alternative for the original action of "C-x m".
(global-set-key (kbd "C-x M-m") 'compose-mail)

(global-set-key (kbd "C-x m") 'magit-status)

;; "C-x g" is used globally for other purposes.
(define-key magit-file-mode-map (kbd "C-x g") nil)




;;; Org

(custom-set-variables '(org-directory "~/working/notes")
		      '(org-default-notes-file (concat org-directory "/gtd.org"))
		      '(org-log-done t)
		      '(org-refile-targets '((org-default-notes-file :level . 1)))
		      '(org-capture-templates '(("i" "Inbox" entry
						 (file+headline "" "Inbox")
						 "* %?%i\n  %U\n  %a\n"))))
(global-set-key (kbd "C-x /") (lambda ()
				"Open GTD file"
				(interactive) (find-file "~/working/notes/gtd.org")))
(global-set-key (kbd "C-c c") 'org-capture)




;;; Twittering

(setq twittering-use-master-password t)
(setq epa-pinentry-mode 'loopback)
(setq twittering-display-remaining t)
(setq twittering-status-format
      "%RT{%FACE[bold]{RT}}%i %S (@%s),  %@:\n%FOLD[  ]{%T // from %f%L%r%R%QT{\n+----\n%FOLD[|]{%i %s,  %@:\n%FOLD[  ]{%T // from %f%L%r%R}}\n+----}}\n ")




;;; Undo-Tree

(global-undo-tree-mode)

(define-key undo-tree-map (kbd "M-r") 'undo-tree-redo)








;;;; Filetypes
;;; Agda

(load-file (let ((coding-system-for-read 'utf-8))
	     (shell-command-to-string "agda-mode locate")))




;;; Clojure

(require 'smartparens-config)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)




;;; Coq

(add-hook 'coq-mode-hook #'company-coq-mode)




;;; Go

(add-hook 'before-save-hook 'gofmt-before-save)
(custom-set-variables '(gofmt-command "goimports"))
(require 'go-autocomplete)




;;; Haskell

(require 'haskell-interactive-mode)
(require 'haskell-process)

(add-to-list 'load-path "~/.stack/snapshots/x86_64-osx/lts-4.0/7.10.3/share/x86_64-osx-ghc-7.10.3/HaRe-0.8.2.1/elisp")

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

(custom-set-variables '(haskell-tags-on-save t)
		      '(haskell-process-suggest-remove-import-lines t)
		      '(haskell-process-auto-import-loaded-modules t)
		      '(haskell-process-log t)
		      '(haskell-process-type 'stack-ghci)
		      '(haskell-compile-cabal-build-command "stack build"))




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




;;; SATySFi

(require 'satysfi)
(add-to-list 'auto-mode-alist '("\\.saty$" . satysfi-mode))
(add-to-list 'auto-mode-alist '("\\.satyh$" . satysfi-mode))
(add-to-list 'auto-mode-alist '("\\.satyg$" . satysfi-mode))




;;; Stletiori

(add-to-list 'auto-mode-alist '("\\.stl$" . clojure-mode))
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)




;;; Twelf

(setq twelf-root "~/Downloads/twelf.plparty.org/builds/twelf/")
(load (concat twelf-root "emacs/twelf-init.el"))








;;;; Color Theme

(load-theme 'monotone t)
(enable-theme 'monotone)








;;; init.el ends here
