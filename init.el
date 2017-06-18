;;;; My init.el
;;;; Mappings
;;; Synonym for delete key.
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
;;; For consistency with C-h.
(global-set-key (kbd "M-h") 'backward-kill-word)
;;; Alternative for the original action of C-h (help).
;;; In my environment, suspension with C-z does not work (it's intentionally) and I don't use it.
;;; If C-z were to fail, use <F1>.
(global-set-key (kbd "C-z") help-map)

;;; Alternative for C-a.
;;; C-a is used for a prefix key of GNU Screen.
(global-set-key (kbd "C-q") 'move-beginning-of-line)

;;; Alternative for the original action of C-q.
(global-set-key (kbd "C-x M-q") 'quoted-insert)

;;; Load init file quickly.
(defun load-current-file ()
  (interactive)
  (load-file (buffer-file-name)))

(defun load-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-x RET RET") 'load-current-file)
(global-set-key (kbd "C-x RET .") 'load-init-file)




;;;; Packages
(require 'package)
(package-initialize)
(package-refresh-contents)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defconst packages-to-install
  '(
    clojure-mode
    sml-mode
    ))

(dolist (package packages-to-install)
  (unless (package-installed-p package)
    (package-install package)))



;;;; Filetypes
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
