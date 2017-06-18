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




;;;; Packages
(require 'package)
(package-initialize)
(package-refresh-contents)

(defvar packages-to-install
  '(
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
