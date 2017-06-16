;;;; My init.el
;;;; Mappings
;;; Synonim for delete key. To see help, use <F1> instead.
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)




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
