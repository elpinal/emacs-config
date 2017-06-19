(deftheme monotone
  "Monotone color theme")

(custom-theme-set-faces
 'monotone

 '(cursor ((t (:foreground "#fdf4c1"))))

 '(default ((t (:background "#282828" :foreground "#fdf4c1"))))

 '(font-lock-builtin-face ((t (:foreground "#88778d"))))

 '(font-lock-comment-face ((t (:foreground "#7c6f64"))))

 '(font-lock-constant-face((t (:foreground "#d3869b"))))

 '(font-lock-function-name-face ((t (:foreground "#da8dff"))))

 '(font-lock-keyword-face ((t (:foreground "#fb347a"))))

 '(font-lock-string-face ((t (:foreground "#bd7cc1"))))

 '(font-lock-type-face ((t (:foreground "#d96dc3"))))

 '(font-lock-variable-name-face ((t (:foreground "#d69bdd"))))

 '(fringe ((t (:background "#282828"))))

 '(highlight ((t (:foreground "#000000" :background "#c4be89"))))

 '(minibuffer-prompt ((t (:foreground "#a860e5" :bold t))))

 '(mode-line ((t (:foreground "#282828" :background "#7c6f64" :box (:line-width 1 :color "#000000" :style released-button)))))

 '(mode-line-inactive ((t (:foreground "#bcbcbc" :background "#333333" :box (:line-width 1 :color "#333333")))))

 '(region ((t (:background "#5e5551"))))

 '(secondary-selection ((t (:background "#3e3834"))))

 '(show-paren-match-face ((t (:foreground "#1b1d1e" :background "#fd971f")))))

(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'monotone)
