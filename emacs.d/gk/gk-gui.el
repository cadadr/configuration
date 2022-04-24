;;; gk-gui.el --- GUI customisations                 -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022  Göktuğ Kayaalp

;;; Commentary:

;; GUI-specific customisations.

;;; Code:

(require 'custom)
(require 'highlight-parentheses)
(require 'org-variable-pitch)

(unless (gk-gui-p)
  (user-error "‘gk-gui’ should only be loaded in a graphical session."))

;;;; Fonts:

;; Default fonts to use in this config.

(defconst gk-default-fonts-plist
  (list :serif "DejaVu Serif Condensed"
        :sans "DejaVu Sans Condensed"
        :mono "Iosevka Cadadrish Sans"
        :cjk "Noto Serif CJK JP"
        :emoji "Noto Color Emoji"
        :forecast-moon-phase (or (and (font-info "Quivira")
                                      "Quivira")
                                 "DejaVu Sans"))
  "A plist, default fonts.")

;; Set up so that there's 80-85 chars width for half-sized horizontal
;; windows.
(defconst gk-font-default-height 110)
(defconst gk-font-variable-pitch-height 110)

(defun gk-font (type)
  "Get default font for TYPE, a keyword.

nil if absent."
  (plist-get gk-default-fonts-plist type))



;;;; GUI:

(defvar gk-preferred-themes (list :light 'modus-operandi
                                  :dark  'modus-vivendi)
  "Light and dark theme preferences.")


(defvar gk-gui-theme nil
  "The default theme's name to load at startup.")


(defvar gk-gui-theme-customisation-functions nil
  "Functions to customise themes.

Each function is run with a single argument, the currently
selected colour theme, and is supposed to check if it wants to
customise that theme or not.")

(defmacro gk-inhibit-frame-setup (&rest body)
  "Execute body, ensure ‘gk-inhibit-frame-setup’ is not run."
  `(cl-flet ((gk-setup-frame-looks
               (&rest _)
               (message "Suppressed ‘gk-setup-frame-looks’.")
               nil))
     ,@body))

(defun gk-setup-frame-looks (&optional frame arg)
  "Customisations that modify frame behaviour.

Groups such customisations which might need to be re-ran when a
new frame is created.

When called interactively with prefix argument, prompt for theme
selection.  Otherwise, use the theme ‘gk-preferred-colour-scheme’
picks."
  (interactive
   (list
    (selected-frame)
    (not (not current-prefix-arg))))

  (ignore frame)

  (setf gk-gui-theme
        (if arg
            (intern
             (completing-read
              "Load custom theme: "
              (mapcar #'symbol-name (custom-available-themes))))
          (plist-get gk-preferred-themes
                     (gk-preferred-colour-scheme))))

  (when (and gk-gui-theme
             (not (equal custom-enabled-themes
                         (list gk-gui-theme))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme gk-gui-theme t))

  ;; Customise selected theme.
  (run-hook-with-args 'gk-gui-theme-customisation-functions
                      gk-gui-theme)

  ;; Region should not have a foreground colour.
  (set-face-attribute 'region nil :foreground nil)

  (set-face-attribute 'default nil
                      :height gk-font-default-height
                      :family (gk-font :mono)
                      :weight 'regular)

  (set-fontset-font t 'symbol (gk-font :emoji))

  (set-face-attribute 'variable-pitch nil
                      :height gk-font-variable-pitch-height
                      :family (gk-font :sans))

  (loop for attr in '(mode-line mode-line-inactive) do
        (set-face-attribute attr nil
                            :family (gk-font :sans)
                            :height gk-font-variable-pitch-height
                            :weight 'normal))

  ;; This inherits from mode-line, so we need to fix it for its proper
  ;; display.
  (set-face-attribute 'header-line nil
                      :family (gk-font :mono)
                      :height gk-font-default-height)

  ;; Special font for moon phase visualisation in forecast.el.
  (set-face-attribute 'forecast-moon-phase nil
                      :font (gk-font :forecast-moon-phase))

  ;; Make parentheses more obvious.
  (set-face-attribute 'parenthesis nil :foreground nil :inherit 'font-lock-keyword-face)
  (set-face-attribute 'show-paren-match nil :background nil  :inverse-video t)
  (set-face-attribute 'show-paren-mismatch nil :inherit 'warning)
  (set-face-attribute 'hl-paren-face nil :underline t :bold t)

  ;; Adapts ‘highlight-parentheses-mode’ colours to theme.
  (let ((c (cond ((eq gk-gui-theme 'zenburn) "#ff4500") ;orange red
                 ((eq gk-gui-theme 'wombat)  "#b22222") ;firebrick
                 ((eq gk-gui-theme 'dichromacy) "#8b008b") ;navy
                 (t (face-attribute 'font-lock-keyword-face :foreground)))))
    (setf hl-paren-colors
          (list
           (color-lighten-name c 10)
           (color-lighten-name c 20)
           (color-lighten-name c 30)
           (color-lighten-name c 40))))

  ;; Reset it everywhere it’s found in order to apply the above
  ;; settings.
  (dolist (b (buffer-list))
    (when highlight-parentheses-mode
      (highlight-parentheses-mode -1)
      (highlight-parentheses-mode +1)))

  ;; Have a bit more line-spacing.
  (setq-default line-spacing 0.2))

(add-to-list 'gk-disabled-modes 'tool-bar-mode)
(add-to-list 'gk-disabled-modes 'menu-bar-mode)
(add-to-list 'gk-disabled-modes 'scroll-bar-mode)

;; Fixes blank area above window after startup with Athena.
(setf x-frame-normalize-before-maximize t)

(add-hook 'after-init-hook #'gk-setup-frame-looks)
;;  (add-hook 'after-init-hook #'gk-frame-recenter)

(add-hook 'after-make-frame-functions #'gk-setup-frame-looks)
(add-hook 'after-make-frame-functions #'gk-frame-recenter)

;; Set up cursors:
(setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows 'hollow)

(dolist (hook '(special-mode-hook dired-mode-hook rmail-mode-hook rmail-summary-mode-hook))
  (add-hook hook ($ (setq-local cursor-type 'box))))



;;;;; Theme customisations:

(add-hook 'gk-gui-theme-customisation-functions
          (lambda (theme)
            "Customisations for wombat."
            (when (eq theme 'wombat)
              ;; With wombat the active window is hard to tell.
              (set-face-attribute 'mode-line nil
                                  :background "black"
                                  :foreground "white")
              ;; Default added face renders foreground unreadable.
              (set-face-attribute 'diff-refine-added nil
                                  :background "dark olive green")
              (set-face-attribute 'region nil :foreground nil)

              ;; Nicer, unambigous headline colours for Org mode headlines that
              ;; form a scale from green to red.  Also, make headlines a little
              ;; bit larger if ‘org-variable-pitch-minor-mode’ is enabled.
              (let ((colours ["yellow green"
                              "khaki"
                              "dark sea green"
                              "light sea green"
                              "steel blue"
                              "slate blue"
                              "orchid"
                              "hotpink"]))
                (dotimes (i 7)
                  (let ((face (intern (format "org-level-%d" (1+ i)))))
                    (set-face-attribute face nil :foreground (aref colours i) :height 1.0)
                    (when (with-temp-buffer
                            (gk-org-visuals-hook)
                            org-variable-pitch-minor-mode)
                      (set-face-attribute face nil :height 1.2))))))))

(add-hook 'gk-gui-theme-customisation-functions
          (lambda (theme)
            "Customisations for misterioso."
            (when (eq theme 'misterioso)
              (set-face-attribute 'header-line nil :background "black"
                                  :foreground "white")
              (set-face-attribute 'show-paren-match nil
                                  :foreground "green yellow"
                                  :background nil)
              (set-face-attribute 'diff-refine-added nil
                                  :background "sea green"))))

(add-hook 'gk-gui-theme-customisation-functions
          (lambda (theme)
            "Common customisations for misterioso and wombat."
            (when (memq theme '(wombat misterioso))
              ;; Make the cursor more visible, the default grey colour is
              ;; indistinguishable, especially with the bar cursor.
              (set-face-attribute 'cursor nil :background "hotpink")
              ;; Don't change the foreground or decorate the text when
              ;; ‘hl-line-mode’ is on.
              (set-face-attribute 'highlight nil
                                  :foreground nil
                                  :underline nil))))

(add-hook 'gk-gui-theme-customisation-functions
          (lambda (theme)
            "Customisations for paper theme."
            (when (eq theme 'paper)
              ;; Better background colour for region.
              (set-face-attribute 'region nil :background "medium spring green"))))

(add-hook 'gk-gui-theme-customisation-functions
          (lambda (theme)
            "Customisations for dracula theme."
            (when (eq theme 'dracula)
              ;; Less prominent inactive modeline.
              (set-face-attribute
               'mode-line nil :box t :foreground nil :background nil :inherit 'mode-line)
              (set-face-attribute 'region nil :background "black"))))

(add-hook 'gk-gui-theme-customisation-functions
          (lambda (theme)
            "Customisations for modus themes"
            (when (eq theme 'modus-operandi)
              (let ((bg (face-attribute 'org-block-begin-line :background))
                    (fg (face-attribute 'org-block-begin-line :foreground)))
                (dolist (f '(org-block-begin-line org-block-end-line))
                  (set-face-attribute f nil :background bg :extend t :foreground fg))
                (set-face-attribute 'org-block-begin-line nil :overline t)
                (set-face-attribute 'org-block-end-line nil :overline nil)
                (set-face-attribute 'org-block-end-line nil :underline t)
                (set-face-attribute 'org-block nil :background bg :extend t)))

            (when (eq theme 'modus-vivendi)
              (set-face-attribute 'flymake-warning nil :background nil :underline "yellow")
              (set-face-attribute 'flymake-error nil :background nil :underline "red"))))

(add-hook 'gk-gui-theme-customisation-functions
          (lambda (theme)
            "Customisations for the yoshi theme."
            (when (eq theme 'yoshi)
              (set-face-attribute 'org-level-1 nil :underline nil)
              (set-face-attribute 'org-level-2 nil :weight 'regular)
              (set-face-attribute 'org-level-3 nil :italic nil))))

(add-hook 'gk-gui-theme-customisation-functions
          (lambda (theme)
            "Customisations for the inkpot theme."
            (when (eq theme 'inkpot)
              (set-face-attribute 'mode-line-inactive nil :inverse-video t)
              ;; The normal colours are eye murderingly bright-on-bright.
              (set-face-attribute
               'widget-field nil
               :foreground "white"
               :background (face-attribute 'mode-line :background)))))

(add-hook 'gk-gui-theme-customisation-functions
          (lambda (theme)
            (when (memq theme '(gruvbox-dark-medium gruvbox-dark-hard))
              ;; Make comments readable.
              (set-face-attribute
               'font-lock-comment-face nil
               :foreground (face-attribute 'font-lock-string-face :foreground)
               :background (face-attribute 'default :background)
               :italic t)
              (let ((bgi (face-attribute 'mode-line-inactive :background))
                    (fgi (face-attribute 'mode-line-inactive :foreground))
                    (bga (face-attribute 'mode-line :background))
                    (fga (face-attribute 'mode-line :foreground)))
                (set-face-attribute 'mode-line-inactive nil :foreground fga)
                (set-face-attribute 'mode-line-inactive nil :background bga)
                (set-face-attribute 'mode-line nil :foreground fgi)
                (set-face-attribute 'mode-line nil :background bgi)))))

(add-hook 'gk-gui-theme-customisation-functions
          (lambda (theme)
            (when (eq theme 'anti-zenburn)
              ;; Make comments readable.
              (set-face-attribute
               'font-lock-comment-face nil
               :foreground (face-attribute 'font-lock-string-face :foreground)
               :background (face-attribute 'default :background)
               :italic t)
              ;; Remove underline from org-ellipsis because why?
              (set-face-attribute 'org-ellipsis nil :underline nil))))



(provide 'gk-gui)
;;; gk-gui.el ends here
