;;; init.el -- Config file using el-get
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)                ;; if you use any :bind variant
(use-package delight)
(use-package dash)

;; Custom Customizations
(setq custom-file (make-temp-file "emacs-customizations.el"))

;; Some hooks
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'sql-interactive-mode-hook
            (lambda ()
              (setq sql-prompt-regexp "^[_[:alpha:]]*[=][#>] ")
              (setq sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] ")))

(load "~/.emacs.d/secrets.el")


;; Default configs

;; Personal config
;; Disable startup message
(setq inhibit-startup-message t)

;; Quit tool-bar
(tool-bar-mode -1)

;; No scrollbar
(scroll-bar-mode -1)

;; Turn off alarms
(setq ring-bell-function 'ignore)

;; Typed text delete selection
(pending-delete-mode 1)

;; Show column
(column-number-mode 1)

;; Show parens
(show-paren-mode 1)

;; Indent with two spaces
(setq tab-width 2)

;; Backups
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 delete-old-versions t
 kept-new-versions 2
 kept-old-versions 1
 version-control t)

;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Winner mode
;; Restore window with C-c <left> (undo C-c <right>)
(winner-mode 1)

;; Join lines
(global-set-key (kbd "C-j") 'join-line)

;; Delete whitespaces
(global-set-key (kbd "M-_") 'delete-horizontal-space)

;; Unbindings
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "M-i"))
(global-unset-key (kbd "C-x C-b"))

;; Previous windownn
(defun rae-previous-window ()
  "Call `other-window with negative value."
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x p") 'rae-previous-window)

;; Auto-resize window
(defadvice other-window (after rae-resize-window activate)
  "Enlarge window to 80 columns, if is a `prog-mode' buffer."
  (let ((delta (- 80 (window-width))))
    (enlarge-window delta t)))

;; Move line (region sprex??) up
(defun my-up-to-next-line ()
  "Move up to next line."
  (interactive)
  (kill-line)
  (forward-line -1)
  (end-of-line)
  (yank))
(global-set-key (kbd "C-c u") 'my-up-to-next-line)

;; Toggle comment lines
(defun my-toggle-comment-on-line ()
  "Comment and uncomment lines."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-end-position))))
(global-unset-key (kbd "M-/"))
(global-set-key (kbd "M-/") 'my-toggle-comment-on-line)

;; Trailing whitespaces and save safetly taken from:
;; http://whattheemacsd.com/buffer-defuns.el-01.html
(defun my-cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a 'before-save-hook', and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))
;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'my-cleanup-buffer-safe)

;; Kill whole line
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") 'kill-whole-line)

;; Copy line
(defun copy-line (arg)
  "Copy lines (as many as prefix ARG) in the kill ring.
Ease of use features:
    - Move to start of next line.
    - Appends the copy on sequential calls.
    - Use newline as last char even on the last line of the buffer.
    - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

;; optional key binding
(global-set-key (kbd "C-c C-k") 'copy-line)

;; Reload file
(defun my-reload-file ()
  "Reload current file by reverting current buffer."
  (interactive)
  (revert-buffer t t t)
  (message "Reload %s"
           (buffer-file-name (current-buffer))))
(global-unset-key (kbd "M-R"))
(global-set-key (kbd "M-R") 'my-reload-file)

;; Smooth scroll
(setq scroll-conservatively 111)
(setq scroll-margin 4)

;; Keyboard and locale
;; Locales
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Change Ctrl and Meta on Mac
(setq mac-option-modifier 'super
      mac-right-option-modifier nil
      mac-command-modifier 'meta
      select-enable-clipboard t)

;;; Theme and fonts
;; Font size
(set-frame-font "Hack-14" nil t)

;; Transparency
;; Taken from:
;; https://www.reddit.com/r/emacs/comments/5rnpsm/
;; nice_hydra_to_set_frame_transparency/
;; With little modifications...
(defun rae/set-transparency (inc)
  "Change transparency in `INC' the selected frame transparency."
  (let ((current-alpha
         (or (frame-parameter (selected-frame) 'alpha) 80)))
    (set-frame-parameter (selected-frame) 'alpha (+ current-alpha inc))))

(defhydra hydra-transparency (:columns 2)
  "
 Alpha : [ %(frame-parameter (selected-frame) 'alpha) ] "
  ("j" (rae/set-transparency +1) "+ more")
  ("k" (rae/set-transparency -1) "- less")
  ("J" (rae/set-transparency +10) "++ more")
  ("K" (rae/set-transparency -10) "-- less")
  ("=" (lambda (value) (interactive "nTransparency Value 0 - 100 :")
         (set-frame-parameter (selected-frame) 'alpha value)) "Set to ?" :color blue))
(global-set-key (kbd "M-1") 'hydra-transparency/body)

;; Dimmer focus current Buffer
(use-package dimmer)

;; Packages
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

(use-package eshell
  :bind (("C-c e". eshell))
  :init
  (setq eshell-mv-interactive-query t
        eshell-cp-interactive-query t
        eshell-rm-interactive-query t)
  :config
  (defadvice eshell (around eshell-fullscreen activate)
    (window-configuration-to-register :eshell-fullscreen)
    ad-do-it
    (delete-other-windows)))

(use-package eshell-git-prompt
  :config (eshell-git-prompt-use-theme 'git-radar))

;; Packages config using el-get-bundle macro
;; Avy jump
(use-package avy
  :bind (("C-0" . avy-goto-word-or-subword-1)
         ("C-c 0" . avy-goto-char)
         ("C-c l" . avy-goto-line)))

;; Company
(use-package company
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)

  ;; disable company in eshell
  (setq company-global-modes '(not eshell-mode))

  ;; complete
  (add-to-list 'company-backends '(company-capf company-dabbrev))

  (define-key company-active-map [return] nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "M-<return>") 'company-complete-selection)
  (global-company-mode))

;; Company statistics
(use-package company-statistics
  :hook after-init-hook)

;; Swiper ivy counsel
(use-package counsel
  :delight ivy-mode
  :init
  (setq ivy-display-style 'fancy
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t)
  :bind (("C-s" . swiper)
         ("C-x b". ivy-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("M-y" . counsel-yank-pop) :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :config
  (ivy-mode t))

(use-package helpful
  :bind
  (("C-h f" . #'helpful-callable)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)
   ("C-c C-." . #'helpful-at-point)))

;; Smex (using abo-abo github repo)
(use-package smex
  :load-path "~/.emacs.d/git/smex/"
  :init (setq smex-completion-method 'ivy)
  :bind (("M-x" . smex)))

;; Magit
(use-package magit
  :bind (("M-9" . magit-status))
  :config
  ;; Full screen magit-status taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  ;; Switch projectile use magit-status-internal
  (defadvice magit-status-setup-buffer (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package forge
  :after magit)

(use-package git-timemachine)

(use-package haml-mode)


  :config
(use-package swift-mode)

;; Move text
(use-package move-text
  :bind (("M-P" . move-text-up)
         ("M-N" . move-text-down)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; Projectile
(use-package projectile-rails
  :init (setq projectile-completion-system 'ivy)
  :config
  (projectile-rails-global-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (with-eval-after-load 'rake
    (setq rake-completion-system 'ivy-read)))

;; Delight, persp-projectile take care.
;; (projectile-mode (:eval (format " [%s]" (projectile-project-name))))
(use-package counsel-projectile
  :delight projectile-mode
  :config
  (counsel-projectile-mode)
  (setq projectile-switch-project-action
        (lambda ()
          (magit-status-setup-buffer default-directory)
          (counsel-projectile ivy-current-prefix-arg)))
  (setq counsel-projectile-switch-project-action
        (lambda (project)
          (counsel-projectile-switch-project-by-name project))))

(use-package perspective
  :config
  (persp-mode))

(use-package persp-projectile
  :after (counsel-projectile perspective))

;; Ivy todo
(use-package ivy-todo
 :load-path "~/.emacs.d/git/ivy-todo"
 :init
 (setq ivy-todo-file "~/.emacs.d/org-files/origin.org"
       ivy-todo-guess-list nil)
 :bind ("C-c o t" . ivy-todo)
 :commands ivy-todo)

(use-package ranger
  :config
  (setq ranger-preview-file t)
  (setq ranger-max-preview-size 10))

(use-package browse-at-remote)

(use-package engine-mode
  :config
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "C-c s"))
  (defengine apidock
    "http://apidock.com/rails/search?query=%s"
    :keybinding "a")
  (defengine deepl
    "https://www.deepl.com/translator#en/es/%s"
    :keybinding "d")
  (defengine github
    "https://github.com/search?q=%s"
    :keybinding "g")
  (defengine rae
    "https://dle.rae.es/?w=%s"
    :keybinding "r")
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "s")
  (defengine mozilla
    "https://developer.mozilla.org/en-US/search?q=%s"
    :keybinding "m"))

;; Smartparens
(use-package smartparens
  :delight
  :init
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (define-key smartparens-mode-map (kbd "<M-S-backspace>") 'sp-unwrap-sexp))

;; Latex
(use-package tex
  :ensure auctex)

;; Web-mode
(use-package web-mode
  :mode "\\.ejs\\'"
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indent-style 2))

;; Coffee-mode
(use-package coffee-mode
  :init
  (setq coffee-tab-width 2))

;; Elixir
(use-package elixir-mode)

;; Racket
(use-package racket-mode
  :config
  ;; Where homebrew installed racket.
  (setq racket-program "/usr/local/bin/racket"))

;; Ruby
(use-package ruby-mode
  :init
  (setq ruby-insert-encoding-magic-comment nil))

(use-package rbenv
  :config
  (rbenv-use-global)
  (add-hook 'ruby-mode-hook
            (lambda  () (rbenv-use-corresponding))))

(use-package rubocop
  :hook ((ruby-mode . rubocop-mode)))

(use-package rspec-mode
  :config
  (setq rspec-use-spring-when-possible nil))

(use-package aggressive-indent
  :hook ((ruby-mode . aggressive-indent-mode)))

(use-package origami
  :config
  (global-origami-mode)
  :bind (("C-c f" . origami-toggle-node)))

(use-package flycheck
  :delight
  :init
  (global-flycheck-mode))

(use-package ag)
(use-package markdown-mode)
(use-package css-mode
  :config
  (setq css-indent-offset 2))
(use-package sass-mode)
(use-package yaml-mode)
(use-package indent-tools
  :bind (("C-c >" . indent-tools-hydra/body)))

;; Bundler
(use-package bundler)

;; Restclient
(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode)))

(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode))

;; Do not use
;; (use-package eyebrowse
;;   :init
;;   (setq eyebrowse-keymap-prefix (kbd "C-c w"))
;;   :config
;;   (eyebrowse-mode))

;; Checkout
;; eyebrowes (windows) -- try don't use
;; yuya373/emacs-slack

(use-package buffer-move
  :bind (("C-c w" . rae-buffer-move-hydra/body))
  :config
  (with-eval-after-load 'hydra
    (defhydra rae-buffer-move-hydra (:columns 2)
      ("h" buf-move-left "left")
      ("j" buf-move-down "down")
      ("k" buf-move-up "up")
      ("l" buf-move-right "rigth")
      ("q" nil "Quit"))))

;; Typing test for emacs
(use-package typit)

;; Org-mode
(use-package org
  :init
  (setq org-log-done 'time
        org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M:%S>")
        org-fontify-done-headline t
        org-clock-persistance 'history)
  :config
  (setq org-agenda-files (list "~/.emacs.d/org-files/origin.org"))
  (setq org-capture-templates
        '(("e" "Emacs" entry
           (file+headline "~/.emacs.d/org-files/origin.org" "Emacs")
           "* %^{prompt} %T \n %?")
           ("j" "Jetty" entry
            (file+headline "~/.emacs.d/org-files/origin.org" "Jetty")
            "* TODO %^{task} %T %^g \n %?")
           ("i" "Ideas" entry
            (file+headline "~/.emacs.d/org-files/origin.org" "Ideas")
            "* TODO %^{idea} %T %^g \n %?")
           ("t" "Continuations" entry
            (file "~/Documents/coroutines-continuations/continuations.org")
            "* %^{entry} %^g \n %?" :kill-buffer t)))
  ;; Strike through DONE tasks
  (custom-set-faces
   '(org-done
     ((t (:strike-through t))))
   '(org-headline-done
     ((t (:strike-through t)))))

  (setq org-ellipsis "⤵")
  (define-key org-mode-map (kbd "M-p") 'org-move-subtree-up)
  (define-key org-mode-map (kbd "M-n") 'org-move-subtree-down)

  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)))
;; Elfeed
(use-package elfeed
  :config
  (setq-default elfeed-search-filter "+unread @2-weeks-ago")
  (defun elfeed-start ()
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (delete-other-windows)
    (elfeed-search-update--force)))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(use-package elfeed-goodies
  :after (elfeed)
  :config
  (elfeed-goodies/setup)
  (setq elfeed-goodies/entry-pane-position 'bottom
        elfeed-goodies/entry-pane-size 0.65))

;; Ivy todo
(use-package ivy-todo
 :load-path "~/.emacs.d/git/ivy-todo"
 :init
 (setq ivy-todo-file "~/.emacs.d/org-files/origin.org"
       ivy-todo-guess-list nil)
 :bind ("C-c o t" . ivy-todo)
 :commands ivy-todo)

(use-package multiple-cursors
  :bind (("M-0" . mc/rae-hydra/body))
  :config
  (with-eval-after-load 'hydra
    (defhydra mc/rae-hydra (:columns 2)
      ("n" mc/mark-next-like-this "next")
      ("p" mc/mark-previous-like-this "previous")
      ("s" mc/skip-to-next-like-this "skip")
      ("a" mc/mark-all-like-this "all")
      ("q" nil "Quit"))))

(use-package hydra
  :config
  ;; Hydras
  ;; Taken from abo-abo
  (global-set-key (kbd "M-m")
                  (defhydra hydra-move
                    (:body-pre (forward-line)
                               :columns 4)
                    "Move vim comands"
                    ("a" beginning-of-line "Beginning")
                    ("e" move-end-of-line "End")
                    ("w" forward-word "Forward word")
                    ("W" forward-sexp "Forward sexp")
                    ("b" backward-word "Backward word")
                    ("B" backward-sexp "Backward sexp")
                    ("dd" kill-whole-line "Kill whole line")
                    ("h" backward-char "Backward")
                    ("l" forward-char "Forward char")
                    ("j" forward-line "Forward line")
                    ("k" previous-line "Backward line")
                    ("{" backward-paragraph "Backward paragraph")
                    ("}" forward-paragraph "Forward paragraph")
                    ("g" avy-goto-char "Go to char" :bind nil)
                    ("c" recenter-top-bottom "Center")
                    ("m" set-mark-command "mark" :bind nil)
                    ("q" nil "Quit")))
  (global-set-key (kbd "C-c g")
                  (defhydra hydra-go (:colums 2)
                    "Go to"
                    ("l" goto-line "Line")
                    ("u" browse-url "Url")
                    ("r" browse-at-remote "Remote")
                    ("q" nil "Quit" :color blue)))
  (defhydra hydra-zoom (global-map "M-+")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out")))

;;Theme
(load-theme 'tango-dark t)

;; Mode line
;; Mode line setup
;; Based on: http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(setq-default
 mode-line-format
 '(" %n "
   mode-line-client
   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 20))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ;; Position, including warning for 80 columns
   " "
   (:propertize "%3l:" face mode-line-position-face)
   (:propertize "%2c" face mode-line-position-face)
   " "
   ;; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize "ro" 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize "**" 'face 'mode-line-modified-face))
          (t "  ")))
   " "
   (:propertize mode-name
                face mode-line-mode-face)
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show `DIR' name shorten to `MAX-LENGTH' characters."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)

(set-face-background 'fringe "gray20")
(set-face-attribute 'mode-line nil
                    :foreground "gray60" :background "gray20"
                    :inverse-video nil
                    :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :foreground "gray80" :background "gray40"
                    :inverse-video nil
                    :box '(:line-width 1 :color "gray40" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#4271ae"
                    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#c82829"
                    :background "gray20"
                    :box '(:line-width 1 :color "gray20"))
(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-face
                    :foreground "gray50")
(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :foreground "#b8c12b"
                    :weight 'bold)
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "gray70"
                    :height 80)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :foreground "#718c00")

;; Set actions for startup
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(provide 'init)
;;; init.el ends here
