;;; init.el -- Config file using use-package
;;; Commentary:
;;; Packages once tryed and do no use anymore
;;; eyebrowes (windows)
;;; Dimmer focus current Buffer
;;;
;;; Packege to try
;;; yuya373/emacs-slack
;;; https://github.com/raxod502/prescient.el
;;; https://github.com/Silex/docker.el
;;; https://github.com/raxod502/straight.el
;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
;; use-package
(setq use-package-always-ensure t)
(use-package delight)
(use-package dash)

;; Custom Customizations
(setq custom-file (make-temp-file "emacs-customizations.el"))
(load "~/.emacs.d/secrets.el")

;; Some hooks
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (defvar sql-prompt-regexp "^[_[:alpha:]]*[=][#>] ")
            (defvar sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] ")))

;; Personal config
;; Disable startup message
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)

;; Turn off alarms
(setq ring-bell-function 'ignore)
;; Typed text delete selection
(pending-delete-mode 1)
;; Indent with two spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
;; Do not split window vertically
(setq-default split-height-threshold nil)
(setq-default split-width-threshold 80)

;; Backups
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 delete-old-versions t
 version-control t)

;; Change all prompts to y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Winner mode
;; Restore window with C-c <left> (undo C-c <right>)
;; (winner-mode 1)

;; Join lines
(global-set-key (kbd "M-J") 'join-line)

(defun rae-kill-current-file ()
  "Kill current file if any and kill current buffer."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (when current-file
      (save-buffer current-file)
      (delete-file current-file))
    (kill-buffer (current-buffer))))

;; Delete whitespaces
(defun rae-kill-word-or-whitespaces (arg)
  "Kill a word (`ARG')or remove whitespaces."
  (interactive "p")
  (let ((next-char (following-char)))
    (cond
     ((eql 32 next-char) (delete-horizontal-space))
     ('else (kill-word arg)))))
(global-set-key (kbd "M-D") 'rae-kill-word-or-whitespaces)

;; Unbindings
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "M-i"))
(global-unset-key (kbd "M-q"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "s-h"))
(global-unset-key (kbd "s-p"))

;; Previous window
(defun rae-previous-window ()
  "Call `other-window with negative value."
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x p") 'rae-previous-window)

;; Previous frame
(defun rae-previous-frame ()
  "Call `other-frame with negative value."
  (interactive)
  (other-frame -1))

;; Frames
(global-set-key (kbd "C-c N") 'make-frame-command)
(global-set-key (kbd "C-c X") 'delete-frame)
(global-set-key (kbd "C-x O") 'other-frame)
(global-set-key (kbd "C-x P") 'rae-previous-frame)

;; Auto-resize window
(defadvice other-window (after rae-resize-window activate)
  "Enlarge window to 80 columns buffer."
  (let ((delta (- 80 (window-width))))
    (enlarge-window delta t)))

;; Move line (region sprex??) up
(defun rae-up-to-next-line ()
  "Move up to next line."
  (interactive)
  (kill-line)
  (forward-line -1)
  (end-of-line)
  (yank))
(global-set-key (kbd "C-c u") 'rae-up-to-next-line)

;; Toggle comment lines
(defun rae-toggle-comment-on-line ()
  "Comment and uncomment lines."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-end-position))))
(global-unset-key (kbd "M-/"))
(global-set-key (kbd "M-/") 'rae-toggle-comment-on-line)

;; Trailing whitespaces and save safetly taken from:
;; http://whattheemacsd.com/buffer-defuns.el-01.html
(defun rae-cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a 'before-save-hook', and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))
;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'rae-cleanup-buffer-safe)

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
          (setq beg (save-excursion (goto-char (mark))
                                    (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark))
                                  (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))
(global-set-key (kbd "C-c C-k") 'copy-line)

;; Reload file
(defun rae-reload-file ()
  "Reload current file by reverting current buffer."
  (interactive)
  (revert-buffer t t t)
  (message "Reload %s"
           (buffer-file-name (current-buffer))))
(global-unset-key (kbd "M-R"))
(global-set-key (kbd "M-R") 'rae-reload-file)

;; Smooth scroll
(setq scroll-conservatively 101)
(setq scroll-margin 5)

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
(set-frame-font "Hack-15" nil t)

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
         (set-frame-parameter (selected-frame) 'alpha value))
   "Set to ?" :color blue))
(global-set-key (kbd "M-1") 'hydra-transparency/body)

;; Packages
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

(use-package eshell
  :bind (("C-c e". eshell))
  :init
  (defvar eshell-mv-interactive-query t)
  (defvar eshell-cp-interactive-query t)
  (defvar eshell-rm-interactive-query t)
  :config
  (defadvice eshell (around eshell-fullscreen activate)
    (window-configuration-to-register :eshell-fullscreen)
    ad-do-it
    (delete-other-windows)))

;; Autocomplete packages for eshell.
(use-package pcmpl-homebrew
  :after eshell
  :config
  ;; set hombrew imporant!
  (setenv "HOMEBREW_NO_INSECURE_REDIRECT" "1")
  (setenv "HOMEBREW_NO_ANALYTICS" "1")
  (setenv "HOMEBREW_CASK_OPTS" "--require-sha"))
(use-package pcmpl-args :after eshell)

(use-package eshell-git-prompt
  :config (eshell-git-prompt-use-theme 'git-radar))

;; Packages config using el-get-bundle macro
;; Avy jump
(use-package avy
  :bind (("C-0" . avy-goto-char-timer)
         ("C-c 0" . avy-goto-word-or-subword-1)
         ("C-c l" . avy-goto-line)))

;; Beacon - Highlight cursor
(use-package beacon
  :init
  (setq beacon-blink-duration 0.5)
  (setq beacon-size 80)
  (setq beacon-color "#0a84ff")
  :config
  (beacon-mode))

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
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop) :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :config
  (ivy-mode t))

(use-package ivy-posframe
  :after (perspective)
  :config
  ;; Different command can use different display function.
  (setq ivy-posframe-height-alist '((counsel-M-x . 20)
                                    (t           . 10)))
  ;; Different command can use different display function.
  (setq ivy-posframe-display-functions-alist
        '((swiper . nil)
          (counsel-find-file . nil)
          (ivy-switch-buffer . nil)
          (complete-symbol . ivy-posframe-display-at-point)
          ;; (ivy-switch-buffer . ivy-posframe-display-at-frame-bottom-left)
          (t . ivy-posframe-display-at-frame-top-center)))

  ;; Hack: set posframe parameters after emacs startup.
  ;; Wait for persp-init current frame to persp parameters get value.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq ivy-posframe-parameters
                    (list (cons 'alpha 85)
                          (cons 'persp--hash (perspectives-hash))
                          (cons 'persp--curr (persp-curr))
                          (cons 'persp--last (persp-last))))))
  (setq ivy-posframe-border-width 10)
  (ivy-posframe-mode 1))

(use-package helpful
  :bind
  (("C-h f" . #'helpful-callable)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)
   ("C-c C-." . #'helpful-at-point)))

(use-package smex)

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

  (defadvice magit-status-setup-buffer (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))

(use-package forge :after magit)
(use-package github-review :after forge)
(use-package git-timemachine)
(use-package gitconfig-mode)
(use-package haml-mode)
(use-package swift-mode)

;; Move text
(use-package move-text
  :bind (("M-P" . move-text-up)
         ("M-N" . move-text-down)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package ag)

;; Projectile
(use-package projectile
  :delight
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-switch-project-action
        (lambda ()
          (let ((git-dir (expand-file-name ".git" default-directory)))
            (when (file-exists-p git-dir)
              (magit-status-setup-buffer default-directory))
            (projectile-find-file ivy-current-prefix-arg)))))

(use-package counsel-projectile
  :config
  (setq counsel-projectile-org-capture-templates
        '(("f" "Todo for ${name}" entry
           (file+headline "~/.emacs.d/org-files/projects.org" "${name}")
           "* TODO %^{todo} %^g \n %T \n %?")
          ("i" "Todo in ${name}" entry
           (file+headline "${root}/todo.org" "${name}")
           "* TODO %^{todo} %^g \n %T \n %?")))
  (counsel-projectile-mode))

(use-package perspective
  :config
  (set-face-foreground 'persp-selected-face "#729fcf")

  (defface persp-non-selected-face
    '((t (:height 70 :foreground "gray70")))
    "The face used for non current prespectives on the modeline."
    :group 'perspective-mode)

  (defun persp-format-name (name)
    "Format the perspective name given by NAME for display in the modeline."
    (let ((string-name (format "%s" name)))
      (if (equal name (persp-name (persp-curr)))
          (propertize string-name 'face 'persp-selected-face)
        (propertize string-name
                    'face 'persp-non-selected-face
                    'local-map persp-mode-line-map
                    'mouse-face 'mode-line-highlight))))
  (persp-mode))

(use-package persp-projectile
  :after (counsel-projectile perspective)
  :bind (:map projectile-mode-map
              ("C-c p p" . projectile-persp-switch-project)))

(use-package projectile-rails
  :config
  (projectile-rails-global-mode)
  (with-eval-after-load 'rake
    (setq rake-completion-system 'ivy-read)))

(use-package counsel-dash)

;; Ivy todo
(use-package ivy-todo
  :load-path "~/.emacs.d/git/ivy-todo"
  :init
  (setq ivy-todo-file "~/.emacs.d/org-files/projects.org"
        ivy-todo-guess-list nil)
  :bind ("C-c o t" . ivy-todo)
  :commands ivy-todo)

(use-package ranger
  :config
  (setq ranger-preview-file t)
  (setq ranger-max-preview-size 10))

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
  :hook ((prog-mode . smartparens-mode))
  :init
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  :config
  (require 'smartparens-config)
  (global-unset-key (kbd "M-}"))
  (global-unset-key (kbd "M-{"))
  (define-key smartparens-mode-map (kbd "<M-S-backspace>") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "M-p") 'backward-paragraph)
  (define-key smartparens-mode-map (kbd "M-n") 'forward-paragraph)
  (define-key smartparens-mode-map (kbd "s-p s") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "s-p b") 'sp-forward-barf-sexp))

;; Latex
(use-package tex
  :ensure auctex)

;; Config js
(with-eval-after-load 'js-mode (setq js-indent-level 2))
;; Web-mode
(use-package web-mode
  :mode "\\.ejs\\'"
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indent-style 2))
;; JSON- mode
(use-package json
  :config
  (defvar json-reformat:indent-width 2))

;; Coffee-mode
(use-package coffee-mode
  :init
  (setq coffee-tab-width 2))

;; Node
;; https://github.com/mojochao/npm-mode#project-activation
;; https://www.emacswiki.org/emacs/DirectoryVariables
(use-package npm-mode)

;; Elixir
(use-package alchemist)


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
  :hook
  ((ruby-mode . rbenv-use-corresponding)))

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
  (setq org-log-done 'time)
  (setq org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M:%S>"))
  (setq org-fontify-done-headline t)
  (defvar org-clock-persistance 'history)
  :config
  (setq org-agenda-files (list "~/.emacs.d/org-files/origin.org"
                               "~/.emacs.d/org-files/projects.org"))
  (defvar org-capture-templates
    '(("c" "General [c]apture" entry
       (file+headline "~/.emacs.d/org-files/origin.org" "Ideas")
       "* TODO %^{idea} %T %^g \n %?")))

  ;; Strike through DONE tasks
  (custom-set-faces
   '(org-done ((t (:strike-through t))))
   '(org-headline-done ((t (:strike-through t)))))

  (setq org-ellipsis "⤵")
  (define-key org-mode-map (kbd "M-p") 'org-move-subtree-up)
  (define-key org-mode-map (kbd "M-n") 'org-move-subtree-down))

;; Elfeed
(use-package elfeed
  :config
  (setq-default elfeed-search-filter "+unread @2-weeks-ago")
  (defun elfeed-start ()
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-update))
    (delete-other-windows)
  (define-key global-map (kbd "C-c F") 'elfeed-start))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(use-package elfeed-goodies
  :after (elfeed)
  :config
  (elfeed-goodies/setup)
  (setq elfeed-goodies/entry-pane-position 'bottom)
  (setq elfeed-goodies/entry-pane-size 0.65)
  (define-key elfeed-show-mode-map "j" 'next-line)
  (define-key elfeed-show-mode-map "k" 'previous-line))

;; Pocket
(use-package pocket-reader
  :bind (("C-c R" . pocket-reader)))

;; Twitter
(use-package twittering-mode)

;; Ivy todo
(use-package ivy-todo
  :load-path "~/.emacs.d/git/ivy-todo"
  :init
  (setq ivy-todo-file "~/.emacs.d/org-files/projects.org"
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

(use-package dumb-jump
  :after hydra
  :config
  (defvar dumb-jump-selector 'ivy)
  ;; https://github.com/jacktasia/dumb-jump#hydra-for-effieciency
  (global-set-key
   (kbd "M-g")
   (defhydra dumb-jump-hydra ( :color blue :columns 3)
     "Go to -"
     ("SPC" dumb-jump-go-prompt "Prompt")
     ("b" dumb-jump-back "Back")
     ("e" dumb-jump-go-prefer-external "Go external")
     ("g" dumb-jump-go-prefer-external-other-window "Go external other window")
     ("l" dumb-jump-quick-look "Quick look")
     ("o" dumb-jump-go-other-window "Other window")
     ("q" nil "Quit" :color blue)
     ("u" browse-url "Url")
     ("j" dumb-jump-go "Go"))))

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
                    ("j" next-line "Forward line")
                    ("k" previous-line "Backward line")
                    ("{" backward-paragraph "Backward paragraph")
                    ("}" forward-paragraph "Forward paragraph")
                    ("g" avy-goto-char "Go to char" :bind nil)
                    ("c" recenter-top-bottom "Center")
                    ("m" set-mark-command "mark" :bind nil)
                    ("q" nil "Quit")))
  (defhydra hydra-zoom (global-map "M-+")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

;;Theme
(use-package tangotango-theme)

;; Mode line
(use-package all-the-icons)

(use-package hide-mode-line
  :load-path "~/.emacs.d/git/emacs-hide-mode-line"
  :hook ((eshell-mode . hide-mode-line-mode))
  :config
  (global-set-key (kbd "C-c h") 'hide-mode-line-mode))

;; Helper function
(defun shorten-directory (dir max-length &optional prefix)
  "Show `DIR' name shorten to `MAX-LENGTH' characters.
When `PREFIX' is given append prefix to result, default prefix is ..."
  (let* ((path (abbreviate-file-name dir))
         (path-length (length path))
         (prefix (or prefix "..."))
         (prefix-length (length prefix)))
    (if (< path-length max-length)
        path
      (concat prefix
              (substring path (- path-length
                                 (- max-length prefix-length)))))))

;; Custome mode-line faces

(set-face-attribute 'mode-line nil
                    :box "#0a84ff")

(set-face-attribute 'mode-line-buffer-id nil
                    :inherit 'mode-line
                    :slant 'italic
                    :foreground "OliveDrab3"
                    :weight 'bold)
(set-face-attribute 'mode-line-buffer-id-inactive nil
                    :inherit 'mode-line-inactive
                    :weight 'bold)

(make-face 'mode-line-folder)
(set-face-attribute 'mode-line-folder nil
                    :width 'condensed
                    :height 0.8)

(make-face 'mode-line-flycheck)
(set-face-attribute 'mode-line-flycheck nil
                    :inherit 'flycheck-fringe-error
                    :height 0.6)

(make-face 'mode-line-position-face)
(set-face-attribute 'mode-line-position-face nil
                    :slant 'italic
                    :height 0.9
                    :weight 'normal)

(make-face 'mode-line-minor-mode-face)
(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    :height 80)

(make-face 'mode-line-process-face)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line
                    :foreground "#718c00")

;; Mode line setup
;; Based on: http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(setq-default
 mode-line-format
 '("%n"
   mode-line-client
   " "
   ;; read-only or modified status
   (:eval
    (cond
     (buffer-read-only
      (all-the-icons-fileicon "emacs"
                              :face 'all-the-icons-blue))
     ((buffer-modified-p)
      (all-the-icons-faicon "exclamation-triangle"
                            :face 'all-the-icons-red))
     (t (all-the-icons-faicon "thumbs-up"
                              :face 'all-the-icons-green))))
   " "
   ;; Position
   (:propertize "%3l:%2c " face mode-line-position-face)
   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 20))
                face mode-line-folder)
   "%b "
   (:eval (all-the-icons-icon-for-mode major-mode))
   " "
   (:eval (persp-mode-line))
   " "
   (:propertize mode-line-process
                face mode-line-process-face)
   " "
   (:propertize (flycheck-mode flycheck-mode-line)
                face mode-line-flycheck)))


;; Set actions for startup
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(provide 'init)
;;; init.el ends here
