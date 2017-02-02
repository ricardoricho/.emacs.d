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
(setq use-package-always-ensure t)
(require 'use-package)
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant


;; Custom Customizations
(setq custom-file (expand-file-name "emacs-customizations.el"
                                    user-emacs-directory))

;; Packages config using el-get-bundle macro
;; Avy jump
(use-package avy
  :bind (("C-0" . avy-goto-word-or-subword-1)
         ("C-c 0" . avy-goto-char)
         ("C-c l" . avy-goto-line)))

;; Company
(use-package company
  :diminish company-mode
  :config
  (global-company-mode))

(use-package dash)

;; Swiper ivy-mode
(use-package swiper
  :load-path "~/.emacs.d/git/swiper"
  :init
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-s" . swiper)
         ("M-Y" . counsel-yank-pop))
  :config
  (ivy-mode t)
  (diminish ivy-mode))

;; Smex (using abo-abo github repo)
(use-package smex
  :bind (("M-x" . smex))
  :load-path "~/.emacs.d/git/smex/"
  :init
  (setq smex-completion-method 'ivy))

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

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package git-timemachine)
(use-package haml-mode
  :init
  (setq indet-tabs-mode nil)
  :bind (("C-m" . newline-and-indent)))

(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (require 'yasnippet)
  (yas-global-mode 1))

;; Move text
(use-package move-text
  :config
  (move-text-default-bindings))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; Projectile
(use-package projectile
  :init
  (setq projectile-completion-system 'ivy
        projectile-mode-line '(:eval
                               (format " [%s]" (projectile-project-name))))
  :config
  (projectile-mode))

(use-package projectile-rails
  :config
  (projectile-rails-global-mode))

;; Smartparens
(use-package smartparens
  :init
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode))

;; Web-mode
(use-package web-mode
  :mode "\\.ejs\\'"
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        webm-mode-indent-style 2))

;; Coffee-mode
(use-package coffee-mode
  :init
  (setq coffee-tab-width 2))

;; Ruby
(use-package ruby-mode
  :mode "\\.rb\\'"
  :init
  (setq ruby-insert-encoding-magic-comment nil))

(use-package rvm
  :config
  (rvm-use-default)
  (add-hook 'ruby-mode-hook
            (lambda () (rvm-activate-corresponding-ruby))))

(use-package rspec-mode
  :config
  (defadvice rspec-compile (around rspec-compile-around)
    "Use BASH shell for running the specs fo ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))
  (ad-activate 'rspec-compile))

;; Ruby Bundler
;; (el-get-bundle! bundler)

;; ;; Undo tree
;; (el-get-bundle! undo-tree
;;   (global-undo-tree-mode))

(use-package ag)
(use-package sass-mode)
(use-package yaml-mode)
(use-package indent-tools
  :bind (("C-c >" . indent-tools-hydra/body)))


;; Electric pair mode
(electric-pair-mode 1)

;; Checkout
;; mrkkrp/typit
;; yuya373/emacs-slack
;; buffer-move https://www.emacswiki.org/emacs/buffer-move.el

;; ;; Org-mode
(use-package org
  :init
  (setq org-log-done 'time
        org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M:%S>")
        org-fontify-done-headline t
        org-clock-persistance 'history)
  :config
  (setq org-agenda-files (list "~/.emacs.d/org-files/work.org"
                                 "~/.emacs.d/org-files/home.org"
                                 "~/.emacs.d/emacs.org")
        org-capture-templates '(("h" "Home" entry
                                 (file+headline "~/.emacs.d/org-files/home.org"
                                                "Home")
                                 "* TODO %?\n %i\n %a")
                                ("i" "INNKU" entry
                                 (file+headline "~/.emacs.d/org-files/work.org"
                                                "INNKU")
                                 "* TODO %? %^g\n %i")
                                ("e" "Emacs" entry (file "~/.emacs.d/emacs.org")
                                 "* %?\n %i\n %a")))
  (custom-set-faces
   '(org-done
     ((t (:strike-through t))))
   '(org-headline-done
     ((t (:strike-through t)))))
  (setq org-ellipsis "⤵")

  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)))

(use-package multiple-cursors
  :bind (("M-n" . mc/rae-hydra/body))
  :config
  (with-eval-after-load 'hydra
    (defhydra mc/rae-hydra (:columns 1)
      ("n" mc/mark-next-like-this "next")
      ("p" mc/mark-previous-like-this "previous")
      ("a" mc/mark-all-like-this "all")
      ("q" nil "Quit"))))

(use-package hydra)
;; Hydras
;; Taken from abo-abo
(global-set-key (kbd "M-m")
                (defhydra hydra-move
                  (:body-pre (next-line)
                   :columns 4)
                  "move"
                  ;; Converting M-v to V here by analogy.
                  ("V" scroll-down-command "Scroll down")
                  ("a" beginning-of-line "Beginning")
                  ("b" backward-char "Backward")
                  ("e" move-end-of-line "End")
                  ("f" forward-char "Forward")
                  ("g" avy-goto-char "Go to char" :bind nil)
                  ("l" recenter-top-bottom "Center")
                  ("m" set-mark-command "mark" :bind nil)
                  ("n" next-line "Next")
                  ("p" previous-line "Previous")
                  ("v" scroll-up-command "Scroll up")
                  ("q" nil "Quit")))

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

;; Previous window
(defun rae-previous-window ()
  "Call `other-window with negative value."
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x p") 'rae-previous-window)

;; Resize window
(defun rae-resize-window ()
  "Make window to a certain size"
  (interactive)
  (let ((rae-resize (- 80 (window-width))))
    (enlarge-window rae-resize t)))
;; Automatic resize
(advice-add 'other-window :after (lambda (foo) (rae-resize-window)))

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

;; Reload file
(defun my-reload-file ()
  "Reload current file by reverting current buffer."
  (interactive)
  (revert-buffer t t t)
  (message "Reload %s"
           (buffer-file-name (current-buffer))))
(global-unset-key (kbd "M-R"))
(global-set-key (kbd "M-R") 'my-reload-file)

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

;;p Theme
(load-theme 'tango-dark t)

(provide 'init)
;; init.el ends here
