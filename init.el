;;; init.el -- Config file using el-get
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; Custom Customizations
(setq custom-file (expand-file-name "emacs-customizations.el"
                                    user-emacs-directory))

;; Packages config using el-get-bundle macro

;; Ivy / Swiper
(el-get-bundle swiper
  :features ivy
  :after
  (ivy-mode 1)
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer))

;; Smex
(el-get-bundle! smex
  :name "smex"
  :type github
  :pkgname "abo-abo/smex"
  :before (setq smex-completion-method 'ivy)
  (global-set-key (kbd "M-x") 'smex))

;; Magit
(el-get-bundle magit
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
    (jump-to-register :magit-fullscreen))
  (global-set-key (kbd "M-9") 'magit-status))

;; Avy jump
(el-get-bundle avy
  (global-set-key (kbd "C-0") 'avy-goto-word-or-subword-1)
  (global-set-key (kbd "C-c 0") 'avy-goto-char)
  (global-set-key (kbd "C-c l") 'avy-goto-line))

;; Expand region
(el-get-bundle expand-region
  (global-set-key (kbd "C-=") 'er/expand-region))

;; Multiple cursors
(el-get-bundle! multiple-cursors
  (global-set-key (kbd "M-n") 'mc/mark-next-like-this))

;; Yasnippets

;; Hydra
(el-get-bundle hydra)

;; Projectile
(el-get-bundle projectile
  :before (setq projectile-completion-system 'ivy)
  (projectile-mode))

;; Projectile Rails
(el-get-bundle projectile-rails
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

;; Company
(el-get-bundle company-mode
  (global-company-mode))

;; Flycheck
(el-get-bundle flycheck
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Web-mode
(el-get-bundle! web-mode
  :before (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                webm-mode-indent-style 2)
  :after
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode)))

;; Coffee-mode
(el-get-bundle coffee-mode
  :before (setq coffee-tab-width 2))

;; Ruby
(el-get-bundle ruby-mode
  :before (setq ruby-insert-encoding-magic-comment nil))

;; Ruby electric mode
(el-get-bundle ruby-electric
  :type svn
  :url "http://svn.ruby-lang.org/repos/ruby/trunk/misc/"
  :post-init (eval-after-load "ruby-mode"
               '(add-hook 'ruby-mode-hook 'ruby-electric-mode)))

;; Move text
(el-get-bundle move-text
  (move-text-default-bindings))

;; Undo tree
(el-get-bundle! undo-tree
  (global-undo-tree-mode))

;; Packages without config
(el-get 'sync '(ag haml-mode yaml-mode sass-mode git-timemachine))

;; Electric pair mode
(electric-pair-mode 1)

;; Checkout
;; mrkkrp/typit
;; yuya373/emacs-slack

;;; Personal config
; Disable startup message
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
(global-set-key (kbd "M-D") 'delete-horizontal-space)

;; Previous window
(defun rae-previous-window ()
  "Call `other-window with negative value."
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x p") 'rae-previous-window)


;;; Toggle comment lines
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

;;; Keyboard and locale
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

;; Theme
(load-theme 'tango-dark t)

(provide 'init)
;;; init.el ends here
