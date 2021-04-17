(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"  . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" default))
 '(package-selected-packages
   '(matlab-mode visual-fill-column visual-fill org-bullets evil-magit forge magit counsel-projectile counsel-projectil projectile hydra evil-collection doom-themes helpful ivy-rich which-key rainbow-delimiters counsel doom-modeline ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

 (use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(setq visible-bell t)

;; Default Font
(set-face-attribute 'default nil :font "Hurmit Nerd Font Mono" :height 110)

;; Set the fixed pitch font
(set-face-attribute 'fixed-pitch nil :font "Hurmit Nerd Font Mono" :height 110)

;; Set variable pitch font
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 110 :weight 'regular)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
		       vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
(add-hook mode 'disable-linum))

;; Disable line numbers on buffer
(defun disable-linum ()
  (display-line-numbers-mode 0))

(set-frame-parameter (selected-frame) 'alpha '(90 . 80))

;; RAINBOW DELIMITERS
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Auto Compile
(add-hook 'after-save-hook 'compiler-script)

;; Compile script
(defun compiler-script ()
  "Run compile command on currently opened buffer"
  (call-process-shell-command (concat "compile " (buffer-file-name)) nil 0))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(with-eval-after-load 'evil-collection
  (use-package dired
    :ensure nil
    :commands (dired dired-jump)
    :bind (("C-x C-j" . dired-jump))
    :custom ((dired-listing-switches "-agho --group-directories-first"))
    :config
    (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l"  'dired-single-buffer)))

(use-package dired-single)

(use-package all-the-icons-dired
:hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package dired-open
:config
(setq dired-open-extensions '(("png" . "feh")
                                                                     ("jpg" . "feh")
                                                                    ;; Video
                                                                    ("mkv" . "mpv")
                                                                    ("mp4" . "mpv")
                                                                    ;; PDF
                                                                    ("pdf" . "zathura"))))

;; Improved key bindings and allows for leader keys (SPACE)
(use-package general
  :config
  ;  (general-evil-setup t)
    (general-create-definer heretic/leader-keys
                            :keymaps '(normal insert visual emacs)
                            :prefix "SPC"
                            :global-prefix "C-SPC"))

(heretic/leader-keys
   "t"  '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")
   "ts" '(hydra-text-scale/body :which-key "scale text")

   "sb" 'treemacs

   "." 'dired

   "p" 'projectile-command-map

   "g" 'magit)

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key]      . helpful-key))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
"scale text"
("j" text-scale-increase "in")
("k" text-scale-decrease "out")
("f" nil "finished" :exit t))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
          ("C-x b" . counsel-ibuffer)
          ("C-x C-f" . counsel-find-file)
          :map minibuffer-local-map
          ("C-r" . 'counsel-minibuffer-history))
  :config
        (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package lsp-mode
:commands(lsp lsp-deffered)
:init
(setq lsp-keymap-prefix "C-c l")
:config
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'shell-mode-hook #'lsp)
(add-hook 'latex-mode-hook #'lsp)
(lsp-enable-which-key-integration t))

(use-package lsp-ui
:hook (lsp-mode . lsp-ui-mode)
  :custom
(setq lsp-ui-doc-position 'bottom))

(use-package lsp-ivy)

(setq lsp-clients-clangd-executable t)

(use-package lsp-latex)

(use-package matlab
  :ensure matlab-mode
  :config
  (add-to-list
   'auto-mode-alist
   '("\\.m\\'" . matlab-mode))
  (setq matlab-indent-function t)
  (setq matlab-shell-command "matlab"))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package lsp-mode
  :commands lsp
  :hook
  (sh-mode . lsp))

(use-package magit)

(use-package forge)

;; Cleanup whitespace
(add-hook 'before-save-hook' 'delete-trailing-whitespace)

;; Open current buffer in zathura 
(global-set-key (kbd "C-c z") 'open-in-zathura)

;; Open current buffer and replace basename with *.pdf extention and open
;; in zathura
(defun open-in-zathura ()
  "Open current buffer with zathura"
  (interactive)
  (call-process-shell-command (concat "zathura " (file-name-base) ".pdf&") nil 0))

(use-package org
  :hook (org-mode . heretic/org-mode-setup)
  :config
  ;; Replace '...' with down arrrow
  (setq org-ellipsis " ▾" 
   org-hide-emphasis-markers t))

;; Set up org mode
(defun heretic/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

;; Only evaluate this after org-faces has loaded
(general-with-eval-after-load 'org-faces
  ;; Set face heading sizes 
  (dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;; Set fixed-pitch fonts for org mode
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil                                      :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil                                      :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil                             :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil            :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil                            :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil                             :inherit 'fixed-pitch))

;; Execute code block languages
(org-babel-do-load-languages
 'org-babel-load-language
 '((emacs-lisp . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

;; Structure templates
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;;  Automatically tangle init.org config when saved
(defun heretic/org-babel-tangle-config ()
(when (string-equal (buffer-file-name)
                  (expand-file-name "~/Code/dotfiles/emacs/.emacs.d/init.org"))
(let ((org-confirm-babel-evaluate nil))
(org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'heretic/org-babel-tangle-config)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))  
  :init
  ;; Set paths to where you have your projects at
  (when (file-directory-p "~/Code")
    (setq projectil-project-search-path '("~/Code/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile)
:config (counsel-projectile-mode)

(use-package treemacs
      :ensure t
      :defer t)

(use-package treemacs-evil
      :after evil-collection)

(use-package lsp-treemacs
      :after lsp)

(use-package treemacs-projectile
      :after projectile)

(defun heretic/display-startup-time ()
  (interactive)
  (message "Emacs loaded in %s with %d garbage collections."
          (format "%.2f seconds"
         (float-time
         (time-subtract after-init-time before-init-time)))
         gcs-done))

(add-hook 'emacs-startup-hook #'heretic/display-startup-time)

(use-package vterm
:commands vterm
:config
(setq vterm-shell "zsh")
(setq vterm-max-scrollback 10000))

;; EVIL Mode
(use-package evil
:init
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scrill t)
(setq evil-want-C-i-jump nil)
:config
(evil-mode 1)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(evil-set-initial-state 'message-buffers-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal))

;; Better EVIL configs in other modes
(use-package evil-collection
:after evil
:config
(evil-collection-init))

(use-package visual-fill-column
  :hook (org-mode . heretic/org-mode-visual-fill))

;; Center and wrap text in org mode
(defun heretic/org-mode-visual-fill()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))
