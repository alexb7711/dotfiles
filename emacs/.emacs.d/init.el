;;==============================================================================;;
;;  _   _ _____ ____  _____ _____ ___ ____      _____ __  __    _    ____ ____  ;;
;; | | | | ____|  _ \| ____|_   _|_ _/ ___|    | ____|  \/  |  / \  / ___/ ___| ;;
;; | |_| |  _| | |_) |  _|   | |  | | |   _____|  _| | |\/| | / _ \| |   \___ \ ;;
;; |  _  | |___|  _ <| |___  | |  | | |__|_____| |___| |  | |/ ___ \ |___ ___) |;;
;; |_| |_|_____|_| \_\_____| |_| |___\____|    |_____|_|  |_/_/   \_\____|____/ ;;
;;==============================================================================;;

;;------------------------------------------------------------------------------;;
;;  ___  _   ___ _  __   _   ___ ___                                            ;;
;; | _ \/_\ / __| |/ /  /_\ / __| __|                                           ;;
;; |  _/ _ \ (__| ' <  / _ \ (_ | _|                                            ;;
;; |_|/_/ \_\___|_|\_\/_/ \_\___|___|                                           ;;
;;------------------------------------------------------------------------------;;

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			  ("org"   . "https://orgmode.org/elpa/")
			  ("elpa"   . "https://elpa.gnu.org/packages/")))
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
   '(doom-themes helpful ivy-rich which-key rainbow-delimiters counsel doom-modeline ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Improved key bindings and allows for leader keys (SPACE)
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer rune/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC"))

;;------------------------------------------------------------------------------;;
;; __   _____ __  __     ___ __  __ _   _ _      _ _____ ___ ___  _  _          ;;
;; \ \ / /_ _|  \/  |___| __|  \/  | | | | |    /_\_   _|_ _/ _ \| \| |         ;;
;;  \ V / | || |\/| |___| _|| |\/| | |_| | |__ / _ \| |  | | (_) | .` |         ;;
;;   \_/ |___|_|  |_|   |___|_|  |_|\___/|____/_/ \_\_| |___\___/|_|\_|         ;;
;;------------------------------------------------------------------------------;;

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'evil-normal-state)

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

;;------------------------------------------------------------------------------;;
;;    _   ___ _____ _  _ ___ _____ _ _____ ___ ___ ___                          ;;
;;   /_\ | __|_   _| || / __|_   _/_\_   _|_ _/ __/ __|                         ;;
;;  / _ \| _|  | | | __ \__ \ | |/ _ \| |  | | (__\__ \                         ;;
;; /_/ \_\___| |_| |_||_|___/ |_/_/ \_\_| |___\___|___/                         ;;
;;------------------------------------------------------------------------------;;

;; DEFAULTS
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(setq visible-bell t)

;; A lot of themes
(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

;; ALPHA
(set-frame-parameter (selected-frame) 'alpha '(95 . 80))

;; DOOM MODELINE
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; FONT
(set-face-attribute 'default nil :font "Hurmit Nerd Font Mono" :height 110)

;; LINE NUMBERS
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; RAINBOW DELIMITERS
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;------------------------------------------------------------------------------;;
;;    _  _   _ _____ ___       ___ ___  __  __ ___ _    ___ _____ ___ ___  _  _ ;;
;;   /_\| | | |_   _/ _ \ ___ / __/ _ \|  \/  | _ \ |  | __|_   _|_ _/ _ \| \| |;;
;;  / _ \ |_| | | || (_) |___| (_| (_) | |\/| |  _/ |__| _|  | |  | | (_) | .` |;;
;; /_/ \_\___/  |_| \___/     \___\___/|_|  |_|_| |____|___| |_| |___\___/|_|\_|;;
;;------------------------------------------------------------------------------;;

;; Auto Completion
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

;; Auto completion made nicer
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; EMACS Ivy-enhanced commands
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	  ("C-x b" . counsel-ibuffer)
	  ("C-x C-f" . counsel-find-file)
	  :map minibuffer-local-map
	  ("C-r" . 'counsel-minibuffer-history))
  :config
	(setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;;------------------------------------------------------------------------------;;
;;  ___ _   _ _  _  ___ _____ ___ ___  _  _   _   _    ___ _______   __         ;;
;; | __| | | | \| |/ __|_   _|_ _/ _ \| \| | /_\ | |  |_ _|_   _\ \ / /         ;;
;; | _|| |_| | .` | (__  | |  | | (_) | .` |/ _ \| |__ | |  | |  \ V /          ;;
;; |_|  \___/|_|\_|\___| |_| |___\___/|_|\_/_/ \_\____|___| |_|   |_|           ;;
;;------------------------------------------------------------------------------;;

;; Auto Compile
(add-hook 'after-save-hook 'compiler-script)

;; Augemtned help
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

;; Cleanup whitespace
(add-hook 'before-save-hook' 'delete-trailing-whitespace)

;; Give information when use meta key strokes
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

;; Open current buffer in zathura 
(global-set-key (kbd "C-c z") 'open-in-zathura)

;;------------------------------------------------------------------------------;;
;;  ___ _   _ _  _  ___ _____ ___ ___  _  _ ___                                 ;;
;; | __| | | | \| |/ __|_   _|_ _/ _ \| \| / __|                                ;;
;; | _|| |_| | .` | (__  | |  | | (_) | .` \__ \                                ;;
;; |_|  \___/|_|\_|\___| |_| |___\___/|_|\_|___/                                ;;
;;------------------------------------------------------------------------------;;

;; Compile script
(defun compiler-script ()
  "Run compile command on currently opened buffer"
  (princ (buffer-file-name))
  (call-process-shell-command (concat "compile " (buffer-file-name)) nil 0))

;; Open current buffer and replace basename with *.pdf extention and open
;; in zathura
(defun open-in-zathura ()
  "Open current buffer with zathura"
  (interactive)
  (call-process-shell-command (concat "zathura " (file-name-base) ".pdf&") nil 0))

;;------------------------------------------------------------------------------;;
;;  ___   ___   ___ _   _ __  __ ___ _  _ _____                                 ;;
;; |   \ / _ \ / __| | | |  \/  | __| \| |_   _|                                ;;
;; | |) | (_) | (__| |_| | |\/| | _|| .` | | |                                  ;;
;; |___/ \___/ \___|\___/|_|  |_|___|_|\_| |_|                                  ;;
;;------------------------------------------------------------------------------;;


;;------------------------------------------------------------------------------;;
;; __      _____ _  _ ___   _____      __                                       ;;
;; \ \    / /_ _| \| |   \ / _ \ \    / /                                       ;;
;;  \ \/\/ / | || .` | |) | (_) \ \/\/ /                                        ;;
;;   \_/\_/ |___|_|\_|___/ \___/ \_/\_/                                         ;;
;;                                                                              ;;
;;  __  __   _   _  _   _   ___ ___ __  __ ___ _  _ _____                       ;;
;; |  \/  | /_\ | \| | /_\ / __| __|  \/  | __| \| |_   _|                      ;;
;; | |\/| |/ _ \| .` |/ _ \ (_ | _|| |\/| | _|| .` | | |                        ;
;; |_|  |_/_/ \_\_|\_/_/ \_\___|___|_|  |_|___|_|\_| |_|                        ;;
;;------------------------------------------------------------------------------;;

