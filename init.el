;; -*- lexical-binding: t; -*-

;; Install package manager
(defvar elpaca-installer-version 0.2)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-installer*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--"))))
               (emacs (concat invocation-directory invocation-name))
               ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                     "--eval" "(byte-recompile-directory \".\" 0 'force)"))))
          (progn (require 'elpaca)
                 (elpaca-generate-autoloads "elpaca" repo)
                 (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error) (warn "%s" err) (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;Utilities to debug startup time
(setq use-package-compute-statistics t)
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

;; Set up general - a keybinding manager 
(use-package general
  :demand t
  :config
  (general-override-mode)
  (general-auto-unbind-keys)
  (general-define-key
   :keymaps 'override
   :states '(insert normal hybrid motion visual operator emacs)
   :prefix-map '+prefix-map
   :prefix "SPC"
   :global-prefix "S-SPC")
  (general-create-definer global-definer
    :wk-full-keys nil
    :keymaps '+prefix-map)
  (global-definer
    "!"   'shell-command
    ":"   'eval-expression
    "."   'repeat
  )
  (general-define-key
     :keymaps 'override
     :states '(normal hybrid motion visual operator emacs)
     "-" 'dired-jump))
(elpaca-wait)

;; Add some default configuration to emacs
(use-package emacs
  :elpaca nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Store recent files to allow c-o c-f to work
  (recentf-mode 1)
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 100)

  ;; Disable automatic documentation/signature help popup in minibuffer
  ;; TODO: eldoc-box does not work in terminal, and we can't set this per client
  ;; If you always want popups in the minibuffer, remove these lines
  (if (display-graphic-p)
    (global-eldoc-mode -1))

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Basic customization
  ;; Remove tool and menu bar
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  ;; Preserve cursor position on screen
  (setq scroll-preserve-screen-position 'always)

  ;; Display line numbers
  (global-display-line-numbers-mode)

  ;; Disable line numbers for certain modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

  ;; Custom error message
  (defun display-startup-echo-area-message () (message ()))

  ;; Suppress startup message
  ;; TODO: does not work
  (setq inhibit-startup-echo-area-message user-login-name)

  ;; Associate rust files with rust-ts-mode
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

  ;; Enable mouse mode in terminal
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package flymake
  :elpaca nil
  :general
  (general-define-key
     :keymaps 'override
     :states '(normal hybrid motion visual operator emacs)
    "]d" 'flymake-goto-next-error
    "[d" 'flymake-goto-prev-error
    )
  (global-definer
    "db"   'flymake-show-buffer-diagnostics
    "dp"   'flymake-show-project-diagnostics
    "ds"   'consult-flymake)
)

;; Persist history over Emacs restarts, include recentf.
(use-package savehist
  :elpaca nil
  :init
  (savehist-mode))

;; Use doom one theme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (load-theme 'doom-one t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Add default statusline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Nice startup start screen
;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook))

;; Add indent guides
(use-package highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :config
  :init (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 100))
  ;; (set-face-foreground 'highlight-indent-guides-character-face "white"))

;; Use vim-like tree for undo
(use-package undo-fu)

;; Save undo/redo across sessions
(use-package undo-fu-session
  :init
  (undo-fu-session-global-mode))

;; Allow moving block regions of text with up/down arrows
(use-package move-text
  :elpaca (:repo "https://github.com/mjlbach/evil-move-text.git")
  :general
  (:states 'normal "M-<up>" #'move-text-up)
  (:states 'normal "M-<down>" #'move-text-down))

;; Evil (vim keybindings) support
(use-package evil
  :preface (setq evil-want-keybinding nil)
  :custom
  (evil-complete-all-buffers nil)
  (evil-search-module 'evil-search "use vim-like search instead of 'isearch")
  (evil-shift-width 2 "Same behavior for vim's '<' and '>' commands")
  (evil-symbol-word-search t "search by symbol with * and #.")
  (evil-undo-system 'undo-fu)
  (evil-want-C-u-delete t)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-integration t)
  :init
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode))

;; Additional evil bindings for other packages (e.g. vterm)
(use-package evil-collection
  :after evil
  :config (evil-collection-init)
  :custom
  (evil-collection-elpaca-want-g-filters nil)
  (evil-collection-setup-minibuffer t "Add evil bindings to minibuffer"))

;; Use gc to comment regions and blocks
(use-package evil-nerd-commenter
  :general (:states 'normal "gc" #'evilnc-comment-or-uncomment-lines))

;; Make cursor theme match between gui emacs
(use-package evil-terminal-cursor-changer
  :config
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate)))

;; Fast, compiled terminal
(use-package vterm
  :general
  (global-definer
    "t" 'vterm)
  :elpaca (vterm :post-build
                 (progn
                   (setq vterm-always-compile-module t)
                   (require 'vterm)
                   ;; (require 'exec-path-from-shell)
                   ;; (exec-path-from-shell-initialize))
                   ;;print compilation info for elpaca
                   (with-current-buffer (get-buffer-create vterm-install-buffer-name)
                     (goto-char (point-min))
                     (while (not (eobp))
                       (message "%S"
                                (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                       (forward-line)))
                   (when-let ((so (expand-file-name "./vterm-module.so"))
                              ((file-exists-p so)))
                     (make-symbolic-link
                      so (expand-file-name (file-name-nondirectory so)
                                           "../../builds/vterm")
                      'ok-if-already-exists))))
  :commands (vterm vterm-other-window))

;; Enable vertico - fuzzy minibuffer completion
(use-package vertico
  :init
  (vertico-mode))

;; Consult - additional finders for vertico
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :general 
  (global-definer
    "SPC" 'consult-buffer
    "s"   'consult-ripgrep
    "?"   'consult-recent-file
    "f"   'consult-find)

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Cnfigure the narrowing key.
  (setq consult-narrow-key "<"))

;; Optionally use the `orderless' completion style.
;; More flexible matching, divides the pattern into space-separated components,
;; and matches candidates that match all of the components in any order
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable lsp support
(use-package eglot
  :defer t
  :elpaca nil
  :hook ((prog-mode . (lambda ()
                         (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                           (eglot-ensure)))))
  :config

  ;; Example - add rust support
  ;; Rust is not enabled by default for eglot
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)

  ;; To install: rustup component add rust-src && rustup component add rust-analyzer
  (add-to-list 'eglot-server-programs '(rust-ts-mode "rustup" "run" "nightly" "rust-analyzer"))

  ;; Disable eldoc support by default
  ;; TODO: due to limitations with emacs TUI/GUI compatibility and
  ;; server/client, this only applies to the emacs you start your config with
  ;; If you always want popups in the minibuffer, remove these lines
  (if (display-graphic-p)
    (add-to-list 'eglot-stay-out-of 'eldoc))) 

;; Add floating window with information summonable by K
(use-package eldoc-box
  :general
  (defun eldoc-box-scroll-up ()
    "Scroll up in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-down 3))))
  (defun eldoc-box-scroll-down ()
    "Scroll down in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-up 3))))
  (:keymaps 'eglot-mode-map
   :states '(insert normal hybrid motion visual operator emacs)
   "C-k" 'eldoc-box-scroll-up
   "C-j" 'eldoc-box-scroll-down
   "K" 'eldoc-box-eglot-help-at-point))

;;Formatting Support
(use-package apheleia
  :general
  (global-definer
    "bf"   'apheleia-format-buffer)
  :init (apheleia-global-mode +1))

;; Allow opening current link to git repo in browser
(use-package git-link
  :config
  (setq git-link-open-in-browser t))

;; Allow viewing older git revisions
(use-package git-timemachine
  :defer t)

;; Show git change information in the sign column
(use-package git-gutter
  :hook prog-mode
  :config (global-git-gutter-mode +1))

;; Treesitter installation/language mapping support
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; Tree sitter text objects (TODO: Does not support built-in treesitter yet)
;; See https://github.com/meain/evil-textobj-tree-sitter/issues/76
;;(use-package evil-textobj-tree-sitter
;;  :config
;;    ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
;;  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;;  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
;;  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner")) 
;;
;;  ;; You can also bind multiple items and we will match the first one we can find
;;  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))

;; Enable Corfu completion UI with autocompletion enabled
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  :init
  (global-corfu-mode))

;; Enable completion UI in terminal
(use-package corfu-terminal
  :elpaca (:repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :init (corfu-terminal-mode))

;; Add to CAPE for additional completion functionality
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :general
  (:states 'insert "C-x C-o" #'completion-at-point)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-symbol))
