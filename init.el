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

;; Use evil in elpaca
(with-eval-after-load 'evil
  (with-eval-after-load 'elpaca-ui (evil-make-intercept-map elpaca-ui-mode-map))
  (with-eval-after-load 'elpaca-info (evil-make-intercept-map elpaca-info-mode-map)))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

;; Set up keybindings
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
     "-" 'dired-jump)
  (general-create-definer global-leader
    :keymaps 'override
    :states '(insert normal hybrid motion visual operator)
    :prefix "SPC m"
    :non-normal-prefix "S-SPC m"
    "" '( :ignore t
          :which-key
          (lambda (arg)
            (cons (cadr (split-string (car arg) " "))
                  (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))
   )
(elpaca-wait)

;; Basic customization
;; Remove tool and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Preserve cursor position on screen
(setq scroll-preserve-screen-position 'always)

;; Display line numbers
(global-display-line-numbers-mode)

;; Enable mouse mode in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(use-package exec-path-from-shell
  ;; :elpaca (:repo "")
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

  ;; Store recent files
  (recentf-mode 1)
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 100)


  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

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

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :elpaca nil
  :init
  (savehist-mode))

;; Theming
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (load-theme 'doom-one t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Evil support
(use-package evil
  :demand t
  :preface (setq evil-want-keybinding nil)
  :custom
  (evil-complete-all-buffers nil)
  (evil-search-module 'evil-search "use vim-like search instead of 'isearch")
  (evil-shift-width 2 "Same behavior for vim's '<' and '>' commands")
  (evil-symbol-word-search t "search by symbol with * and #.")
  (evil-undo-system 'undo-redo)
  (evil-want-C-i-jump t)
  (evil-want-C-u-delete t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-integration t)
  :init
  (setq evil-undo-system 'undo-fu)
  :config
  (defun +evil-kill-minibuffer ()
    (interactive)
    (when (windowp (active-minibuffer-window))
      (evil-ex-search-exit)))

  (add-hook 'mouse-leave-buffer-hook #'+evil-kill-minibuffer)
  (define-key evil-motion-state-map [down-mouse-1] nil)
  (evil-mode))

(use-package undo-fu)
(use-package undo-fu-session
  :init
  (undo-fu-session-global-mode)
)

(use-package evil-collection
  :after (evil)
  :config (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t "Add evil bindings to minibuffer")
  (evil-collection-company-use-tng t))

(use-package evil-terminal-cursor-changer
  :config
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate)))

(use-package evil-nerd-commenter
  :init (setq evilnc-hotkey-comment-operator "gc"))

(use-package vterm
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

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
;; Different scroll margin
;; (setq vertico-scroll-margin 0)

;; Show more candidates
;; (setq vertico-count 20)

;; Grow and shrink the Vertico minibuffer
;; (setq vertico-resize t)

;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
;; (setq vertico-cycle t)
  )


;; Example configuration for Consult
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
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; LSP support
;; Enable hooks for eglot
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(rust-mode "rustup" "run" "nightly" "rust-analyzer"))) 

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
  ;; :init
  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t))

;; Support for rust
(use-package rust-mode
  :init (add-hook 'rust-mode-hook 'eglot-ensure))

;; Support for python
(dolist (mode '(python-mode-hook c-mode-hook))
 (add-hook mode #'eglot-ensure))

;;Formatting Support
(use-package apheleia
  :init (apheleia-global-mode +1))

;; Treesitter support
(use-package tree-sitter
  :init (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(use-package corfu-terminal
  :elpaca (:repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :init (corfu-terminal-mode))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (
    ("C-x C-o" . completion-at-point)) ;; capf
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  )
