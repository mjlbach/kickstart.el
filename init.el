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
    "."   'repeat)
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

(use-package evil-terminal-cursor-changer
  :config
    (unless (display-graphic-p)
      (require 'evil-terminal-cursor-changer)
      (evil-terminal-cursor-changer-activate)))

;;   (defmacro +general-global-menu! (name infix-key &rest body)
;;     "Create a definer named +general-global-NAME wrapping global-definer.
;;     Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
;;     (declare (indent 2))
;;     (let* ((n (concat "+general-global-" name))
;;            (prefix (intern (concat n "-map"))))
;;       `(progn
;;          (general-create-definer ,(intern n)
;;            :wrapping global-definer
;;            :prefix-map (quote ,prefix)
;;            :infix ,infix-key
;;            :wk-full-keys nil
;;            "" '(:ignore t :which-key ,name))
;;          (,(intern n) ,@body))))
;;   (+general-global-menu! "application" "a"
;;     "p" '(:ignore t "elpaca")
;;     "pb" 'elpaca-browse
;;     "pr"  '((lambda () (interactive)
;;               (let ((current-prefix-arg (not current-prefix-arg)))
;;                 (call-interactively #'elpaca-rebuild)))
;;             :which-key "rebuild")
;;     "pm" 'elpaca-manager
;;     "pl" 'elpaca-log
;;     "pi" 'elpaca-info
;;     "pI" '((lambda () (interactive) (info "Elpaca"))
;;            :which-key "elpaca-info")
;;     "ps" 'elpaca-status
;;     "pt" 'elpaca-try
;;     "pv" 'elpaca-visit)
;;   (+general-global-menu! "buffer" "b"
;;     "d"  'kill-current-buffer
;;     "o" '((lambda () (interactive) (switch-to-buffer nil))
;;           :which-key "other-buffer")
;;     "p"  'previous-buffer
;;     "r"  'rename-buffer
;;     "R"  'revert-buffer
;;     "M" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
;;           :which-key "messages-buffer")
;;     "n"  'next-buffer
;;     "s" '((lambda () (interactive) (switch-to-buffer "*scratch*"))
;;           :which-key "scratch-buffer")
;;     "TAB" '((lambda () (interactive) (switch-to-buffer nil))
;;             :which-key "other-buffer"))
;;   (+general-global-menu! "bookmark" "B")
;;   (+general-global-menu! "eval" "e"
;;     "b" 'eval-buffer
;;     "d" 'eval-defun
;;     "e" 'eval-expression
;;     "p" 'pp-eval-last-sexp
;;     "s" 'eval-last-sexp)
;;   (+general-global-menu! "file" "f"
;;     "d"   '((lambda (&optional arg)
;;               (interactive "P")
;;               (let ((buffer (when arg (current-buffer))))
;;                 (diff-buffer-with-file buffer))) :which-key "diff-with-file")
;;     "e"   '(:ignore t :which-key "edit")
;;     "ed"  '((lambda () (interactive) (find-file-existing literate-file) (widen))
;;             :which-key "dotfile")
;;     "eR"  '((lambda () (interactive) (load-file user-init-file))
;;             :which-key "reload-init.el")
;;     "et"  '((lambda ()
;;               (interactive)
;;               (save-restriction
;;                 (widen)
;;                 (check-parens)
;;                 (org-babel-tangle-file literate-file))
;;               (load-file "~/.emacs.d/init.el"))
;;             :which-key "tangle/reload-init.el")
;;     "l"   '((lambda (&optional arg)
;;               (interactive "P")
;;               (call-interactively (if arg #'find-library-other-window #'find-library)))
;;             :which-key "+find-library")
;;     "p"   'find-function-at-point
;;     "P"   'find-function
;;     "R"   'rename-file-and-buffer
;;     "s"   'save-buffer
;;     "v"   'find-variable-at-point
;;     "V"   'find-variable)
;;   (+general-global-menu! "frame" "F"
;;     "D" 'delete-other-frames
;;     "F" 'select-frame-by-name
;;     "O" 'other-frame-prefix
;;     "c" '(:ingore t :which-key "color")
;;     "cb" 'set-background-color
;;     "cc" 'set-cursor-color
;;     "cf" 'set-foreground-color
;;     "f" 'set-frame-font
;;     "m" 'make-frame-on-monitor
;;     "n" 'next-window-any-frame
;;     "o" 'other-frame
;;     "p" 'previous-window-any-frame
;;     "r" 'set-frame-name)
;;   (+general-global-menu! "git/version-control" "g")
;;   (+general-global-menu! "help" "h"
;;     "d"   '(:ignore t :which-key "describe")
;;     "df"  'describe-function
;;     "dF"  'describe-face
;;     "dk"  'describe-key
;;     "dt"  '((lambda () (interactive) (describe-text-properties (point)))
;;             :which-key "describe-text-properties")
;;     "dv"  'describe-variable
;;     "h"   (general-simulate-key "C-h" :which-key "help"))
;;   (+general-global-menu! "link" "l")
;;   (+general-global-menu! "narrow" "n"
;;     "d" 'narrow-to-defun
;;     "p" 'narrow-to-page
;;     "r" 'narrow-to-region
;;     "w" 'widen)
;;   (+general-global-menu! "project" "p"
;;     "b" '(:ignore t :which-key "buffer"))
;;   (+general-global-menu! "quit" "q"
;;     "q" 'save-buffers-kill-emacs
;;     "r" 'restart-emacs
;;     "Q" 'kill-emacs)
;;   (+general-global-menu! "spelling" "s")
;;   (+general-global-menu! "text" "x"
;;     "i" 'insert-char
;;     "I" (general-simulate-key "C-x 8" :which-key "iso"))
;;   
;;   (+general-global-menu! "tab" "t")
;;   (+general-global-menu! "toggle" "T"
;;     "d" '(:ignore t :which-key "debug")
;;     "de" 'toggle-debug-on-error
;;     "dq" 'toggle-debug-on-quit
;;     "s" '(:ignore t :which-key "spelling"))
;;   (+general-global-menu! "window" "w"
;;     "?" 'split-window-vertically
;;     "=" 'balance-windows
;;     "/" 'split-window-horizontally
;;     "O" 'delete-other-windows
;;     "X" '((lambda () (interactive) (call-interactively #'other-window) (kill-buffer-and-window))
;;           :which-key "kill-other-buffer-and-window")
;;     "d" 'delete-window
;;     "h" 'windmove-left
;;     "j" 'windmove-down
;;     "k" 'windmove-up
;;     "l" 'windmove-right
;;     "o" 'other-window
;;     "t" '((lambda () (interactive)
;;             "toggle window dedication"
;;             (set-window-dedicated-p (selected-window) (not (window-dedicated-p))))
;;           :which-key "toggle window dedication")
;;     "."  '(:ingore :which-key "resize")
;;     ".h" '((lambda () (interactive)
;;              (call-interactively (if (window-prev-sibling) #'enlarge-window-horizontally
;;                                    #'shrink-window-horizontally)))
;;            :which-key "divider left")
;;     ".l" '((lambda () (interactive)
;;              (call-interactively (if (window-next-sibling) #'enlarge-window-horizontally
;;                                    #'shrink-window-horizontally)))
;;            :which-key "divider right")
;;     ".j" '((lambda () (interactive)
;;              (call-interactively (if (window-next-sibling) #'enlarge-window #'shrink-window)))
;;            :which-key "divider up")
;;     ".k" '((lambda () (interactive)
;;              (call-interactively (if (window-prev-sibling) #'enlarge-window #'shrink-window)))
;;            :which-key "divider down")
;;     "x" 'kill-buffer-and-window)
;;   ;;vim-like completion
;;   (general-create-definer completion-def
;;     :prefix "C-x"))
;;
;; Block until current queue processed.

(use-package evil
  :demand t
  :preface (setq evil-want-keybinding nil)
  :general
    (+general-global-window
      "H" 'evil-window-move-far-left
      "J" 'evil-window-move-very-bottom
      "K" 'evil-window-move-very-top
      "L" 'evil-window-move-far-right)
    (+general-global-menu! "quit" "q"
      ":" 'evil-command-window-ex
      "/" 'evil-command-window-search-forward
      "?" 'evil-command-window-search-backward)
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
  :config
    (defun +evil-kill-minibuffer ()
    (interactive)
    (when (windowp (active-minibuffer-window))
      (evil-ex-search-exit)))

    (add-hook 'mouse-leave-buffer-hook #'+evil-kill-minibuffer)
    (define-key evil-motion-state-map [down-mouse-1] nil)
  (evil-mode))

(use-package evil-collection
  :after (evil)
  :config (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t "Add evil bindings to minibuffer")
  (evil-collection-company-use-tng t))

(use-package evil-nerd-commenter
  :init (setq evilnc-hotkey-comment-operator "gc"))

(use-package vterm :ensure t)

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode))

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)

;; ;; Persist history over Emacs restarts. Vertico sorts by history position.
;; (use-package savehist
;;   :init
;;   (savehist-mode))


;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
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

(use-package exec-path-from-shell
  ;; :elpaca (:repo "")
  :config
    (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package apheleia
  :init (apheleia-global-mode +1))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

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

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(add-hook 'python-mode-hook 'eglot-ensure)

(tool-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode)
