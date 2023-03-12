;;; early-init.el --- Emacs pre package.el & GUI configuration -*- lexical-binding: t; -*-

; From elpaca
(setq package-enable-at-startup nil)

(setq inhibit-default-init nil)

; From prog
(setq native-comp-async-report-warnings-errors nil)

; Skipping a bunch of regular expression searching in the file-name-handler-alist should improve start time.
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

; ggc-cons-threshold (800 KB) and gc-cons-percentage (0.1) control when the Emacs garbage collector can kick in. 
; Temporarily turning these off during init should decrease startup time. Resetting them afterward will ensure that normal operations don’t suffer from a large GC periods.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun +reset-init-values ()
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           gc-cons-threshold 100000000)
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))

(with-eval-after-load 'elpaca
  (add-hook 'elpaca-after-init-hook '+reset-init-values))

;; Turning off these visual elements before UI initialization should speed up init.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

; Prevent printing instructions on how to close an emacsclient frame.
(setq server-client-instructions nil)

; Implicitly resizing the Emacs frame adds to init time. Fonts larger than the system default can cause frame resizing, which adds to startup time.
(setq frame-inhibit-implied-resize t)

; Silence the bell function
(setq ring-bell-function #'ignore
      inhibit-startup-screen t)

