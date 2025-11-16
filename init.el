;; -*- lexical-binding: t; byte-compile-warnings: (not free-vars unresolved) -*-

;;; Commentary:
;; Custom Emacs config

;;; Code:


(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(setq gc-cons-threshold (* 100 1024 1024))

(setq lsp-log-io nil)


(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory) nil 'nomessage)


;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(setq straight-use-package-by-default t)


(use-package gcmh
  :straight (gcmh :type git :host gitlab :repo "koral/gcmh")
  :config
  (setq gcmh-idle-delay 5)
  (setq gcmh-high-cons-threshold (* 100 1024 1024))
  (gcmh-mode 1))


(use-package xclip
							:config
							(setq xclip-program "wl-copy")
							(setq xclip-select-enable-clipboard t)
							(setq xclip-mode t)
							(setq xclip-method (quote wl-copy))
							)

;; This doesn't work when emacs daemon is run through systemd because wayland is not loaded yet
;; Clipboard for Wayland
;; (setq wl-copy-process nil)
;; (defun wl-copy (text)
;;   (setq wl-copy-process (make-process :name "wl-copy"
;;                                       :buffer nil
;;                                       :command '("wl-copy" "-f" "-n")
;;                                       :connection-type 'pipe))
;;   (process-send-string wl-copy-process text)
;;   (process-send-eof wl-copy-process))

;; (defun wl-paste ()
;;   (if (and wl-copy-process (process-live-p wl-copy-process))
;;       nil ; Skip paste while we're the one who owns the clipboard
;;     (shell-command-to-string "wl-paste -n | tr -d \r")))

;; (setq interprogram-cut-function 'wl-copy)
;; (setq interprogram-paste-function 'wl-paste)

;; Use xclip for clipboard integration
;;(setq interprogram-cut-function
;;      (lambda (text &optional push)
;;        (with-temp-buffer
;;          (insert text)
;;          (call-process-region (point-min) (point-max) "xclip" nil nil nil "-selection" "clipboard"))))
;;
;;(setq interprogram-paste-function
;;      (lambda ()
;;        (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
;;          (unless (string= (car kill-ring) xclip-output)
;;            xclip-output))))

;; Disable Emacs toolbar, menubar, and scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq use-short-answers t)

(global-hl-line-mode)
(global-word-wrap-whitespace-mode)

;; MEOW
(use-package meow
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode 1))

;; Set jk as escape insert mode
(setq meow-two-char-escape-sequence "jk")
(setq meow-two-char-escape-delay 0.5)

(defun meow--two-char-exit-insert-state (s)
  (when (meow-insert-mode-p)
    (let ((modified (buffer-modified-p)))
      (insert (elt s 0))
      (let* ((second-char (elt s 1))
             (event
              (if defining-kbd-macro
                  (read-event nil nil)
								(read-event nil nil meow-two-char-escape-delay))))
        (when event
          (if (and (characterp event) (= event second-char))
              (progn
                (backward-delete-char 1)
                (set-buffer-modified-p modified)
                (meow--execute-kbd-macro "<escape>"))
            (push event unread-command-events)))))))

(defun meow-two-char-exit-insert-state ()
  "Exit insert mode with two-key sequence."
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (self-insert-command 1)  ; Just insert the character normally in vterm
    (meow--two-char-exit-insert-state meow-two-char-escape-sequence)))

(define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1)
						#'meow-two-char-exit-insert-state)

;; end j k


(keymap-global-set "C-h C-t" 'consult-theme)


(use-package vterm)
(use-package meow-vterm
	:straight '(meow-vterm :type git :host github :repo "accelbread/meow-vterm" :branch "master")
	:ensure t)

(meow-vterm-enable)

(defun my/vterm-new ()
  "Create a new vterm buffer with a unique name."
  (interactive)
  (let ((vterm-buffer-name (generate-new-buffer-name "*vterm*")))
    (vterm)))

(use-package which-key
  :config
  (which-key-mode))

(add-to-list 'default-frame-alist
             '(font . "FantasqueSansM Nerd Font-10"))


(let ((mono-spaced-font "FantasqueSansM Nerd Font") 
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 100)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))


(use-package default-font-presets

  :commands
  (default-font-presets-forward
   default-font-presets-backward
   default-font-presets-choose
   default-font-presets-scale-increase
   default-font-presets-scale-decrease
   default-font-presets-scale-fit
   default-font-presets-scale-reset)

  :config
  (setq default-font-presets-list
    (list
      "Maple Mono NF-10:spacing=90"
      "Fantasque Sans Mono-10"
      "AporeticSansMonoNerdFont-11")))


;; DOOM THEMES
(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-nord-aurora t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Add custom theme directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package doom-wallust
  :straight '(doom-wallust :type nil :local-repo "~/code/elisp/doom-wallust"))

;; MODELINE

(use-package mood-line
  :straight '(mood-line :type git :host github :repo "jessiehildebrandt/mood-line" :branch "master")
  :config (mood-line-mode))

(use-package page-break-lines)
(global-page-break-lines-mode)

;; DASHBOARD
(use-package dashboard
  :demand t
  :config
  (setq dashboard-banner-logo-title "Minimacs")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-show-shortcuts t)
	(setq dashboard-display-icons-p t)
	(setq dashboard-icon-type 'nerd-icons)
	(setq dashboard-set-heading-icons t)
	(setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)
                          (registers . 5)))
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-projects-switch-function 'projectile-switch-project-by-name)
  (setq dashboard-item-shortcuts '((recents   . "r")
                                    (bookmarks . "m")
                                    (projects  . "p")
                                    (agenda    . "a")
                                    (registers . "e")))
  (dashboard-setup-startup-hook))


(add-hook 'server-after-make-frame-hook
          (lambda ()
            (when (and (not (minibufferp))
                       (string= (buffer-name) "*scratch*"))
              (dashboard-refresh-buffer))))

(defun my/new-frame-with-perspective ()
  "Create a new perspective and show dashboard when opening a new frame."
  ;; Create perspective with timestamp or let user name it later
  (persp-switch "dashboard")
  (dashboard-refresh-buffer))

;; Hook into new frame creation
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              ;; Only run if we're in scratch buffer (no --eval was used)
              (when (string= (buffer-name) "*scratch*")
                (my/new-frame-with-perspective)))))

;; Also handle emacsclient with no frame (terminal)
(add-hook 'server-after-make-frame-hook
          (lambda ()
            ;; Only run if we're in scratch buffer (no --eval was used)
            (when (string= (buffer-name) "*scratch*")
              (my/new-frame-with-perspective))))

(use-package spacious-padding)
(spacious-padding-mode)


(use-package centaur-tabs
	:bind ("C-c <tab>" . centaur-tabs-ace-jump)
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'left)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-mode t))

(use-package zoom
  :straight t
  :config
  (zoom-mode t)
  
  (setq zoom-size '(0.618 . 0.618))
  (setq zoom-ignored-major-modes '(dired-mode markdown-mode which-key-mode))
  (setq zoom-ignored-buffer-names '("zoom.el" "*lsp-ui-imenu*"))
  (setq zoom-ignored-buffer-name-regexps '("^lsp-ui"))
  (setq zoom-ignore-predicates 
        '((lambda () 
            (string-match-p "\\*which-key\\*" (buffer-name)))))
  (setq zoom-minibuffer-preserve-layout t))

(defun my/which-key-fix (original window &rest args)
  "Prevent zoom from affecting which-key window size."
  (let ((buf-name (buffer-name (window-buffer window))))
    (if (string-match-p "\\*which-key\\*" buf-name)
        ;; For which-key windows: unfix, fit, then fix
        (progn
          (with-selected-window window
            (setq window-size-fixed nil))
          (apply original window args)
          (with-selected-window window
            (setq window-size-fixed t)))
      ;; For other windows: call normally
      (apply original window args))))

(advice-add 'fit-window-to-buffer :around #'my/which-key-fix)




;; ACE WINODW
(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; WINDOW STUFF

(use-package solaire-mode
	:config
	(solaire-global-mode +1)
)

(meow-leader-define-key
 '("w h" . windmove-left)
 '("w l" . windmove-right)
 '("w j" . windmove-down)
 '("w k" . windmove-up)
 '("w v" . split-window-horizontally)
 '("w s" . split-window-vertically)
 '("w d" . delete-window)
 '("f f" . find-file)
 '("b b" . consult-buffer)
 '("z z" . recenter-top-bottom)
 )


;; The `vertico' package applies a vertical layout to the minibuffer.
;; It also pops up the minibuffer eagerly so we can see the available
;; options without further interactions.  This package is very fast
;; and "just works", though it also is highly customisable in case we
;; need to modify its behaviour.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:cff33514-d3ac-4c16-a889-ea39d7346dc5
(use-package vertico
  ;; Special recipe to load extensions conveniently
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  ;; :general
  ;; (:keymaps 'vertico-map
  ;;           "<tab>" #'vertico-insert    ; Choose selected candidate
  ;;           "<escape>" #'minibuffer-keyboard-quit ; Close minibuffer
  ;;           ;; NOTE 2022-02-05: Cycle through candidate groups
  ;;           "C-M-n" #'vertico-next-group
  ;;           "C-M-p" #'vertico-previous-group)
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  ;; :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
  ;;        (minibuffer-setup . vertico-repeat-save)) ; Make sure vertico state is saved
  :config
  (vertico-mode)
  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
								(setq cand (funcall orig cand prefix suffix index _start))
								(concat
								 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
								 cand)))
  )

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (orderless-matching-styles '(orderless-flex orderless-regexp))
  ;; Optional: match even with spaces
  (orderless-component-separator #'orderless-escapable-split-on-space))


;; The `marginalia' package provides helpful annotations next to
;; completion candidates in the minibuffer.  The information on
;; display depends on the type of content.  If it is about files, it
;; shows file permissions and the last modified date.  If it is a
;; buffer, it shows the buffer's size, major mode, and the like.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:bd3f7a1d-a53d-4d3e-860e-25c5b35d8e7e
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;; The `orderless' package lets the minibuffer use an out-of-order
;; pattern matching algorithm.  It matches space-separated words or
;; regular expressions in any order.  In its simplest form, something
;; like "ins pac" matches `package-menu-mark-install' as well as
;; `package-install'.  This is a powerful tool because we no longer
;; need to remember exactly how something is named.
;;
;; Note that Emacs has lots of "completion styles" (pattern matching
;; algorithms), but let us keep things simple.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:7cc77fd0-8f98-4fc0-80be-48a758fcb6e2
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; The `consult' package provides lots of commands that are enhanced
;; variants of basic, built-in functionality.  One of the headline
;; features of `consult' is its preview facility, where it shows in
;; another Emacs window the context of what is currently matched in
;; the minibuffer.  Here I define key bindings for some commands you
;; may find useful.  The mnemonic for their prefix is "alternative
;; search" (as opposed to the basic C-s or C-r keys).
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:22e97b4c-d88d-4deb-9ab3-f80631f9ff1d
(use-package consult
  :ensure t
  :bind (;; A recursive grep
         ("C-c C-s C-g" . consult-grep)
         ;;  Search for files names recursively
         ("C-c C-s C-f" . consult-find)
         ;;  Search through the outline (headings) of the file
         ("C-c C-s C-o" . consult-outline)
         ;;  Search the current buffer
         ("C-c C-s C-l" . consult-line)
         ;;  Switch to another buffer, or bookmarked file, or recently
         ;;  opened file.
         ("C-c C-s C-b" . consult-buffer)
				 ("C-c C-s C-r" . consult-recent-file)
				 ("C-c C-s C-i" . consult-imenu)))

;; The `embark' package lets you target the thing or context at point
;; and select an action to perform on it.  Use the `embark-act'
;; command while over something to find relevant commands.
;;
;; When inside the minibuffer, `embark' can collect/export the
;; contents to a fully fledged Emacs buffer.  The `embark-collect'
;; command retains the original behaviour of the minibuffer, meaning
;; that if you navigate over the candidate at hit RET, it will do what
;; the minibuffer would have done.  In contrast, the `embark-export'
;; command reads the metadata to figure out what category this is and
;; places them in a buffer whose major mode is specialised for that
;; type of content.  For example, when we are completing against
;; files, the export will take us to a `dired-mode' buffer; when we
;; preview the results of a grep, the export will put us in a
;; `grep-mode' buffer.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:61863da4-8739-42ae-a30f-6e9d686e1995
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

;; The `embark-consult' package is glue code to tie together `embark'
;; and `consult'.
(use-package embark-consult
  :ensure t)

;; The `wgrep' packages lets us edit the results of a grep search
;; while inside a `grep-mode' buffer.  All we need is to toggle the
;; editable mode, make the changes, and then type C-c C-c to confirm
;; or C-c C-k to abort.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852
(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

;; Icons
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package nerd-icons
  :straight (nerd-icons
             :type git
             :host github
             :repo "rainstormstudio/nerd-icons.el"
             :files (:defaults "data"))
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;; Direnv
(use-package direnv
  :config
  (direnv-mode))


(use-package flymake-jsts
  :straight '(flymake-jsts :type git :host github :repo "orzechowskid/flymake-jsts" :branch "main"))

(use-package tsx-mode
  :straight '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el" :branch "emacs30"))

(use-package csv-mode)

(setq-default tab-width 2)

(use-package apheleia)

(use-package flymake)

;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
;;;; Code Completion
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto nil)                  ; Enable auto completion
  (corfu-auto-prefix 3)           ; Minimum length of prefix for completion
  (corfu-auto-delay 0.2)            ; No delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . corfu-insert))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
	:config
	(keymap-set corfu-mode-map "TAB" 'completion-at-point)
	)


;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )


;; LANGUAGES

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package tuareg-mode
  :mode "\\.ml\\'")



;; Register .tsx files to use tsx-ts-mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))


;; Treesit
(use-package treesit
  :straight nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.lua\\'"  . lua-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode))
  :preface
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

;; ;; Combobulate as a separate package
;; ;; Combobulate as a separate package
;; (use-package combobulate
;;   :straight (combobulate 
;;              :type git 
;;              :host github 
;;              :repo "mickeynp/combobulate"
;;              :files ("*.el"))  ; Only load .el files, ignore submodules
;;   :preface
;;   (setq combobulate-key-prefix "C-c o")
;;   :hook
;;   ((python-ts-mode . combobulate-mode)
;;    (js-ts-mode . combobulate-mode)
;;    (go-ts-mode . combobulate-mode)
;;    (html-ts-mode . combobulate-mode)
;;    (css-ts-mode . combobulate-mode)
;;    (yaml-ts-mode . combobulate-mode)
;;    (typescript-ts-mode . combobulate-mode)
;;    (json-ts-mode . combobulate-mode)
;;    (tsx-ts-mode . combobulate-mode)))
;; LSP

(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter
	:preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)

       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (setq lsp-use-plists t)
  ;; Initiate https://github.com/blahgeek/emacs-lsp-booster for performance
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))



(use-package lsp-completion
  :no-require
	:straight nil
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :config (setq lsp-ui-doc-enable t
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))


(use-package lsp-eslint
  :demand t
	:straight nil
  :after lsp-mode)


(with-eval-after-load 'lsp-mode
  ;; Tell lsp-mode how to identify these modes
  (add-to-list 'lsp-language-id-configuration '(tsx-ts-mode . "typescriptreact"))
  (add-to-list 'lsp-language-id-configuration '(typescript-ts-mode . "typescript"))
  
  ;; Register the client with explicit path
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda ()
                       (list (or (executable-find "typescript-language-server")
                                 "/run/current-system/sw/bin/typescript-language-server")
                             "--stdio")))
    :activation-fn (lsp-activate-on "typescript" "typescriptreact")
    :priority 1
    :server-id 'ts-ls
    :major-modes '(typescript-mode typescript-ts-mode tsx-ts-mode js-mode js-ts-mode))))




;; Treemacs
(global-unset-key (kbd "C-t")) ;; Removing C-t for transpose-char because i dont ever use that and i want treemacs to have it

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-buffer-name-function            #'treemacs-default-buffer-name
          treemacs-buffer-name-prefix              " *Treemacs-Buffer-"
          treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      -2
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-t 1"   . treemacs-delete-other-windows)
        ("C-t t"   . treemacs)
        ("C-t d"   . treemacs-select-directory)
        ("C-t b"   . treemacs-bookmark)
        ("C-t f f" . treemacs-find-file)
        ("C-t f t" . treemacs-find-tag)
        ("C-t f m" . treemacs-move-file)
        ("C-t f D" . treemacs-delete-file)
        ("C-t c f" . treemacs-create-file)
        ("C-t c d" . treemacs-create-dir)
        ("C-t c w" . treemacs-create-workspace)
        ("C-t R" . treemacs-rename-file)
        ("C-t r" . treemacs-refresh))
	)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-all-the-icons
  :ensure t
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))


;; (use-package ranger)
(use-package dirvish)


;; (treemacs-start-on-boot)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))


(defun my/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless))) ;; Configure orderless

;; Add this hook
(add-hook 'lsp-completion-mode-hook #'my/lsp-mode-setup-completion)


;; LSP - Emmet
(use-package emmet-mode
  :hook ((tsx-ts-mode . emmet-mode)
         (typescript-ts-mode . emmet-mode)
         (web-mode . emmet-mode)
         (html-mode . emmet-mode))
  :config
  (setq emmet-expand-jsx-className? t)) ;; Support className in JSX

;; Workspaces

(use-package perspective
  :bind (("C-x C-b" . persp-list-buffers)
         ("C-<tab> b" . persp-switch-to-buffer*)
         ("C-<tab> k" . persp-kill-buffer*)
				 ("C-<tab> <tab>" . persp-switch))
  :custom
  (persp-mode-prefix-key (kbd "C-<tab>"))
  :init
  (persp-mode))
  ;; :config
  ;; ;; Integrate with projectile
  ;; (with-eval-after-load "projectile"
  ;;   (setq projectile-switch-project-action #'projectile-dired)))

;; Auto-create perspectives for projectile projects
;; (add-hook 'projectile-after-switch-project-hook
;;           (lambda ()
;;             (persp-switch (projectile-project-name))))


;; Better projectile + perspective integration
(use-package persp-projectile
  :after (perspective projectile)
  :bind (:map projectile-mode-map
              ("C-c p p" . projectile-persp-switch-project)))


;; Add this after your projectile use-package
(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

;; PROJECTILE
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
	(setq projectile-switch-project-action #'projectile-find-file)
	)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


	
(meow-leader-define-key
 '("p p" . projectile-persp-switch-project)
 '("p f" . projectile-find-file))


;; Other Appearance
(use-package rainbow-mode)

(use-package rainbow-delimiters
  :straight (rainbow-delimiters :type git :host github :repo "Fanael/rainbow-delimiters" :branch "master"))

(add-hook 'tsx-ts-mode-hook #'rainbow-delimiters-mode) 

;; Line numbers
(global-display-line-numbers-mode 1)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
								dashboard-mode-hook
								vterm-mode-hook
								treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Vundo
(use-package vundo
  :straight t
  :config
  ;; Use unicode symbols for better visualization
  (setq vundo-glyph-alist vundo-unicode-symbols)
  
  ;; Compact display
  (setq vundo-compact-display t)
  
  ;; Window configuration
  (setq vundo-window-max-height 10)
  
  ;; Roll back to last saved state easily
  (setq vundo-roll-back-on-quit t)
  
  :bind
  ("C-x u" . vundo)
  ;; Optional: easier access
  ("C-/" . vundo))

;; Undo-fu: Better undo/redo commands (works great with vundo)
(use-package undo-fu
  :straight t
  :config
  ;; Disable C-z (suspend-frame) if you want
  (global-unset-key (kbd "C-z"))
  
  :bind
  ;; Standard undo/redo bindings
  ("C-z" . undo-fu-only-undo)
  ("C-S-z" . undo-fu-only-redo)
  ;; Alternative bindings
  ("C-?" . undo-fu-only-redo))

;; Undo-fu-session: Persistent undo history
(use-package undo-fu-session
  :straight t
  :after undo-fu
  :config
  ;; Directory for undo history files
  (setq undo-fu-session-directory 
        (expand-file-name "undo-fu-session" user-emacs-directory))
  
  ;; Don't save undo history for these files
  (setq undo-fu-session-incompatible-files 
        '("/COMMIT_EDITMSG\\'" 
          "/git-rebase-todo\\'"
          "/tmp/"
          "/temp/"))
  
  ;; Compress the undo history files
  (setq undo-fu-session-compression 'gz)
  
  ;; Enable globally
  (undo-fu-session-global-mode))

;; Clean up old undo files (older than 30 days)
(with-eval-after-load 'undo-fu-session
  (setq undo-fu-session-file-limit 
        (* 60 60 24 30))) ; 30 days in seconds


;; Vimish fold
(use-package vimish-fold)


;; The built-in `savehist-mode' saves minibuffer histories.  Vertico
;; can then use that information to put recently selected options at the top
(savehist-mode 1)

;; Store backup files elsewhere

;; Store all backup files in a single directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Make sure the directory exists
(unless (file-exists-p "~/.emacs.d/backups")
  (make-directory "~/.emacs.d/backups" t))

;; this must be here to keep the package system happy, normally you do
;; `package-initialize' for real in your own init.el
;; (package-initialize)

;;; init.el ends here
