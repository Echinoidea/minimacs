;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom Emacs config

;;; Code:

;; Chemacs, disable this when youre ready
;;(require 'chemacs
;;         (expand-file-name "chemacs.el"
;;                           (file-name-directory
;;                            (file-truename load-file-name))))
;;(chemacs-load-user-init)

;; (setq lsp-use-plists t)

(server-start)

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-sync
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(setq straight-use-package-by-default t)

;; Clipboard for Wayland
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))

(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; Skip paste while we're the one who owns the clipboard
    (shell-command-to-string "wl-paste -n | tr -d \r")))

(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

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
  (interactive)
  (meow--two-char-exit-insert-state meow-two-char-escape-sequence))

(define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1)
	    #'meow-two-char-exit-insert-state)
;; end j k 


(keymap-global-set "C-h C-t" 'consult-theme)


(use-package which-key
  :config
  (which-key-mode))

(add-to-list 'default-frame-alist
             '(font . "AporeticSansMonoNerdFont-10"))

(let ((mono-spaced-font "Monospace")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 100)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))


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
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
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

;; (use-package moody
;;   :config
;;   (moody-replace-mode-line-front-space)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode))

;; DASHBOARD
(use-package dashboard
  :demand t  ;; Force load immediately
  :init
  (setq dashboard-banner-logo-title "Emacs")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-show-shortcuts t)
  :config
  (dashboard-setup-startup-hook))

;; After everything loads, open dashboard
(add-hook 'after-init-hook 
          (lambda ()
            (dashboard-refresh-buffer)))

(use-package spacious-padding)
(spacious-padding-mode)

;; ACE WINODW
(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; WINDOW STUFF

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
 )

;; PROJECTILE
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package page-break-lines)
(global-page-break-lines-mode)

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

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))


(use-package direnv
  :config
  (direnv-mode))

(use-package flymake-jsts
  :straight '(flymake-jsts :type git :host github :repo "orzechowskid/flymake-jsts" :branch "main"))

(use-package tsx-mode
  :straight '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el" :branch "emacs30"))

(setq-default tab-width 2)

(use-package apheleia)

(use-package flymake)

;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.1)         ;; Small delay
  (corfu-auto-prefix 3)          ;; Complete after 2 chars
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt))     ;; Preselect the prompto 
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

;; LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((nix-mode . lsp-deferred)
         (tuareg-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom
  ;; This is the key part - use corfu instead of lsp-ui
  ;; (lsp-completion-provider :none) ;; we use Corfu!
	(lsp-idle-delay 0.1)
	(lsp-completion-show-detail t)
(lsp-completion-show-kind t)
	) 

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

(use-package flycheck)

(use-package lsp-ui)

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
  (persp-mode)
  :config
  ;; Integrate with projectile
  (with-eval-after-load "projectile"
    (setq projectile-switch-project-action #'projectile-dired)))

;; Auto-create perspectives for projectile projects
(add-hook 'projectile-after-switch-project-hook
          (lambda ()
            (persp-switch (projectile-project-name))))


(meow-leader-define-key
 '("p p" . projectile-switch-project)
 '("p f" . projectile-find-file))


;; (setq projectile-switch-project-action #'projectile-find-file)

;; ORG MODE
(setq org-directory "~/org/")


;; (setq org-agenda-files
;;       (list "~/org/todo-personal.org"
;;             "~/org/todo-work.org"
;;             "~/org/todo-ibegin.org"
;;             "~/org/todo-midas.org"
;;             "~/org/todo-school.org"
;;             "~/org/todo-gamedev.org"
;;             "~/org/todo-programming.org"
;;             "~/org/calendar.org"))
;; 


(use-package org-modern
  :config 
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-agenda-tags-column 0
   org-ellipsis "..."
   org-modern-star '("✿" "❀" "❁" "❃" "❋" "✾" "✽" "✻")
   ))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'org-modern-mode)

;; (add-hook 'org-mode-hook #'org-modern-indent-mode 90)

;; (setq org-log-done 'time)

;; (add-hook 'org-mode-hook #'org-modern-mode)
;; (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(use-package deft
  :config (setq deft-directory "~/org/deft/"
                deft-extensions '("md" "org")
 		deft-recursive t))


(defun my/consult-org-files ()
  "Fuzzy find files in ~/org directory using consult."
  (interactive)
  (find-file "~/org")
  )

(global-set-key (kbd "C-c n f") #'my/consult-org-files)

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
		dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))




;; The built-in `savehist-mode' saves minibuffer histories.  Vertico
;; can then use that information to put recently selected options at the top
(savehist-mode 1)

;; Store backup files elsewhere

;; Store all backup files in a single directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Make sure the directory exists
(unless (file-exists-p "~/.emacs.d/backups")
  (make-directory "~/.emacs.d/backups" t))

;; (setq lsp-use-plists t)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(when lsp-use-plists
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse))

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)




;; this must be here to keep the package system happy, normally you do
;; `package-initialize' for real in your own init.el
;; (package-initialize)

;;; init.el ends here
