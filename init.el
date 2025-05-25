;;; init.el --- initilization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs
;; Elpaca Installer -*- lexical-binding: t; -*-
;; Copy below this line into your init.el
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
(elpaca-use-package-mode))
(elpaca-wait)

;; Adds crafted-emacs modules to the `load-path', sets up a module
;; writing template, sets the `crafted-emacs-home' variable.
(load "~/crafted-emacs/modules/crafted-init-config")
(load "~/crafted-emacs/modules/crafted-completion-config")

;;; Configuration phase
;;(use-package general :ensure (:wait t))

;;; Optional configuration
;; load timu theme
(use-package timu-spacegrey-theme
  :ensure t
  :config
  (load-theme `timu-spacegrey t) 
) 

;; disable Emacs toolbar
(tool-bar-mode -1)

;; disable scrollbar mode
(scroll-bar-mode -1)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;  modern tab-bar mode on
(use-package modern-tab-bar
  :ensure (modern-tab-bar :host github :repo "aaronjensen/emacs-modern-tab-bar" :protocol ssh)
  :init
  (setq tab-bar-show t
        tab-bar-new-button nil
        tab-bar-close-button-show nil)

  (modern-tab-bar-mode))

(custom-set-faces
' (modern-tab-bar ((t (:weight light :box (:line-width (12 . 8) :style flat-button) :foreground "CadetBlue2" :background "purple4" :inherit (variable-pitch default)))))
' (modern-tab-bar-separator ((t (:height 1.2 :foreground "purple4" :inherit modern-tab-bar)))))

;; set tab-bar shortcuts
;;(global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
;;(global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)
;;(global-set-key (kbd "s-t") 'tab-bar-new-tab)
;;(global-set-key (kbd "s-w") 'tab-bar-close-tab)

;; tabs are set by consecutives spaces
(setq-default indent-tabs-mode nil)

;; show tabs as 2 spaces
(setq tab-stop-list nil)
(setq default-tab-width 2)





;;; dashboard for emacs
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
      dashboard-display-icons-p t ;; display icons on both GUI and terminal
      dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
)




;; xah-flykeys
(use-package xah-fly-keys
 :ensure (:source "NON-GNU-ELPA" :host github :repo "https://github.com/xahlee/xah-fly-keys") 
 :init (xah-fly-keys 1))

;; Which-Key Config
(use-package which-key
:ensure t
:config
(setq which-key-paging-prefixes '("C-x"))
(setq which-key-paging-key "<f5>")
(which-key-mode))

;; Replace Emacs default describe commands with helpful package
(use-package helpful
 :ensure t)
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

;; access devdocs
(use-package devdocs
 :ensure t)


(use-package diminish :ensure t)
;; let emacs know to use authinfo.gpg as my password store
(setq auth-sources '("~/.authinfo.gpg"))
;; setup emacs to ask for password authnetication
(setf epg-pinentry-mode 'loopback)

;;seq manipulation funcs; dependency for magit

(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

(defun +elpaca-seq-build-steps ()
 (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(elpaca `(seq :ensure (:source "GNU-ELPA" :repo "https://git.savannah.gnu.org/gitweb/?p=emacs/elpa.git;a=shortlog;h=refs/heads/externals/seq") :build ,(+elpaca-seq-build-steps)))

;; enable matching brackets/parens
(use-package emacs :ensure nil :config (setq electric-pair-mode 1)) 

;; Org Mode Configuration ------------------------------------------------------
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

 
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch))




 (use-package emacs
  :ensure nil
  :after org
  :hook (org-mode . efs/org-mode-setup)
  :config
  
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files 
        '("~/Library/Mobile Documents/com~apple~CloudDocs/Org Notes/conch.org"
        "~/Library/Mobile Documents/com~apple~CloudDocs/Org Notes/birthdays.org"))
  
(setq org-todo-keywords
'((sequence "TODO(E)" "NEXT(n)" "|" "DONE(d!)")
  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

 (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

 
 ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 14)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))


  (efs/org-font-setup))
(elpaca-wait)


(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . efs/org-mode-visual-fill))


(use-package d2-mode
   :ensure t)


;; simple org note-taking scheme

(use-package denote
  :ensure t
  :hook
  ( ;; If you use Markdown or plain text files, then you want to make
   ;; the Denote links clickable (Org renders links as buttons right
   ;; away)
   (text-mode . denote-fontify-links-mode-maybe)
   ;; Apply colours to Denote names in Dired.  This applies to all
   ;; directories.  Check `denote-dired-directories' for the specific
   ;; directories you may prefer instead.  Then, instead of
   ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
   (dired-mode . denote-dired-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  ( :map global-map
    ("C-c n n" . denote)
    ("C-c n d" . denote-sort-dired)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    ("C-c n l" . denote-link)
    ("C-c n L" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ;; Note that `denote-rename-file' can work from any context, not just
    ;; Dired bufffers.  That is why we bind it here to the `global-map'.
    ("C-c n r" . denote-rename-file)
    ("C-c n R" . denote-rename-file-using-front-matter)

    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))

  :config
  ;; Remember to check the doc string of each of those variables.
  (setq denote-directory (expand-file-name "~/Library/Mobile Documents/com~apple~CloudDocs/Org Notes/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("emacs" "go" "backend" "sql" "real-time data" "no sql" "database" "web" "server" "api" "automation" "framework" "caching" "authentication" "security" "monitor and logging" "ci/cd"  "testing" "load and performance" "frontend" "cloud" "aws" "azure" "gcp" "docker" "podman" "kubernetes" "finance" "qa" "metanote"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
 

  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))
(elpaca-wait)

;; consult integration
(use-package consult-denote
 :ensure t
)




;; (use-package markdown-mode

;;   :config
;;   (markdown-max-image-size t)
;;   :ensure t 
;;   )



;; easily let-bind vals of an assoc-list by their names
;; dependency for forge
(use-package let-alist
  :ensure t
  :after 
  (elpaca))
(elpaca-wait)

;; transient; dependency for magit
  (use-package transient
   :ensure (:source "MELPA" :host github :repo "magit/transient" :branch "main"))
(elpaca-wait)

;; Magit
  (use-package magit
  :ensure (:source "MELPA" :host github :repo "magit/magit" :branch "main")
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(elpaca-wait)


;;hl-todo depedency for magit-todo
(use-package hl-todo
 :ensure t)
(elpaca-wait)

;; magit todos
(use-package magit-todos
 :ensure t
 :after magit
 :config (magit-todos-mode 1))

;; allows manipulation of forges; like github
;; activate after magit
;; submode to magit
(use-package forge
  :ensure (:host github :repo "magit/forge" :branch "main")
  :after magit)


;; terminal emulators
(use-package vterm
 :ensure t
)

;; multiple vterm buffers
(use-package multi-vterm :ensure t)

;; manage macos external services
;; servers,network services, daemons, etcs
(use-package prodigy
 :ensure t
 :config
  
 (prodigy-define-service
   :name "drumstick-app"
   :command "air"
   :cwd "~/github/drumstick/"
   :tags '(portfolio-proj)
   :stop-signal `sigint
   :kill--process-buffer-on-stop t)
)

;; handles system init services like brew
(use-package daemons
 :ensure t
)

;; manage docker containers with support
;; for docker compose, contexts, networks,
;; volume and image controls through transient menus
(use-package docker
 :ensure t
)

;; read and edit dockerfile in emacs
;; install dockerfile-language-server-nodejs for better syntax highlighting, editing, 
;; autocomplete, diagnostics, and linting. Its something you install from lsp-mode.
(use-package dockerfile-mode
 :ensure t
)

;; read and edit yaml files in emacs
(use-package yaml-mode
 :ensure t
 :mode 
 ("\\.yaml\\'" "\\.yml\\`")
)


;; manage kubernetes pods
(use-package kubed
 :ensure t
)

;; manage aws-cli
;;(use-package aws
;; :ensure t
;;)

;; manage azure-cli
;;(use-package azure
;; :ensure t
;;)

       
;; drag lines and regions easily
(use-package drag-stuff
 :ensure t
 :init
(drag-stuff-global-mode 1)
(drag-stuff-define-keys))

(use-package nerd-icons
  :ensure t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))
(elpaca-wait)

 ;;Use nerdicons in buffer switcher
 (use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; my mode-line

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-lsp-icon t)
  (setq doom-modeline-lsp nil)
  (setq doom-modeline-fly t)
  (setq doom-modeline-hud nil)
  (setq doom-modeline-workspace-name nil)
  (setq doom-modeline-buffer-file-name-style 'file-name))

;; install nerd icons for dired
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))
  (add-to-list 'nerd-icons-extension-icon-alist '("epub" nerd-icons-faicon "nf-fa-book" :face nerd-icons-red))
(elpaca-wait)


;; provides a directory tree
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemac-follow-after-init                t
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
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 22)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

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
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
   )
(elpaca-wait)


(use-package treemacs-nerd-icons
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))


;; collection of really useful extensions
(use-package crux
 :ensure t)

(use-package treemacs-magit
   :ensure (:host github :repo "Alexander-Miller/treemacs")
   :after (treemacs magit))

;; enable avy go to line
(global-set-key (kbd "M-g f") 'avy-goto-line)

;; Programming Configs
(use-package go-mode
 :ensure t
 :config
 (autoload 'go-mode "go-mode" nil t)
 (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook 
 (lambda ()
   (add-hook 'before-save-hook 'gofmt-before-save) 
  (setq-default) 
  (setq tab-width 4) 
  (setq standard-indent 1) 
   (setq indent-tabs-mode nil))))

;; go-tags mode enalble
 (use-package go-tag 
  :ensure t
  :after go-mode
  :config
   (define-key go-mode-map (kbd "C-c t") #'go-tag-add)
   (define-key go-mode-map (kbd "C-c T") #'go-tag-remove))



;; lsp is allows for language syntax highlighting
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-which-key-integration t)
;; gopls LSP Mode Config
(add-hook 'go-mode-hook #'lsp-deferred)
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'before-save-hook #'gofmt-before-save))

  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks) )
(elpaca-wait)

(use-package lsp-ui
  :ensure t)

(setq lsp-diagnostic-package :none)
;; enable line numbers for programming modes only
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(global-hl-line-mode +1)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)



;; flycheck: Syntax Checker
 (use-package flycheck
   :ensure t
   :init (global-flycheck-mode))

;; flycheck-golangci-linter
  (use-package flycheck-golangci-lint
   :ensure t; 
   :hook (go-mode . flycheck-golangci-lint-setup)
   :config (setq flycheck-golangci-lint-config "/Users/username/.emacs.d/elpaca/repos/flycheck-golangci-lint/golangci-lint-config/.golangci.yml"))

;;consult-lsp integration
(use-package consult-lsp
  :ensure t)

(use-package jsonrpc
:ensure t)
(elpaca-wait)

(use-package dape
  :ensure (:host github :repo "svaante/dape")
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load))

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
   (dape-breakpoint-global-mode)

  ;; Info buffers to the right
   (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  ;; (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  ;; (setq dape-cwd-fn 'projectile-project-root)
  )
(elpaca-wait)

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :ensure nil
   :config
  (repeat-mode))

;; Emmet for webdev
(use-package emmet-mode
 :ensure t
 :config
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(setq emmet-move-cursor-between-quotes t) ;; default nil
)


;; web-mode config for web templating
(use-package web-mode
 :ensure t
 :config
(add-to-list 'auto-mode-alist'("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist'("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist'("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist'("\\.php\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook 'my-web-mode-hook))

(use-package htmltagwrap
  :ensure t
  :config
  (setq htmltagwrap-tag "div")
  (setq htmltagwrap-indent-region-after-wrap t)
  (setq htmltagwrap-indie-tag-wrap-not-inline t))

;; js2 minor-mode
(use-package js2-mode 
 :ensure t
 :config
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

;; folding files/code closures
(use-package treesit-fold
 :ensure t
)

;; save buffers along with split window configs
;; in a named workspace tab
(use-package activities
  :ensure t
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))


;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)


(define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)

;;Use base  Project.el for projects with add consult support
(use-package consult-project-extra
  :ensure t)

;; Profile emacs startup
(defun crafted-startup-example/display-startup-time ()
  "Display the startup time after Emacs is fully initialized."
  (message "Crafted Emacs loaded in %s."
           (emacs-init-time)))
(add-hook 'elpaca-after-init #'crafted-startup-example/display-startup-time)


;;(setq custom-file (expand-file-name "custom.el" user-emacs-directory)) ;;; Code: if custom file exists
;;(when (and custom-file
 ;;          (file-exists-p custom-file))
  ;;(load custom-file nil 'nomessage))

(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

(setq elpaca-after-init-time (or elpaca-after-init-time (current-time)))
(elpaca-wait)

(provide 'init)
;;; init.el ends here
