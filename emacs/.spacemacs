;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     ;; better-defaults
     emacs-lisp
     git
     markdown
     org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     syntax-checking
     version-control
     clojure
     haskell
     html
     python
     sql
     colors
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(visual-fill-column) ;;'(evil-terminal-cursor-changer)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         monokai
                         leuven
                         solarized-dark
                         solarized-light
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Input Mono"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols 'display-graphic-p
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  )

(defun pcl/fix-terminal-keys ()
  (define-key input-decode-map (kbd "M-O a") [C-up])
  (define-key input-decode-map (kbd "M-O b") [C-down])
  (define-key input-decode-map (kbd "M-O c") [C-right])
  (define-key input-decode-map (kbd "M-O d") [C-left])
  (define-key input-decode-map (kbd "ESC M-O A") [M-up])
  (define-key input-decode-map (kbd "ESC M-O B") [M-down])
  (define-key input-decode-map (kbd "ESC M-O C") [M-right])
  (define-key input-decode-map (kbd "ESC M-O D") [M-left]))

(defun pcl/fix-powerline ()
  (setq powerline-default-separator (if (display-graphic-p) 'wave 'utf-8)))

(defun pcl/eval-line-sexp ()
   (interactive)
   (setq evil-move-beyond-eol t)
   (save-excursion (execute-kbd-macro (kbd "$,ee")))
   (setq evil-move-beyond-eol nil))

(defun pcl/eval-defun-not-comment ()
  (interactive)
  (save-excursion
    (let (last-pos (point))
      (condition-case nil
          (while (not (string-equal "(comment"
                                    (buffer-substring-no-properties (point) (+ (point) 8))))
            (print          (buffer-substring-no-properties (point) (+ (point) 8)))
            (setq last-pos (point))
            (backward-up-list))
        (scan-error nil))
      (goto-char last-pos)
      (execute-kbd-macro (kbd "va(,er"))
      (evil-exit-visual-state))))

(defun pcl/indent-sexp ()
  (interactive)
  (save-excursion
    (execute-kbd-macro (kbd "va(=va("))
    (call-interactively 'clojure-align)
    (evil-exit-visual-state)))

(defun pcl/indent-defun ()
  (interactive)
  (sp-indent-defun)
  (call-interactively 'clojure-align))

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; useful insert mode keys
  (define-key evil-insert-state-map (kbd "<C-up>")      'sp-raise-sexp)
  (define-key evil-insert-state-map (kbd "<C-right>")   'sp-forward-slurp-sexp)
  (define-key evil-insert-state-map (kbd "<C-left>")    'sp-forward-barf-sexp)
  (define-key evil-insert-state-map (kbd "C-k")         'sp-kill-sexp)

  ;; Clojure stuff
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
    "eE" 'pcl/eval-line-sexp
    "eF" 'pcl/eval-defun-not-comment
    "ix" 'pcl/indent-sexp
    "if" 'pcl/indent-defun
    "cm" 'clojure-convert-collection-to-map
    "cs" 'clojure-convert-collection-to-set
    "cl" 'clojure-convert-collection-to-list
    "c'" 'clojure-convert-collection-to-quoted-list
    "cv" 'clojure-convert-collection-to-vector)

  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (define-clojure-indent
                                   ;; plumbing
                                   (fnk 'defun)
                                   (defnk 'defun)
                                   ;; midje
                                   (fact 'defun)
                                   (facts 'defun)
                                   (fact-group 'defun)
                                   (silent-fact 'defun)
                                   (future-fact 'defun)
                                   (tabular 'defun)
                                   (against-background 'defun)
                                   (provided 0))))
  (setq clojure-enable-fancify-symbols t)
  (setq clojure-align-forms-automatically t)

  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

  ;; fix PATH when run as service on nixos
  (setenv "PATH" (concat (getenv "PATH") ":/run/current-system/sw/bin"))
  (setq exec-path (append exec-path '("/run/current-system/sw/bin")))

  ;; terminal stuff
  (xterm-mouse-mode -1)
  (add-hook 'spaceline-pre-hook #'pcl/fix-powerline)
  (add-hook 'terminal-init-xterm-hook #'pcl/fix-terminal-keys)

  ;; Hack to disable GUI theme reload: unsafely assume last hook is the one that reloads theme
  ;(setq spacemacs--after-display-system-init-list (butlast spacemacs--after-display-system-init-list))
  ;; reload theme when terminal client connects so colors aren't terrible
  (add-hook 'terminal-init-xterm-hook (lambda () (load-theme spacemacs--cur-theme t)))
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yapfify sql-indent rainbow-mode rainbow-identifiers pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda color-identifiers-mode anaconda-mode pythonic uuidgen pug-mode org-projectile org-download link-hint intero hlint-refactor hide-comnt helm-hoogle git-link eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff dumb-jump f company-ghci column-enforce-mode clojure-snippets ws-butler window-numbering web-mode volatile-highlights visual-fill-column vi-tilde-fringe toc-org tagedit spaceline powerline smooth-scrolling smeargle slim-mode shm scss-mode sass-mode restart-emacs rainbow-delimiters popwin persp-mode pcre2el paradox page-break-lines orgit org-repo-todo org-present org-pomodoro alert log4e gntp org-plus-contrib org-bullets open-junk-file neotree move-text mmm-mode markdown-toc markdown-mode magit-gitflow macrostep lorem-ipsum linum-relative leuven-theme less-css-mode jade-mode info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo hindent highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile helm-gitignore request helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets haml-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md flycheck-pos-tip flycheck-haskell flycheck flx-ido flx fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-args evil-anzu anzu emmet-mode elisp-slime-nav diff-hl define-word company-web web-completion-data company-statistics company-quickhelp pos-tip company-ghc ghc haskell-mode company-cabal company cmm-mode clj-refactor hydra inflections edn multiple-cursors paredit s peg clean-aindent-mode cider-eval-sexp-fu eval-sexp-fu highlight cider spinner queue pkg-info clojure-mode epl buffer-move bracketed-paste auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup quelpa package-build use-package which-key bind-key bind-map evil spacemacs-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
