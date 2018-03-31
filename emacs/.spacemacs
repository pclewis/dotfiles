;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     javascript
     yaml
     csv
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
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
     nixos
     ;(ruby :variables ruby-enable-enh-ruby-mode t)
     ruby-on-rails
     w3m
     evil-cleverparens
     speed-reading
     spell-checking
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     visual-fill-column
     xterm-color
     mixed-pitch
     ;auto-dim-other-buffers
     ;evil-terminal-cursor-changer
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(adaptive-wrap)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
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
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Input"
                               :size 12.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ":"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

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
    (let ((last-pos (point)))
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

(defun pcl/blend-backgrounds (base tint subtlety)
  (let ((base (color-name-to-rgb (face-attribute base :background)))
        (tint (color-name-to-rgb (face-attribute tint :background))))
    (apply 'color-rgb-to-hex
            (second (color-gradient base tint subtlety)))))

(defun pcl/get-evil-states ()
  (mapcar 'first evil-state-properties))

(defvar pcl/color-states '())

(defface pcl-inactive
  '((((class color) (min-colors 8)) :background "black"))
  "Face for inactive buffers")

(defvar-local pcl/face-remap-cookie nil)

(defun pcl/set-overlay-color (&optional state)
  (let* ((state (or state evil-next-state))
         (face (or (intern-soft (format "pcl-%s" state))
                   (intern-soft (format "spacemacs-%s-face" state)))))
    (when pcl/face-remap-cookie
      (face-remap-remove-relative pcl/face-remap-cookie)
      (setq pcl/face-remap-cookie nil))
    (if (and face (member state pcl/color-states))
        (let ((blended (pcl/blend-backgrounds 'default face 20)) )
          (setq pcl/face-remap-cookie
                (face-remap-add-relative 'default :background blended))))))

(defvar pcl/last-buffer nil)
(defun pcl/highlight-active-buffer ()
  (let ((buf (window-buffer)))
    (unless (eq buf pcl/last-buffer)
      (when (buffer-live-p pcl/last-buffer)
        (with-current-buffer pcl/last-buffer
          (pcl/set-overlay-color 'inactive)))
      (pcl/set-overlay-color evil-state)
      (setq pcl/last-buffer buf))))

(defun pcl/overlay-add-hooks (&optional local)
  (mapc (lambda (state)
          (add-hook (intern (format "evil-%s-state-entry-hook" state))
                    'pcl/set-overlay-color nil local))
        (pcl/get-evil-states)))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

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

  ;; Hy stuff
  (spacemacs/set-leader-keys-for-major-mode 'hy-mode
    "ee" 'lisp-eval-last-sexp
    "eE" 'pcl/eval-line-sexp
    "ef" 'lisp-eval-defun
    "eF" 'pcl/eval-defun-not-comment
    "er" 'lisp-eval-region
    "eb" (lambda () (interactive) (lisp-eval-region (point-min) (point-max)))
    "ix" 'pcl/indent-sexp
    "if" 'pcl/indent-defun)

  ;; Add smartparens-strict-mode to all sp-lisp-modes hooks.
  ;; from https://gitlab.com/mordocai/emacs.d/blob/master/packages/smartparens.el
  ;; 2018-03-31 mordocai disappeared from internet, only trace at thinkingbicycle.net

  (add-to-list 'sp-lisp-modes 'hy-mode)

  (dolist (mode sp-lisp-modes)
    (let ((hook-sym (intern (format "%s-hook" (symbol-name mode)))))
      (add-hook hook-sym #'smartparens-strict-mode)
      (add-hook hook-sym #'evil-cleverparens-mode)))

  (pcl/overlay-add-hooks)
  (setq pcl/color-states '(lisp emacs hybrid replace visual insert inactive))
  (add-hook 'post-command-hook 'pcl/highlight-active-buffer)

  (setq clojure-enable-fancify-symbols nil)
  (setq clojure-align-forms-automatically t)

  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

  ;; fix PATH when run as service on nixos
  (setenv "PATH" (concat (getenv "PATH") ":/run/current-system/sw/bin"))
  (setq exec-path (append exec-path '("/run/current-system/sw/bin")))

  ;; I should maybe learn to push SPC f s instead
  (evil-ex-define-cmd "W" "w")

  ;; terminal stuff
  (xterm-mouse-mode -1)
  (add-hook 'spaceline-pre-hook #'pcl/fix-powerline)
  (add-hook 'terminal-init-xterm-hook #'pcl/fix-terminal-keys)

  ;; Hack to disable GUI theme reload: unsafely assume last hook is the one that reloads theme
  ;(setq spacemacs--after-display-system-init-list (butlast spacemacs--after-display-system-init-list))
  ;; reload theme when terminal client connects so colors aren't terrible
  (add-hook 'terminal-init-xterm-hook (lambda () (load-theme spacemacs--cur-theme t)))

  (spacemacs/set-leader-keys "SPC" 'avy-goto-char)
  (spacemacs/set-leader-keys "(" (lambda () (interactive) (avy-goto-char (string-to-char "("))))

  (evil-define-key 'normal clojure-mode-map
    "(" 'sp-next-sexp
    ")" 'evil-next-close-paren)

  (define-key evil-lisp-state-map "L" (evil-lisp-state-enter-command sp-next-sexp))
  (define-key evil-lisp-state-map "a" (lambda () (interactive) (sp-forward-sexp) (evil-insert-state)))

  (defun rubocop-bundled-p () "don't do it" nil)

  (setq-default evil-escape-key-sequence "kj")

  (add-hook 'prog-mode-hook 'turn-on-fci-mode)
  (add-hook 'text-mode-hook 'turn-on-fci-mode)

  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-pop-to-buffer-on-connect t)
  (defun cider-repl--banner ()
    "Generate the welcome REPL buffer banner."
    (let ((host (cider--connection-host (current-buffer)))
          (port (cider--connection-port (current-buffer))))
      (format ";; Connected to nREPL server - nrepl://%s:%s
;; CIDER %s, nREPL %s
;; Clojure %s, Java %s"
              host
              port
              (cider--version)
              (cider--nrepl-version)
              (cider--clojure-version)
              (cider--java-version))))

  (defun pcl/pretty-org-mode ()
    ;; non-monospace font
    (mixed-pitch-mode)

    ;; pretty org mode headers
    (dotimes (n 5)
      (set-face-attribute
       (intern (concat "org-level-" (number-to-string (1+ n))))
       nil
       :weight 'bold
       :foreground "#d9d8df"
       :background "#212026"
       :underline nil
       :height (max 0.8 (- 1.3 (* 0.2 n)))
       :family "Caladea"))

    (set-face-attribute 'org-level-1
                        nil
                        :underline nil
                        :foreground "#ececef"
                        :background "#212026"
                        :box '(:line-width 1 :color "#393b3e") )

    (set-face-attribute 'org-document-title nil
                        :weight 'bold
                        :foreground "#cccccc"
                        :underline t
                        :height 1.5
                        :family "Caladea")

    (set-face-attribute 'org-quote nil
                        :foreground "#cccccc"
                        :slant 'italic
                        :inherit '(variable-pitch org-block))

    ;; use fixed pitch font for indents so they are wider than
    (set-face-attribute 'org-indent nil
                        :height 0.7
                        :inherit '(org-hide fixed-pitch))

    (set-face-attribute 'org-ellipsis nil
                        :height 0.6
                        :underline nil
                        :foreground "#ffffff")

    (setq line-spacing nil)

    ;; wrapping - need to toggle VLN to make it work right
    (spacemacs/toggle-visual-line-navigation-off)

    (fci-mode 0)
    (visual-fill-column-mode 1)
    (visual-line-mode 1)

    (spacemacs/toggle-visual-line-navigation-on)

    ;; hide bullets
    (setq org-bullets-bullet-list '(" ")
          org-bullets-face-name 'org-hide)

    (org-bullets-mode 1))

  (add-hook 'org-mode-hook #'pcl/pretty-org-mode)

  (setq ;; pretty ellipsis
        org-ellipsis "⮷"

        ;; latex entities (\to \cup \cap etc), ^super, _sub
        org-pretty-entities-include-sub-superscripts t
        org-pretty-entities t

        ;; use org-indent-mode by default
        org-startup-indented t

        ;; don't show ex slashes on /italic/
        ;; note: breaks alignment inside table
        org-hide-emphasis-markers t

        ;; less indentation
        ;; don't need this with zero-width space
        ;;org-indent-indentation-per-level 1

        ;; make first line align with following lines
        ;; em space
        ;;org-indent-boundary-char 8195
        ;; zero-width space
        ;; lines up correct with 0.7 height org-indent
        org-indent-boundary-char 65279

        ;; for pretty quotes
        org-fontify-quote-and-verse-blocks 1

        ;; show blank lines between headings
        ;; actually I like default of 2 better
        ;;org-cycle-separator-lines 1
        org-cycle-separator-lines 2

        ;; make heading background go for whole line
        org-fontify-whole-heading-line t
        )

  ;; pretty bullet lists
  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([-*]\\) "
      (0 (prog1 () (compose-region (match-beginning 1)
                                   (match-end 1)
                                   "•"))))))

  ;; make latex bigger
  (with-eval-after-load 'org
    (setq org-format-latex-options (plist-put org-format-latex-options
                                              :scale 1.5)))
 
  (use-package xterm-color))

(defun lisp-state-insert-sexp-after (&optional arg)
  "Insert sexp after the current one."
  (interactive "P")
  (let ((sp-navigate-consider-symbols nil))
    (if (char-equal (char-after) ?\() (forward-char))
    (sp-up-sexp)
    (evil-insert-state)
    (dotimes (_ (if arg (1+ arg) 1))
      (sp-newline))
    (sp-insert-pair "(")))

(defun lisp-state-insert-sexp-before (&optional arg)
  "Insert sexp before the current one."
  (interactive "P")
  (if (char-equal (char-after) ?\() (forward-char))
  (sp-backward-up-sexp)
  (evil-insert-state)
  (save-excursion
    (dotimes (_ (if arg (1+ arg) 1))
      (sp-newline)))
  (insert " ")
  (sp-insert-pair "(")
  (indent-for-tab-command)
  (save-excursion
    (evil-next-visual-line)
    (indent-for-tab-command)))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flyspell-correct-helm flyspell-correct auto-dictionary spray evil-cleverparens posframe mixed-pitch helm-w3m w3m org-category-capture org-mime ghub let-alist goto-chg undo-tree enh-ruby-mode web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc company-tern dash-functional tern coffee-mode yaml-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv projectile-rails rake minitest feature-mode chruby bundler inf-ruby winum fuzzy diminish seq csv-mode auto-dim-other-buffers nix-mode helm-nixos-options company-nixos-options nixos-options xterm-color yapfify sql-indent rainbow-mode rainbow-identifiers pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda color-identifiers-mode anaconda-mode pythonic uuidgen pug-mode org-projectile org-download link-hint intero hlint-refactor hide-comnt helm-hoogle git-link eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff dumb-jump f company-ghci column-enforce-mode clojure-snippets ws-butler window-numbering web-mode volatile-highlights visual-fill-column vi-tilde-fringe toc-org tagedit spaceline powerline smooth-scrolling smeargle slim-mode shm scss-mode sass-mode restart-emacs rainbow-delimiters popwin persp-mode pcre2el paradox page-break-lines orgit org-repo-todo org-present org-pomodoro alert log4e gntp org-plus-contrib org-bullets open-junk-file neotree move-text mmm-mode markdown-toc markdown-mode magit-gitflow macrostep lorem-ipsum linum-relative leuven-theme less-css-mode jade-mode info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo hindent highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile helm-gitignore request helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets haml-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md flycheck-pos-tip flycheck-haskell flycheck flx-ido flx fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-args evil-anzu anzu emmet-mode elisp-slime-nav diff-hl define-word company-web web-completion-data company-statistics company-quickhelp pos-tip company-ghc ghc haskell-mode company-cabal company cmm-mode clj-refactor hydra inflections edn multiple-cursors paredit s peg clean-aindent-mode cider-eval-sexp-fu eval-sexp-fu highlight cider spinner queue pkg-info clojure-mode epl buffer-move bracketed-paste auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup quelpa package-build use-package which-key bind-key bind-map evil spacemacs-theme)))
 '(ruby-align-chained-calls t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(variable-pitch ((t (:height 160 :family "Calibri")))))

