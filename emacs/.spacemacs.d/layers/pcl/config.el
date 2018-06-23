(setq clojure-align-forms-automatically t)

(setq cider-repl-display-help-banner nil)
(setq cider-repl-pop-to-buffer-on-connect t)
(with-eval-after-load 'cider
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
              (cider--java-version)))))

(setq cider-lein-parameters "repl :headless :host localhost")

(setq cider-jack-in-dependencies '(("nrepl" "0.3.1")))

(setq ruby-align-chained-calls t)

(with-eval-after-load 'smartparens
  (dolist (mode sp-lisp-modes)
    (let ((hook-sym (intern (format "%s-hook" (symbol-name mode)))))
      (add-hook hook-sym #'smartparens-strict-mode))))

(with-eval-after-load 'evil
  (evil-ex-define-cmd "W" "w"))

(add-hook 'prog-mode-hook 'turn-on-fci-mode)
(add-hook 'text-mode-hook 'turn-on-fci-mode)

(setq auto-completion-return-key-behavior nil
      auto-completion-tab-key-behavior nil)

(setq-default tab-always-indent t)

(with-eval-after-load 'evil (pcl/overlay-add-hooks))
(add-hook 'post-command-hook 'pcl/highlight-active-buffer)

(add-hook 'org-mode-hook #'mixed-pitch-mode)

(add-hook 'org-mode-hook #'pcl.org/fixed-pitch-indents)

(setq org-indent-boundary-char 65279)

(add-hook 'org-mode-hook #'pcl.org/pretty-headers)
(setq org-fontify-whole-heading-line t)

(add-hook 'org-mode-hook #'pcl.org/pretty-quotes)
(setq org-fontify-quote-and-verse-blocks 1)

(add-hook 'org-mode-hook #'pcl.org/pretty-ellipsis)
(setq org-ellipsis "⮷")

;; pretty bullet lists
(font-lock-add-keywords
  'org-mode
  '(("^ +\\([-*]\\) "
    (0 (prog1 () (compose-region (match-beginning 1)
                                  (match-end 1)
                                  "•"))))))

(add-hook 'org-mode-hook #'pcl.org/wrap)

(setq org-hide-emphasis-markers t)

(setq org-startup-indented t)

(with-eval-after-load 'org
  (setq org-format-latex-options (plist-put org-format-latex-options
                                            :scale 1.5)))

(setq org-pretty-entities t
      org-pretty-entities-include-sub-superscripts t)

(setq org-edit-src-content-indentation 0)

(setq org-src-tab-acts-natively t)

(with-eval-after-load 'org
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((shell      . t)
      (emacs-lisp . t)
      (clojure    . t))))

(when (and (string-equal system-type "gnu/linux")
           (file-directory-p "/run/current-system/sw/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":/run/current-system/sw/bin"))
  (setq exec-path (append exec-path '("/run/current-system/sw/bin"))))

(set-face-attribute 'variable-pitch nil
                    :height 160 :family "Calibri")
