(defun pcl/bind-clojure-keys (&rest bindings)
  (dolist (m '(clojure-mode clojurec-mode clojurescript-mode))
    (apply #'spacemacs/set-leader-keys-for-major-mode m bindings)))

(defun pcl/eval-line-sexp ()
  (interactive)
  (let ((evil-move-beyond-eol t))
    (save-excursion (execute-kbd-macro (kbd "$,ee")))))

(defun pcl-clojure/eval-defun-not-comment ()
  (interactive)
  (save-excursion
    (let ((last-pos (point)))
      (condition-case nil
          (while (not (string-equal "(comment"
                                    (buffer-substring-no-properties
                                     (point) (+ (point) 8))))
            (print (buffer-substring-no-properties (point) (+ (point) 8)))
            (setq last-pos (point))
            (backward-up-list 1 t t))
        (scan-error nil))
      (goto-char last-pos)
      (execute-kbd-macro (kbd "va(,er"))
      (evil-exit-visual-state))))

(defun pcl-clojure/indent-sexp ()
  (interactive)
  (save-excursion
    (execute-kbd-macro (kbd "va(=va("))
    (call-interactively 'clojure-align)
    (evil-exit-visual-state)))

(defun pcl-clojure/indent-defun ()
  (interactive)
  (sp-indent-defun)
  (call-interactively 'clojure-align))

(defun rubocop-bundled-p () "don't do it" nil)

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

(defvar pcl/color-states '(lisp emacs hybrid replace visual insert inactive))
(defvar pcl/last-buffer nil)
(defvar-local pcl/face-remap-cookie nil)

(defface pcl-inactive
  '((((class color) (min-colors 8)) :background "black"))
  "Face for inactive buffers")

(defun pcl/get-evil-states ()
  (mapcar 'first evil-state-properties))

(defun pcl/blend-backgrounds (base tint subtlety)
  (let ((base (color-name-to-rgb (face-attribute base :background)))
        (tint (color-name-to-rgb (face-attribute tint :background))))
    (apply 'color-rgb-to-hex
            (second (color-gradient base tint subtlety)))))

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

(defun pcl.org/fixed-pitch-indents ()
  (set-face-attribute 'org-indent nil
                      :height 0.7
                      :inherit '(org-hide fixed-pitch)))

(defun pcl.org/pretty-headers ()
  (dotimes (n 5)
    (set-face-attribute
     (intern (concat "org-level-" (number-to-string (1+ n))))
     nil
     :weight 'bold
     :foreground "#d9d8df"
     :background "#212026"
     :height (max 0.8 (- 1.3 (* 0.2 n)))
     :family "Caladea"))

  (set-face-attribute 'org-level-1
                      nil
                      :foreground "#ececef"
                      :background "#212026"
                      :box '(:line-width 1 :color "#393b3e"))

  (set-face-attribute 'org-document-title nil
                      :weight 'bold
                      :foreground "#cccccc"
                      :underline t
                      :height 1.5
                      :family "Caladea")

  ;; hide bullets
  (setq org-bullets-bullet-list '(" ")
        org-bullets-face-name 'org-hide)

  (org-bullets-mode 1))

(defun pcl.org/pretty-quotes ()
  (set-face-attribute 'org-quote nil
                      :foreground "#cccccc"
                      :slant 'italic
                      :inherit '(variable-pitch org-block)))

(defun pcl.org/pretty-ellipsis ()
  (set-face-attribute 'org-ellipsis nil
                      :height 0.6
                      :underline nil
                      :foreground "#ffffff"))

(defun pcl.org/wrap ()
  ;; need to toggle VLN to make it work right
  (spacemacs/toggle-visual-line-navigation-off)

  (fci-mode 0)
  (visual-fill-column-mode 1)
  (visual-line-mode 1)

  (spacemacs/toggle-visual-line-navigation-on))
