(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "eE" 'pcl/eval-line-sexp)

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "eF" 'pcl-clojure/eval-defun-not-comment)

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "ix" 'pcl-clojure/indent-sexp)

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "if" 'pcl-clojure/indent-defun)

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "cm" 'clojure-convert-collection-to-map
  "cs" 'clojure-convert-collection-to-set
  "cl" 'clojure-convert-collection-to-list
  "c'" 'clojure-convert-collection-to-quoted-list
  "cv" 'clojure-convert-collection-to-vector)

(evil-define-key 'normal clojure-mode-map
  "(" 'sp-next-sexp
  ")" 'evil-next-close-paren)

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

(define-key evil-insert-state-map (kbd "<C-up>")      'sp-raise-sexp)
(define-key evil-insert-state-map (kbd "<C-right>")   'sp-forward-slurp-sexp)
(define-key evil-insert-state-map (kbd "<C-left>")    'sp-forward-barf-sexp)
(define-key evil-insert-state-map (kbd "C-k")         'sp-kill-sexp)

(define-key evil-lisp-state-map "L" 
  (evil-lisp-state-enter-command sp-next-sexp))
(define-key evil-lisp-state-map "a"
  (lambda () (interactive)
    (sp-forward-sexp)
    (evil-insert-state)))

(setq-default evil-escape-key-sequence "kjk")

(spacemacs/set-leader-keys "SPC" 'avy-goto-char)

(spacemacs/set-leader-keys "(" 
  (lambda () (interactive)
    (avy-goto-char (string-to-char "("))))

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "js" 'org-babel-demarcate-block)
