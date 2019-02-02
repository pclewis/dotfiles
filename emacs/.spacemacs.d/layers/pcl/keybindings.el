(pcl/bind-clojure-keys "eE" 'pcl/eval-line-sexp)

(pcl/bind-clojure-keys "eF" 'pcl-clojure/eval-defun-not-comment)

(pcl/bind-clojure-keys "ix" 'pcl-clojure/indent-sexp)

(pcl/bind-clojure-keys "if" 'pcl-clojure/indent-defun)

(pcl/bind-clojure-keys
  "c{" 'clojure-convert-collection-to-map
  "c#" 'clojure-convert-collection-to-set
  "c(" 'clojure-convert-collection-to-list
  "c'" 'clojure-convert-collection-to-quoted-list
  "c[" 'clojure-convert-collection-to-vector)

(evil-define-key 'normal clojure-mode-map
  "(" 'sp-next-sexp
  ")" 'evil-next-close-paren)

(pcl/bind-clojure-keys "sS" 'cider-repl-set-ns)

(pcl/bind-clojure-keys "gf" 're-frame-jump-to-reg)

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

(define-key evil-insert-state-map (kbd "C-\"")
  (lambda () (interactive) (sp-wrap-with-pair "\"")))

(define-key evil-insert-state-map (kbd "C-(")
  (lambda () (interactive) (sp-wrap-with-pair "(")))

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "js" 'org-babel-demarcate-block)

(spacemacs|add-toggle org-confirm-babel-evaluate
  :status org-confirm-babel-evaluate
  :on (setq org-confirm-babel-evaluate 't)
  :off (setq org-confirm-babel-evaluate nil)
  :documentation "Confirm evaluation of code using babel."
  :evil-leader "tb")
