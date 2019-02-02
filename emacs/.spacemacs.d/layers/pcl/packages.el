(defvar pcl-packages '())

(add-to-list 'pcl-packages '(re-jump :location local))

(defun pcl/init-re-jump ()
  (use-package re-jump))

(with-eval-after-load 'smartparens
  (add-to-list 'sp-lisp-modes 'hy-mode))

(add-to-list 'pcl-packages 'direnv)

(defun pcl/init-direnv ()
  (use-package direnv :config (direnv-mode)))

(add-to-list 'pcl-packages 'mixed-pitch)

(defun pcl/init-mixed-pitch ()
  (use-package mixed-pitch))

(add-to-list 'pcl-packages 'visual-fill-column)
(defun pcl/init-visual-fill-column ()
  (use-package visual-fill-column))

(add-to-list 'pcl-packages '(adaptive-wrap :excluded t))

(unless (version< emacs-version "26")
  (add-to-list 'pcl-packages '(company-posframe))
  (defun pcl/init-company-posframe ()
    (use-package company-posframe)
    (company-posframe-mode 1)))
