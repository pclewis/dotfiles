(defvar pcl-packages '())

(with-eval-after-load 'smartparens
  (add-to-list 'sp-lisp-modes 'hy-mode))

(add-to-list 'pcl-packages 'mixed-pitch)

(defun pcl/init-mixed-pitch ()
  (use-package mixed-pitch))

(add-to-list 'pcl-packages 'visual-fill-column)
(defun pcl/init-visual-fill-column ()
  (use-package visual-fill-column))

(add-to-list 'pcl-packages '(adaptive-wrap :excluded t))
