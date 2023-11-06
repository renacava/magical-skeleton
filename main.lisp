(in-package :magical-skeleton)

(defun start ()
  (clui:start))

(defun make-main-menu ()
  (clui:shape-instance 'basic-image :instance-name 'main-menu-bg :image-path "main-menu.png" :x (lambda () (clui::half clui:*window-width*)) :y (lambda () (clui::half clui:*window-height*))))
