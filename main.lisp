(in-package :magical-skeleton)

(defun start ()
  (clui:start :outer-package *package*))

(defun make-main-menu ()
  (clui:shape-instance 'basic-image
                       :instance-name 'main-menu-bg
                       :image-path "assets/images/main-menu.png"
                       :x (lambda () (clui::half clui:*window-width*))
                       :y (lambda () (clui::half clui:*window-height*))
                       :width (lambda () clui:*window-width*)
                       :height (lambda () clui:*window-height*))
  (clui:shape-instance 'basic-text
                       :instance-name 'title-text
                       :text-string "Magical Skeleton"
                       :x (lambda () (clui::half clui:*window-width*))
                       :y (lambda () (* 0.9 clui:*window-height*))
                       :align-center t
                       :scale 2.0)

  (clui:shape-instance 'basic-button
                       :instance-name 'start-button
                       :button-text "START"
                       :on-pressed (lambda () (print "starting"))
                       :x (lambda () (clui::half clui:*window-width*))
                       :y (lambda () (clui::half clui:*window-height*))
                       :min-width 300)

  (clui:shape-instance 'basic-button
                       :instance-name 'quit-button
                       :button-text "QUIT"
                       :on-pressed (lambda () (clui:exit))
                       :x (lambda () (clui::half clui:*window-width*))
                       :y (lambda () (- (clui::half clui:*window-height*) 100))
                       :min-width 300))
