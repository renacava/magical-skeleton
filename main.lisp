(in-package :magical-skeleton)

(defun start ()
  (clui:start :outer-package *package*)
  (clui:set-bg-colour (list 0 0 0))
  (make-main-menu))

(defun set-bg (bg-image-path)
  (clui:shape-instance 'basic-image
                       :instance-name 'bg
                       :image-path bg-image-path
                       :x (lambda () (clui::half clui:*window-width*))
                       :y (lambda () (clui::half clui:*window-height*))
                       :width (lambda ()  clui:*window-width*)
                       ))
(defparameter current-scene nil)

(defun make-main-menu ()
  (clui:remove-all-instances)
  (setf current-scene 'main-menu)
  (clui:input-register-keypress 'escape (lambda ()
                                          (when (eq current-scene 'main-menu)
                                            (clui:exit))))
  (set-bg "assets/images/main-menu.png")
  (clui:shape-instance 'basic-text
                       :instance-name 'title-text
                       :text-string "Magical Skeleton"
                       :x (lambda () (clui::half clui:*window-width*))
                       :y (lambda () (+ (* 10 (sin (clui:get-real-time-seconds))) (* 0.9 clui:*window-height*)))
                       :align-center t
                       :scale (lambda () (+ 2.0 (* 0.3 (sin (+ 15 (clui:get-real-time-seconds))))))
                       :colour (lambda () (list (abs (sin (clui:get-real-time-seconds)))
                                                (abs (sin (+ 5 (clui:get-real-time-seconds))))
                                                (abs (sin (+ 7 (clui:get-real-time-seconds)))))))

  (clui:shape-instance 'basic-button
                       :instance-name 'start-button
                       :button-text "START"
                       :on-pressed #'enter-game-world
                       :x (lambda () (clui::half clui:*window-width*))
                       :y (lambda () (clui::half clui:*window-height*))
                       :min-width 300)

  (clui:shape-instance 'basic-button
                       :instance-name 'quit-button
                       :button-text "QUIT"
                       :on-pressed (lambda () (clui:exit))
                       :x (lambda () (clui::half clui:*window-width*))
                       :y (lambda () (- (clui::half clui:*window-height*) 100))
                       :min-width 300)
  (clui::play-music "assets/music/piano-copyright.wav"))

(defparameter *day-time* (get-internal-real-time))
(defparameter *timescale* 5)

(defun reset-day-time ()
  (setf *day-time* (get-internal-real-time)))

(defparameter stats nil)

(defun enter-game-world ()
  (setf current-scene 'game-world)
  (clui::play-music "assets/music/everyday-fantasy.wav")
  (clui:input-register-keypress 'escape (lambda ()
                                          (when (eq current-scene 'game-world)
                                            (make-main-menu))))
  (clui:remove-all-instances)
  (reset-day-time)
  (set-bg "assets/images/copyright/class.png")
  (make-date-text)
  (make-stat "Knowledge" "Study" (make-stat-modifier "Knowledge" "Energy" 1 -1))
  (make-stat "Energy" "Rest" (make-stat-modifier "Energy" "Food" 1 -1))
  (make-stat "Food" "Eat")
  (make-settings-button))

(clui:defshape
  nil
  :children (list
             (clui:make-child-list 'basic-rect "outer-rect" :x 0 :y 0 :z 1 :width 410 :height 310 :colour '(0.4 0.4 0.4))
             (clui:make-child-list 'basic-rect "inner-rect" :x 0 :y 0 :z 2 :width 400 :height 300 :colour '(0.8 0.8 0.8))
             (clui:make-child-list 'basic-text "setting-text" :x 0 :y 103 :z 3 :scale 1.5 :align-center t :text-string "SETTINGS")
             (clui:make-child-list 'basic-button "close-btn" :x 170 :y 120 :z 4
                                                             :button-text "X"
                                                             :scale 0.8
                                                             :min-width 30
                                                             :on-pressed (lambda () (clui:remove-instance 'settings-button))
                                                             )
             )
  :name 'ms-settings-menu)

(defun make-settings-button ()
  (clui:shape-instance 'basic-button :instance-name 'settings-button
                                     :x (lambda () (- clui:*window-width* 40))
                                     :y 30
                                     :button-text "..."
                                     :scale 0.8
                                     :min-width 30
                                     :on-pressed #'make-settings-menu))

(defun make-settings-menu ()
  (clui:shape-instance 'ms-settings-menu
                       :x (lambda () (* 0.5 clui:*window-width*))
                       :y (lambda () (* 0.5 clui:*window-height*))
                       :instance-name 'settings-menu))

(defun make-stat (stat-name button-text &optional button-stat-modifier)
  (make-stat-text stat-name)
  (make-stat-button stat-name button-text button-stat-modifier))

(defun make-stat-text (stat-name)
  (setf (getf stats (clui::to-property stat-name)) 0)
  (clui:shape-instance 'basic-text :x (lambda () (- clui:*window-width* 10))
                                   :y (lambda () (- clui:*window-height* 30 (* 20 (position (clui::to-property stat-name) stats))))
                                   :align-right t
                                   :text-string (lambda () (format nil "~a: ~a" stat-name (or (getf stats (clui::to-property stat-name)) 0)))
                                   :colour clui::*clui-white*))

(defun make-stat-button (stat-name button-text &optional stat-modifier-func)
  (clui:shape-instance 'basic-button
                       :x (+ 100 (* 80 (position (clui::to-property stat-name) stats)))
                       :y 50
                       :button-text button-text
                       :on-mouse-enter (lambda () (clui:play-sound "assets/sounds/btn-hover.wav"))
                       :on-pressed (or
                                    (when stat-modifier-func
                                      (lambda () (progn (funcall stat-modifier-func)
                                                        (clui::play-sound "assets/sounds/btn-press.wav"))))
                                    (lambda () (progn
                                                 (setf (getf stats (clui:to-property stat-name))
                                                       (1+ (or (getf stats (clui:to-property stat-name)) 0)))
                                                 (clui::play-sound "assets/sounds/btn-press.wav"))))))

(defun make-stat-modifier (stat1-name stat2-name stat1-amount stat2-amount)
  (lambda () (when (and stat1-name stat1-amount)
               (setf (getf stats (clui:to-property stat1-name))
                     (+ (or (getf stats (clui:to-property stat1-name)) 0)
                        stat1-amount)))
    (when (and stat2-name stat2-amount
               (setf (getf stats (clui:to-property stat2-name))
                     (+ (or (getf stats (clui:to-property stat2-name)) 0)
                        stat2-amount))))))

(defun make-date-text ()
  (let ((y (lambda () (- clui:*window-height* 45))))
    (clui:shape-instance 'basic-rect
                         :x (lambda () (+ 15 (* 0.5 (clui::text-get-width (get-game-time-date-string)))))
                         :y (lambda () (+ 5 (funcall y)))
                         :width (lambda () (+ 30 (clui::text-get-width (get-game-time-date-string))))
                         :instance-name 'date-text-rect
                         :colour '(0.0 0.5 0.5))
    (clui:shape-instance 'basic-text
                         :x 10
                         :y y
                         :instance-name 'date-text
                         :text-string #'get-game-time-date-string)))

(defun get-realtime-seconds-since-game-start ()
  (* 0.001 (- (get-internal-real-time) *day-time*)))

(defun get-game-time-seconds ()
  (* *timescale* (get-realtime-seconds-since-game-start)))

(defun get-game-time-minutes ()
  (* 5 (get-game-time-seconds)))

(defun get-game-time-hours ()
  (float (/ (get-game-time-minutes) 60)))

(defun get-game-time-days ()
  (float (/ (get-game-time-hours) 24)))

(defun get-game-time-date ()
  (list (truncate (get-game-time-days))
        (truncate (mod (get-game-time-hours) 24))
        (truncate (mod (get-game-time-minutes) 60))
        (truncate (mod (get-game-time-seconds) 60))))

(defun get-game-time-date-string ()
  (let ((date (get-game-time-date)))
    (format nil "DAY: ~a TIME: ~a:~a"
            (first date)
            (second date)
            (third date))))

(defun get-light-level ()
  "Returns 0..1 for how light it is according to the current game time."
  )
