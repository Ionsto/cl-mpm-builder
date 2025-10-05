(in-package :cl-mpm-builder)
(defgeneric create-propertiy-view (root obj))
(defmethod create-propertiy-view (root obj))
(defmethod create-propertiy-view (root (obj sim-rep))
  (let ((pane root))
    (dolist (slot
             (set-exclusive-or
              (closer-mop:class-slots (class-of obj))
              (closer-mop:class-slots (closer-mop:ensure-finalized (find-class 'sim-rep)))
              :test (lambda (a b) (equal (closer-mop:slot-definition-name a)
                                         (closer-mop:slot-definition-name b)))
              ))
      (create-div
       pane
       ;:class "w3-container w3-aqua"
       :content (format nil "~A" (closer-mop:slot-definition-name slot)))
      (let ((slot-value
              (let ((*print-readably* t))
                (let ((s (make-string-output-stream)))
                  (print (if (closer-mop:slot-boundp-using-class
                               (class-of obj)
                               obj slot)
                              (closer-mop:slot-value-using-class
                               (class-of obj)
                               obj slot)
                              "UNBOUND") s)
                  (get-output-stream-string s)))))
        (let ((input
                (create-text-area
                 pane
                 :class "w3-input"
                 :value slot-value)))
          (labels ((change (dummy)
                     (progn
                       (handler-case
                           (let ((new-value (read-from-string (text-value input))))
                             (setf
                              (closer-mop:slot-value-using-class
                               (closer-mop:ensure-finalized (class-of obj))
                               obj slot)
                              new-value)
                             (update-data obj))
                         (error (c)
                           (print "Error processing string"))))))
            (set-on-event input "change" #'change)
            (set-on-event input "keyup"  #'change)
            (set-on-event input "paste"  #'change)
            )
          )))))
