(in-package :cl-mpm-builder)

(defgeneric make-slot-viewer (pane type instance slot slot-name))

(defmethod make-slot-viewer (pane (type (eql :SLOT)) obj slot slot-name)
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
                         (pprint (type-of new-value))
                         (when (not (typep new-value (closer-mop:slot-definition-type slot)))
                           (error "Wrong type"))
                         (setf
                          (closer-mop:slot-value-using-class
                           (closer-mop:ensure-finalized (class-of obj))
                           obj slot)
                          new-value)
                         (update-data obj)
                         (setf (background-color input)  "white")
                         )
                     (error (c)
                       (print "Error processing string")
                       (setf (background-color input) "red"))))))
        (set-on-event input "change" #'change)
        (set-on-event input "keyup"  #'change)
        (set-on-event input "paste"  #'change)
        ))))

;; (defmethod make-slot-viewer ((type (eql :BOOL)) instance slot-definition slot-name)

;;   )

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
       :content (format nil "~A" (closer-mop:slot-definition-name slot)))

      (make-slot-viewer pane :SLOT obj slot (closer-mop:slot-definition-name slot))
      ;; (let ((slot-value
      ;;         (let ((*print-readably* t))
      ;;           (let ((s (make-string-output-stream)))
      ;;             (print (if (closer-mop:slot-boundp-using-class
      ;;                          (class-of obj)
      ;;                          obj slot)
      ;;                         (closer-mop:slot-value-using-class
      ;;                          (class-of obj)
      ;;                          obj slot)
      ;;                         "UNBOUND") s)
      ;;             (get-output-stream-string s)))))
      ;;   (let ((input
      ;;           (create-text-area
      ;;            pane
      ;;            :class "w3-input"
      ;;            :value slot-value)))
      ;;     (labels ((change (dummy)
      ;;                (progn
      ;;                  (handler-case
      ;;                      (let ((new-value (read-from-string (text-value input))))
      ;;                        (setf
      ;;                         (closer-mop:slot-value-using-class
      ;;                          (closer-mop:ensure-finalized (class-of obj))
      ;;                          obj slot)
      ;;                         new-value)
      ;;                        (update-data obj))
      ;;                    (error (c)
      ;;                      (print "Error processing string"))))))
      ;;       (set-on-event input "change" #'change)
      ;;       (set-on-event input "keyup"  #'change)
      ;;       (set-on-event input "paste"  #'change)
      ;;       )
      ;;     ))
      )))
