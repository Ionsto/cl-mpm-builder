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
                         (when (not (typep new-value (closer-mop:slot-definition-type slot)))
                           (error "Wrong type"))
                         (setf
                          (closer-mop:slot-value-using-class
                           (closer-mop:ensure-finalized (class-of obj))
                           obj slot)
                          new-value)
                         (update-data obj)
                         (setf (background-color input)  "white"))
                     (error (c)
                       (setf (background-color input) "red"))))))
        (set-on-event input "change" #'change)
        (set-on-event input "keyup"  #'change)
        (set-on-event input "paste"  #'change)))))

(defun make-slot-selection (pane obj slot)
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
            (create-select
             pane
             :class "w3-input"))
          (possible-values (list 'cl-mpm/particle::particle-elastic
                                 'cl-mpm/particle::particle-mc)))
      (dolist (v possible-values)
        (let ((value-content
                (let ((*print-readably* t))
                  (let ((s (make-string-output-stream)))
                    (print v s)
                    (get-output-stream-string s)))))
          (create-option input :value value-content :content value-content)))
      ;; (labels ((change (dummy)
      ;;            (progn
      ;;              (handler-case
      ;;                  (let ((new-value (read-from-string (text-value input))))
      ;;                    (when (not (typep new-value (closer-mop:slot-definition-type slot)))
      ;;                      (error "Wrong type"))
      ;;                    (setf
      ;;                     (closer-mop:slot-value-using-class
      ;;                      (closer-mop:ensure-finalized (class-of obj))
      ;;                      obj slot)
      ;;                     new-value)
      ;;                    (update-data obj)
      ;;                    (setf (background-color input)  "white"))
      ;;                (error (c)
      ;;                  (setf (background-color input) "red"))))))
      ;;   (set-on-event input "change" #'change)
      ;;   (set-on-event input "keyup"  #'change)
      ;;   (set-on-event input "paste"  #'change))
      )))

(defmethod make-slot-viewer (pane (type (eql :SELECTION)) obj slot slot-name)
  (make-slot-selection pane obj slot))
(defmethod make-slot-viewer (pane (type (eql :SLOT)) obj slot (slot-name (eql 'CL-MPM-BUILDER::MP-TYPE)))
  ;; (pprint "Making mp-tyep slot")
  (make-slot-selection pane obj slot))

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
      )))

(defmethod create-propertiy-view (root (obj mp-block))
  (let* ((pane root)
         (slots-to-remove '(mp-type
                            ;; size
                            ))
         (slot-list
           (set-exclusive-or
            (closer-mop:class-slots (class-of obj))
            (closer-mop:class-slots (closer-mop:ensure-finalized (find-class 'sim-rep)))
            :test (lambda (a b) (equal (closer-mop:slot-definition-name a)
                                       (closer-mop:slot-definition-name b))))))
    ;; (dolist (srm slots-to-remove)
    ;;   (setf slot-list (delete srm slot-list :test (lambda (a b) (equal a (closer-mop:slot-definition-name b))))))
    ;; (make-slot-viewer pane :SLOT obj slot (closer-mop:slot-definition-name slot))
    (dolist (slot slot-list)
      (create-div
       pane
       :content (format nil "~A" (closer-mop:slot-definition-name slot)))
      (pprint (closer-mop:slot-definition-name slot))
      (make-slot-viewer pane :SLOT obj slot (closer-mop:slot-definition-name slot)))))
