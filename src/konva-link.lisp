(in-package :cl-mpm-builder)

(defgeneric delete-element (obj))
(defmethod delete-element ((obj simulation)))
(defmethod delete-element ((obj sim-rep))
  (let ((clog-id (html-id obj)))
    (setf *rect-list* (delete clog-id *rect-list*))
    (remhash (format nil "~A" clog-id) *stage-list*)
    (js-execute *canvas*
                (format nil "var c = clog['~A'];
c.destroy();
    tr.nodes([]);
"
                        clog-id))))

(defmethod initialize-instance :after ((instance sim-rep) &rest initargs)
  (update-data instance)
  (set-on-event
   instance
   "update"
   (lambda (obj)
     (get-data instance)
     (update-properties)
     (update-data instance)))
  )

(defmethod update-data ((instance mp-block))
  (destructuring-bind (x y) (mp-block-size instance)
    (js-execute *canvas* (format nil "var c = clog['~A'];
c.setScale({x:1,y:1});
c.setSize({width:~F,height:~F});"
                                 (html-id instance)
                                 x
                                 y)))
  (destructuring-bind (x y) (mp-block-position instance)
    (js-execute *canvas* (format nil "let c =clog['~A'];
c.setPosition({x:~F,y: sceneHeight-(~F + (c.height()))});"
                                 (html-id instance)
                                 x
                                 y))))

(defmethod update-data ((instance simulation))
  (let ((st (sim-type instance)))
    (cond
      ((eq st :EXPLICIT) (change-class instance 'simulation-dynamic))
      ((eq st :IMPLICIT) (change-class instance 'simulation-quasi-static))
      (t nil)))
  (js-execute *canvas* (format nil "window.change_domain_size(~F,~F);"
                               (first (sim-size instance))
                               (second (sim-size instance))))
  )



(defmethod get-data (object))

(defmethod get-data ((object mp-block))
  (setf
   (mp-block-position object) (get-mps-position (html-id object))
   (mp-block-size object) (get-mps-size (html-id object))))

(defun get-mps-position (rect)
  (list
   (max 0d0 (parse-float:parse-float (js-query *canvas* (format nil "
c = clog['~A'];
console.log(c.position());
c.x();" rect))))
   (max 0d0 (parse-float:parse-float (js-query *canvas* (format nil "c=clog['~A']; sceneHeight-(c.y() + (c.height()*c.scaleY()));" rect rect))))))

(defun get-mps-size (rect)
  (list
   (parse-float:parse-float (js-query *canvas* (format nil "c=clog['~A'];c.width()*c.scaleX();" rect)))
   (parse-float:parse-float (js-query *canvas* (format nil "c=clog['~A'];c.height()*c.scaleY();" rect)))))

(defun get-mps-data (rect)
  (list :size (get-mps-size rect)
        :position (get-mps-position rect)
        :density 1d3
        ))
