(in-package :cl-mpm-builder)


(defclass render-manager ()
  ((data
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor render-manager-data)
   (steps
    :initform 0
    :accessor render-manager-steps)))

(defmethod initialize-instance :after ((obj render-manager) &rest args)
  (update-render-timeline obj))

(defun cache-render (data)
  (incf (render-manager-steps *render-manager*))
  (vector-push-extend data (render-manager-data *render-manager*))
  (update-render-timeline *render-manager*)
  )

(defun update-render (i)
  (when (and (>= i 0)
             (< i (render-manager-steps *render-manager*)))
    (render-mps-array (aref (render-manager-data *render-manager*) i))))

(defun reset-render-manager (rm)
  (setf (fill-pointer (render-manager-data rm)) 0
        (render-manager-steps rm) 0)
  (update-render-timeline rm))

(defun update-render-timeline (obj)
  (js-execute *cad-window* (format nil "
c=document.getElementById('~A');
c.max = ~D-1;"
                               (clog:html-id *sim-timeline*)
                               (render-manager-steps obj))))

(defun reset-render-canvas (sim)
  (let ((div *canvas*))
    (js-query div "render.destroyChildren();")))



(defun render-mps-to-array (sim)
  (let* ((mp-count (length (cl-mpm:sim-mps sim)))
         (floats-per-mp 5)
         (data-array (make-array (* mp-count floats-per-mp) :initial-element 0e0
                                 :element-type 'single-float)))
    (lparallel:pdotimes (i mp-count)
      (let ((mp (aref (cl-mpm:sim-mps sim) i)))
          (let* ((width  (cl-mpm/utils::varef (cl-mpm/particle::mp-domain-size mp) 0))
                 (height (cl-mpm/utils::varef (cl-mpm/particle::mp-domain-size mp) 1))
                 (x (- (cl-mpm/utils::varef (cl-mpm/particle:mp-position mp) 0) (* width 0.5d0)))
                 (y (+ (cl-mpm/utils::varef (cl-mpm/particle:mp-position mp) 1) (* height 0.5d0)))
                 (data 0d0))
            (let ((base-index (* floats-per-mp i)))
              (setf
               (aref data-array (+ base-index 0)) (float x 0e0)
               (aref data-array (+ base-index 1)) (float y 0e0)
               (aref data-array (+ base-index 2)) (float width 0e0)
               (aref data-array (+ base-index 3)) (float height 0e0)
               (aref data-array (+ base-index 4)) (float data 0e0))))))
    data-array))

(defun render-mps-array (data-array)
  (let ((div *canvas*)
        (floats-per-mp 5))
    (js-execute div
              (format nil "
data = [~{~F~^, ~}];
function render_data(data){
    render.destroyChildren();
    let fpmp = 5;
    for(let i = 0;i < data.length;i+=fpmp){
    mp = new Konva.Rect({
    x: data[i],
    y: sceneHeight - data[i+1],
    width: data[i+2],
    height: data[i+3],
    fill: 'black',
    draggable: false,
    zindex: 100,
      name: 'mp'
    });
    mp.name = 'mp';
    render.add(mp);
    }
}
render_data(data);
  " (coerce data-array 'list)
  ))
    ))


(defun render-mps (sim)
  (let ((data (render-mps-to-array sim)))
    (cache-render data)
    (render-mps-array data)))
