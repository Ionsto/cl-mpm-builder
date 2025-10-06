(in-package :cl-mpm-builder)

(defclass sim-rep (clog-obj)
  ())

(defclass meta-simulation ()
  ((sim
    )
   (mp-blocks
    )
   (sdfs
    )
   ))



(defclass simulation (sim-rep)
  ((sim-size
    :initform (list 10d0 10d0)
    :type list
    :accessor sim-size)
   (mesh-resolution
    :initform 0.25d0
    :type number
    :accessor sim-mesh-resolution)
   (sim-type
    :initform :EXPLICIT
    :accessor sim-type)
   (sim-bcs
    :initform (list :left (list 0 nil)
                    :bottom (list nil 0))
    :type list
    :accessor sim-bcs)
   (sim-args-list
    :initform '(:enable-split t
                :split-factor 0.5d0
                :enable-aggregate t
                :enable-fbar t)
    :type list
    :accessor sim-args-list)
   ))

(defclass simulation-dynamic (simulation)
  ((dt-steps
    :initform 0.1d0
    :type number
    :accessor sim-dt-steps)
   (total-time
    :initform 10d0
    :type number
    :accessor sim-dt-total)
   (damping-factor
    :initform 1d-3
    :type number
    :accessor sim-damping-factor)
   (dt-scale
    :initform 0.9d0
    :type number
    :accessor sim-dt-scale)))

(defclass simulation-quasi-static (simulation)
  ((load-steps
    :initform 100
    :accessor sim-load-steps)
   (residual-criteria
    :initform 1d-3
    :accessor sim-residual-criteria)))

(defclass mp-block (sim-rep)
  ((name
    :initform "mps"
    :accessor mp-block-name)
   (position
    :initform (list 0d0 0d0)
     :accessor mp-block-position)
   (mp-count
    :initform 2
    :accessor mp-mps-count)
   (mp-density
    :initform 1d3
    :accessor mp-block-density)
   (mp-e
    :initform 1d6
    :accessor mp-e)
   (mp-nu
    :initform 0.2d0
    :accessor mp-nu)
   (mp-rho
    :initform 20d3
    :accessor mp-rho)
   (mp-enable-plasticity
    :initform t
    :accessor mp-plasticity)
   (size
    :initform (list 1d0 1d0)
    :accessor mp-block-size)))


(defmethod (setf mp-block-size) (v (instance mp-block))
  (when (and (typep v 'list)
             (= (length v) 2))
    (destructuring-bind (x y) v
        (when (and (numberp x)
                   (numberp y))
          (setf (slot-value instance 'size) v)))))

(defmethod (setf mp-block-position) (v (instance mp-block))
  (setf (slot-value instance 'position) v)
  (when (and (typep v 'list)
             (= (length v) 2))
    (destructuring-bind (x y) v
      (when (and (numberp x)
                 (numberp y))
        (setf (slot-value instance 'position) v)))))
