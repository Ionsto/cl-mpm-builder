(defpackage #:cl-mpm-builder
  (:use #:cl #:clog #:clog-web)
  (:export start-app))

(in-package :cl-mpm-builder)

;(defun on-file-new (obj)
;  (let* ((app (connection-data-item obj "app-data"))
;         (win (create-gui-window obj :title "New Window")))
;    ;(declare (ignore app win))
;    ;(create-panel-4 win)
;    ;(create-div win :content "Hello World!")
;    ))
;
;(defun on-help-about (obj)
;  (let* ((about (create-gui-window obj
;                                   :title   "About"
;                                   :content "<div class='w3-black'>
;                                         <center><img src='/img/clogwicon.png'></center>
;                                         <center>cl-mpm-builder</center>
;                                         <center>cl-mpm-builder</center></div>
;                                         <div><p><center>A New App</center>
;                                         <center>(c) - Some One</center></p></div>"
;                                   :hidden  t
;                                   :width   200
;                                   :height  200)))
;    (window-center about)
;    (setf (visiblep about) t)
;    (set-on-window-can-size about (lambda (obj)
;                                    (declare (ignore obj))()))))

(defclass simulation ()
  (
   (size
   :initform (list 1d0 1d0)
   :accessor sim-size)
   (mesh-resolution
   :initform 1d0
   :accessor sim-mesh-resolution)
   (construction-tasks
     :accessor mp-construction-tasks)  
   (mp-blocks
     :accessor sim-mp-blocks)  
   (mp-sdfs
     :accessor sim-sdfs)))
     
(defclass mp-block ()
  (
   (position
     :accessor mp-block-position)  
   (size
     :accessor mp-block-size)  
  )
)

(defclass app-data ()
  ((data
    :accessor data)))
    
(defun create-property-window (div)
  (let* ((window (create-div div :style "width:10%;min-height: 80vh; background-color:blue; flex:3;"))
         (title (create-web-panel window :content "<h3>Property viewer</h3>"
                                  :class   "w3-grey"))
        )
    (set-styles title '(("min-height" "10%")))
    )
)

(defclass clog-konva-stage (clog-obj)())

(defun create-cad-window (div)
  (let ((window (create-div div :style "width:90%;min-height: 80vh;resize: horizontal;overflow: auto;")))
    ;(create-element window "svg" :viewBox "0 0 10 10" :x "2" :y "2")
    (defparameter *window* window)
    (defparameter *canvas* (make-canvas *window*))
    ))
(defun make-canvas (div)
  (js-query div (format nil "stage = new Konva.Stage({
  container: '~A',
  width: 500,
  height: 500,});
layer = new Konva.Layer();
stage.add(layer);
tr = new Konva.Transformer({rotateEnabled :false});
layer.add(tr);
stage.on('click tap', function (e) {
  // if click on empty area - remove all selections
  if (e.target === stage) {
    tr.nodes([]);
    return;
  }
  // do nothing if clicked NOT on our rectangles
  if (!e.target.hasName('rect')) {
    return;
  }
  // do we pressed shift or ctrl?
  const metaPressed = e.evt.shiftKey || e.evt.ctrlKey || e.evt.metaKey;
  const isSelected = tr.nodes().indexOf(e.target) >= 0;

  if (!metaPressed && !isSelected) {
    // if no key pressed and the node is not selected
    // select just one
    tr.nodes([e.target]);
    tr.moveToTop();
  }});
  
  "
  (html-id div)))
  (make-instance 'clog-konva-stage
                   :connection-id (clog::connection-id div)
                   :html-id "stage")
  )
(defun make-rectangle (div &key (x 0) (y 0) (width 100) (height 100))
(js-query div (format nil ";
const rect = new Konva.Rect({
x: ~D,
y: ~D,
width: ~D,
height: ~D,
fill: 'blue',
draggable: true,
zindex:-1,
  name: 'rect',
});
layer.add(rect);
  " x y width height)
  ))

(defparameter *sim* nil)
(defun make-simulation ()
  (setf *sim* (make-instance 'simulation))
  )
    
(defun on-new-window (body)
  (clog-web-initialize body)
  (load-script (html-document body) "https://unpkg.com/konva@10/konva.min.js")
  (setf (title (html-document body)) "CL-MPM")
  (let* ((main (create-web-main body))
         ;(row (create-web-row body))  
         (panel (create-web-panel main :content "<h1>CL-MPM builder</h1>"
                           :class   "w3-blue"))
         )
    (set-styles panel '(("margin" "0")))
    ;(create-canvas main)
    (let ((workspace (create-div main :style "width:100%;min-height: 80vh; border-style: dashed; border-colour: black;
        display: flex;")))
      (defparameter *cad-window* (create-cad-window workspace))
      (create-property-window workspace)
    )
    ;(create-div main :style "width:100%;min-height: 80vh; border-style: dashed; border-colour: black")
    (let* ((row (create-div main :style "width:100%;height: 10vh;"))
           (create-sim (create-button row :content "Create" :class "w3-border" :style "height: 100%"))
           (create-mps (create-button row :content "Add MPs" :class "w3-border" :style "height: 100%")))
        (set-on-click create-sim (lambda (obj) (make-rectangle *canvas*)))
      )
    ;(create-web-container row :content "Grid Container 1" :column-size :half :class "w3-border")
    ;(create-web-container row :content "Grid Container 2" :column-size :half :class "w3-border")
    ;; (create-web-container row :content "Grid Container 1" :column-size :half :class "w3-border")
    ;; (create-web-container row :content "Grid Container 2" :column-size :half :class "w3-border")
  ;(let ((app (make-instance 'app-data)))
  ;  (setf (connection-data-item body "app-data") app)
  ;  (setf (title (html-document body)) "New App")
  ;  (clog-gui-initialize body)
  ;  (add-class body "w3-teal")
  ;  (let* ((menu-bar    (create-gui-menu-bar body))
  ;         (icon-item   (create-gui-menu-icon menu-bar :on-click 'on-help-about))
  ;         (file-item   (create-gui-menu-drop-down menu-bar :content "File"))
  ;         (file-new    (create-gui-menu-item file-item :content "New Window" :on-click 'on-file-new))
  ;         (help-item   (create-gui-menu-drop-down menu-bar :content "Help"))
  ;         (help-about  (create-gui-menu-item help-item :content "About" :on-click 'on-help-about))
  ;         (full-screen (create-gui-menu-full-screen menu-bar)))
  ;    (declare (ignore icon-item file-new help-about full-screen))))
      ))

(defun start-app ()
  (initialize 'on-new-window
   :static-root (merge-pathnames "./www/"
                  (asdf:system-source-directory :cl-mpm-builder)))
  (open-browser))
