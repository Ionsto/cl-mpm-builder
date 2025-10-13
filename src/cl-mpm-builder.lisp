(in-package :cl-mpm-builder)


(defparameter *scene-size* (list 10 10))
(defparameter *prop-id* nil)
(defparameter *window* nil)
(defparameter *prop-div* nil)
(defparameter *sim* nil)
(defparameter *rect-list* (list))
(defparameter *stage-list* (make-hash-table))
(defparameter *meta-sim* nil)
(defparameter *render-manager* nil)



(defun delete-selected ()
  (when *prop-id*
    (delete-element (gethash *prop-id* *stage-list*))))

(defun update-properties ()
  (destroy-children *prop-div*)
  (when *prop-id*
    (if t
        (when (gethash *prop-id* *stage-list*)
          (create-propertiy-view *prop-div* (gethash *prop-id* *stage-list*)))
        (setf *prop-id* nil))))

(defclass app-data ()
  ((data
    :accessor data)))

(defclass property-view (clog-obj)
  ((current-object
    :initform nil
    :accessor property-view-object)))


(defun create-property-window (div)
  (let* ((window (create-div div :style "width:10%;max-height: 80vh; flex:1 1 auto; order:2;
overflow-y: auto;padding:0"
                                 :class "w3-container w3-grey"
                                 ))
         (title (create-div window :content "<h3>Property viewer</h3>"
                                         :class   "w3-container w3-light-grey"
                                         :style "margin-left:0%;margin-right:0%"
                                         ))
         (property-list (create-div window :class "w3-container w3-grey"))
         )
    (set-styles title '(("min-height" "10%") ("margin-top" "0%")))
    (setf *prop-div* property-list)))

(defclass clog-konva-stage (clog-obj)())

(defun create-cad-window (div)
  (let ((window (create-div div
                            :class "w3-container w3-border"
                            :style "width:90%;min-height: 80vh;resize: horizontal;;overflow: auto;order:1;")))
                                        ;(create-element window "svg" :viewBox "0 0 10 10" :x "2" :y "2")
    (setf *window* window)
    (setf *canvas* (make-canvas *window*))))

(defun end-sim ()
  (setf (cl-mpm::sim-run-sim *sim*) nil))

(defun make-sim ()
  (when *sim*
    (setf (cl-mpm::sim-run-sim *sim*) nil))
  (let* ((st (sim-type *meta-sim*))
         (type (cond
                 ((eq st :EXPLICIT)
                  'cl-mpm/aggregate::mpm-sim-agg-usf
                  ;; 'cl-mpm/dynamic-relaxation::mpm-sim-implict-dynamic
                  )
                 ;; ((eq st :IMPLICIT-DYNAMIC)
                 ;;  'cl-mpm/dynamic-relaxation::mpm-sim-implict-dynamic
                 ;;  )
                 ((eq st :IMPLICIT) 'cl-mpm/dynamic-relaxation::mpm-sim-dr-ul)
                 (t nil))))
    (if (not type)
        (progn
          (print "Error")
          (clog-web-alert *header* "Error" "Invalid sim type!"))
        (let* ((e-scale (/ 1d0 (sim-mesh-resolution *meta-sim*)))
               (mp-scale 2)
               (h 1d0)
               (dt-scale 0.9d0)
               (size *scene-size*)
               )
          (reset-render-manager *render-manager*)
          (let ((sim
                  (cl-mpm/setup::make-simple-sim
                   (/ 1d0 e-scale)
                   (mapcar (lambda (x) (* e-scale (round x h))) size)
                   :sim-type type
                   :args-list (sim-args-list *meta-sim*)
                   )))
            (setf *sim* sim)
            (loop for rect in *rect-list*
                  do (let* ((data (get-mps-data rect))
                            (obj (gethash (format nil "~A" rect) *stage-list*))
                            (mp-scale (mp-mps-count obj)))
                       (cl-mpm::add-mps
                        sim
                        (cl-mpm/setup::make-block-mps
                         (mp-block-position obj)
                         ;; (getf data :position)
                         (mp-block-size obj)
                         ;; (getf data :size)
                         (mapcar (lambda (e) (* e e-scale mp-scale)) (getf data :size))
                         (mp-block-density obj)
                         ;; (getf data :density)
                         ;; 'cl-mpm/particle::particle-elastic
                         ;; :E 1d6
                         ;; :nu 0.3d0
                         'cl-mpm/particle::particle-mc
                         :E (mp-e obj)
                         :nu (mp-nu obj)
                         :c (mp-rho obj)
                         :phi (* 30d0 (/ pi 180))
                         :psi 0d0
                         :enable-plasticity (mp-plasticity obj)
                         ))))
            ;; (cl-mpm/setup::setup-bcs *sim*
            ;;                          :left '(0 0 nil))

            (apply #'cl-mpm/setup::setup-bcs (append (list *sim*) (sim-bcs *meta-sim*)))
            (render-mps sim)
            (let ((lparallel:*debug-tasks-p* nil))
              (cond
                ((typep *meta-sim* 'simulation-dynamic)
                 (let ((dt-scale (sim-dt-scale *meta-sim*)))
                   (setf (cl-mpm:sim-dt sim) (* dt-scale (cl-mpm/setup:estimate-elastic-dt sim)))
                   (setf (cl-mpm:sim-damping-factor sim) (* (sim-damping-factor *meta-sim*) (cl-mpm/setup:estimate-critical-damping sim)))
                   (let ((sub-dt (sim-dt-steps *meta-sim*))
                         (total-dt (sim-dt-total *meta-sim*)))
                     (loop for x from 0 to (round total-dt sub-dt)
                           while (cl-mpm::sim-run-sim sim)
                           do (progn
                                (handler-case
                                    (progn
                                      (let ((substeps (floor sub-dt (cl-mpm:sim-dt sim))))
                                        (format t "Step ~D - substeps ~D~%" x substeps)
                                        (setf (text *sim-messages*)
                                              (format nil "Simulation log: step ~D - substep ~D - time ~F"
                                                      x substeps (cl-mpm::sim-time sim)))
                                        (dotimes (i substeps)
                                          (cl-mpm:update-sim sim)))
                                      ;; (setf (cl-mpm:sim-dt sim) (* dt-scale (cl-mpm::calculate-min-dt sim)))
                                      (render-mps sim))
                                  (error (c)
                                    (princ c)))
                                ))))
                 )
                ((typep *meta-sim* 'simulation-quasi-static)
                 (handler-case
                     (let ((lstp 0))
                       (cl-mpm/dynamic-relaxation::run-load-control
                        *sim*
                        :output-dir "./output/"
                        :plotter (lambda (sim)
                                   ;; (render-mps sim)
                                   )
                        :load-steps (sim-load-steps *meta-sim*)
                        :damping 1d0
                        :substeps 50
                        :criteria (sim-residual-criteria *meta-sim*)
                        :adaptive-damping t
                        :kinetic-damping nil
                        :save-vtk-dr nil
                        :save-vtk-loadstep nil
                        :dt-scale 1d0
                        :post-iter-step (lambda (i energy oobf)
                                          (setf (text *sim-messages*)
                                                (format nil "Simulation log: loadstep ~D - substep ~D - oobf ~E"
                                                        lstp i oobf)))
                        :post-conv-step (lambda (sim)
                                          (incf lstp)
                                          (render-mps sim))))
                   (error (c)
                     (princ c))
                   )))
              )
            (render-mps sim))))))


(defun make-canvas (div)
  (js-query div (format nil "
sceneWidth = ~E;
sceneHeight = ~E;
container_id = '~A';
stage = new Konva.Stage({
  container: container_id,
  width: 500,
  height: 500,});
stage.container().tabIndex = 1;
stage.container().focus();
layer = new Konva.Layer();
stage.add(layer);
  render = new Konva.Layer();
  stage.add(render);
tr = new Konva.Transformer({rotateEnabled :false});
layer.add(tr);
stage.on('click tap', function (e) {
  // if click on empty area - remove all selections
  if (e.target === stage) {
    tr.nodes([]);
    canvas_select_event('stage');
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
    canvas_select_event(e.target.clog_id);
    tr.nodes([e.target]);
    tr.moveToTop();
  }});

scroll_scale = 1;
function fitStageIntoParentContainer() {
  // Get the container element
  const container = document.getElementById(container_id);
  // Make the container take up the full width
  container.style.width = '100%';
  container.style.height = '80vh';
  // Get current container width
  const containerWidth = container.offsetWidth;
  // Calculate scale based on virtual width vs actual width
  const scale = scroll_scale * containerWidth / sceneWidth;
  // Set stage dimensions and scale
  stage.width(sceneWidth * scale);
  stage.height(sceneHeight * scale);
  stage.scale({ x: scale, y: scale });
}

// Initial fit
fitStageIntoParentContainer();

window.change_domain_size = function change_domain_size(x,y) {
console.log('domain change');
  sceneWidth = x;
  sceneHeight = y;
  fitStageIntoParentContainer();
}

// Adapt the stage on window resize
window.addEventListener('resize', fitStageIntoParentContainer);
/*
stage.on('wheel', (e) => {
	// stop default scrolling
	let direction = e.evt.deltaY > 0 ? 1 : -1;
	e.evt.preventDefault();
	if (e.evt.ctrlKey) {
			direction = -direction;
	}
  scale_by = 1.0001;
oldScale = scroll_scale;
 scroll_scale = direction > 0 ? scroll_scale * scale_by : scroll_scale / scale_by;

			var pointer = stage.getPointerPosition();

			var mousePointTo = {
			x: (pointer.x - layer.x()) / oldScale,
			y: (pointer.y - layer.y()) / oldScale,
			};
var newPos = {
				x: pointer.x - mousePointTo.x * scroll_scale,
				y: pointer.y - mousePointTo.y * scroll_scale,
			};
			layer.position(newPos);
 fitStageIntoParentContainer();
});*/


function canvas_select_event(selected) {
 $(window).trigger('canvas_select_event',selected);
}
stage.container().addEventListener('keydown',function(e){
    if(e.keyCode == 46) {
         $(window).trigger('delete_event');
    }
});
  "
            (first *scene-size*)
            (second *scene-size*)
            (html-id div)))
  (make-instance 'clog-konva-stage
                 :connection-id (clog::connection-id div)
                 :html-id "stage"))

(defun make-rectangle (div &key (clog-id nil)  (x -1) (y 0) (width 1) (height 1))
  (let ((clog-id (if clog-id
                     clog-id
                     (clog:generate-id))))
    (js-execute div (format nil ";
let normal_colour = 'blue';
let highlight_colour = 'red';
let clog_id = '~A';
clog[clog_id] = new Konva.Rect({
x: ~D,
y: ~D,
width: ~D,
height: ~D,
fill: normal_colour,
draggable: true,
zindex:-1,
  name: 'rect',
});

let c = clog[clog_id];
clog[clog_id].clog_id = clog_id;
layer.add(clog[clog_id]);

function register(l){
var cid = l.clog_id;
l.on('transformend', () => $(l).trigger('update'));
l.on('dragend', () => $(l).trigger('update'));
}

register(c);
c.on('dragmove', function (){
this.x(Math.min(Math.max(this.x(),0),sceneWidth-this.width()));
this.y(Math.min(Math.max(this.y(),0),sceneHeight-this.height()));
});
c.on('mouseover', function () {
this.opacity(0.5);
this.moveToTop();
this.fill(highlight_colour);
});
c.on('mouseout', function () {
this.opacity(1);
this.fill(normal_colour);
});
" clog-id x y width height))
    clog-id))

(setf *sim* nil)
(setf *rect-list* (list))
(setf *stage-list* (make-hash-table))

(defun make-simulation ()
  (setf *render-manager* (make-instance 'render-manager))
  (setf *meta-sim* (make-instance 'simulation
                                  :connection-id (clog::connection-id *canvas*)
                                  :html-id (generate-id)))
  (setf (gethash "stage" *stage-list*) *meta-sim*))

(defun on-new-window (body)
  (clog-web-initialize body)
  (defparameter *body* body)
  (setf *rect-list* nil)
  (setf *stage-list* (make-hash-table :test 'equal))
  (defparameter *canvas* nil)
  (load-script (html-document body) "https://unpkg.com/konva@10/konva.min.js")
  (setf (title (html-document body)) "CL-MPM")
  (let* ((main (create-web-main body))
         ;(row (create-web-row body))
         (panel (create-web-panel main :content "<h1>CL-MPM builder</h1>"
                           :class   "w3-blue"))
         )
      (defparameter *header* panel)
    (set-styles panel '(("margin" "0")))
                                        ;(create-canvas main)
    (let ((workspace (create-div main :style "width:100%;min-height: 80vh; border-colour: black;
        display: flex;")))
      (defparameter *workspace* workspace)
      (defparameter *cad-window* (create-cad-window workspace))
      (create-property-window workspace)
      (let* ((row (create-div main :style "width:100%;height: 10vh;gap:8px;display: flex;" :class "w3-flex"))
             (button-class "w3-button")
             (create-sim (create-button row :content "Reset"          :class button-class :style "height: 100%"))
             (create-mps (create-button row :content "Add MPs"         :class button-class :style "height: 100%"))
             (run-sim (create-button row :content "Run sim"            :class button-class :style "height: 100%"))
             (end-sim (create-button row :content "End sim"            :class button-class :style "height: 100%"))
             (clear-sim (create-button row :content "Clear render" :class button-class :style "height: 100%"))
             (sim-anim-play (create-button row :content "▶" :class button-class :style "height: 100%:margin-left:auto"))
             (sim-anim-pause (create-button row :content "⏸" :class button-class :style "height: 100%:margin-left:auto"))
             (sim-timeline
               (clog:create-form-element
                row
                :range
                :class "w3-input"
                :style "margin-left:auto;width:50vh"
                :min "0"
                :max "0"
                :value 0))

             (sim-messages (create-div row :content "Simulation log:" :class "w3-container" :style "height:100%;margin-left:auto;min-width: 30vw;")))
        (defparameter *sim-messages* sim-messages)
        (defparameter *sim-timeline* sim-timeline)
        (make-simulation)
        (set-on-event sim-timeline "change" (lambda (obj)
                                              (update-render (parse-integer (text-value *sim-timeline*)))))
        (set-on-click sim-anim-play
                      (lambda (obj)
                        (js-query *canvas*
                                    (format nil "
let slider = clog['~A'];
let vmax = slider.max;
let vmin = slider.min;
let v = slider.value;
if (typeof inter !== 'undefined') {
console.log('clear');
    clearInterval(inter);
}
slider.value = 0;
inter = setInterval(function() {
var newVal = parseInt(slider.value) + 1;
slider.value = newVal;
$(slider).trigger('change');
if (slider.value == slider.max){
    clearInterval(inter);
}
}, 100);
" (html-id sim-timeline)))))
        (set-on-click sim-anim-pause
                      (lambda (obj)
                        (js-query *canvas*
                                  (format nil "
let slider = clog['~A'];
if (typeof inter !== 'undefined') {
    clearInterval(inter);
}" (html-id sim-timeline)))))

        (set-on-click create-sim (lambda (obj)
                                   (setf *rect-list* nil)
                                   (clog:destroy *window*)
                                   (setf *cad-window* (create-cad-window workspace))
                                   (make-simulation)
                                   ))
        (set-on-click create-mps (lambda (obj)
                                   (let* ((clog-id (make-rectangle *canvas* :height 1 :width 1))
                                          (obj
                                            (make-instance 'mp-block
                                                             :connection-id (clog::connection-id *canvas*)
                                                             :html-id clog-id)))
                                     (push clog-id
                                           *rect-list*)
                                     ;; (pprint (format nil "~A" clog-id))
                                     (setf (gethash (format nil "~A" clog-id) *stage-list*) obj)
                                     )))
        (set-on-click run-sim (lambda (obj)
                                (make-sim)
                                   ))
        (set-on-click end-sim (lambda (obj)
                                (end-sim)
                                ))
        (set-on-click clear-sim (lambda (obj)
                                (reset-render-canvas *sim*)))
        (set-on-event-with-data (window body) "canvas_select_event"
                      (lambda (obj id)
                        (setf *prop-id* id)
                        (update-properties)
                        ;; (pprint id)
                        ))
        (set-on-event (window body) "delete_event"
                      (lambda (obj)
                        (delete-selected)))
        ))))

(defun start-app ()
  (setf lparallel:*kernel* (lparallel::make-kernel 8))
  (initialize 'on-new-window
   :static-root (merge-pathnames "./www/"
                  (asdf:system-source-directory :cl-mpm-builder)))
  (open-browser))


