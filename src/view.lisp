;;;; CLOG Builder generated code - modify original .clog file and rerender
(in-package :clog-user)
(defclass panel-1 (clog:clog-panel)
          ((clog-ace-2 :reader clog-ace-2)
           (clog-plotly-1 :reader clog-plotly-1)))
(defun create-panel-1 (clog-obj &key hidden class style html-id (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content "<div id=\"CLOGB39677147181\"
    style=\"width: 400px; height: 300px; box-sizing: content-box; position: absolute; left: 596px; top: 71px;\"
    class=\"w3-border\" data-clog-name=\"clog-plotly-1\"></div>

<div id=\"CLOGB39677147232\"
    style=\"width: 400px; height: 200px; border: thin solid black; box-sizing: content-box; position: absolute; left: 148px; top: 108px;\"
    class=\" ace_editor ace-xcode\" data-clog-name=\"clog-ace-2\"></div>"
                           :hidden hidden :class class :style style :html-id
                           html-id :auto-place auto-place)
          'panel-1)))
    (setf (slot-value panel 'clog-ace-2)
            (attach-as-child clog-obj "CLOGB39677147232" :clog-type
             'clog-ace:clog-ace-element :new-id t))
    (setf (slot-value panel 'clog-plotly-1)
            (attach-as-child clog-obj "CLOGB39677147181" :clog-type
             'clog-plotly:clog-plotly-element :new-id t))
    (let ((target (clog-plotly-1 panel)))
      (declare (ignorable target))
      (clog-plotly:attach-clog-plotly target)
      (clog-plotly:new-plot-plotly target "[{x: [1, 2, 3, 4, 5],
           y: [1, 2, 4, 8, 16]}]"
                                   "{ margin: { t: 0 } }"))
    (let ((target (clog-ace-2 panel)))
      (declare (ignorable target))
      (clog-ace:attach-clog-ace target)
      (setf (clog-ace:theme target) "ace/theme/iplastic")
      (setf (clog-ace:mode target) "ace/mode/lisp")
      (setf (clog-ace:tab-size target) 2))
    panel))