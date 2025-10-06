
(asdf/parse-defsystem:defsystem #:cl-mpm-builder
  :description
  "CLOG powered interface for cl-mpm"
  :author
  "some@one.com"
  :license
  "BSD"
  :version
  "0.0.0"
  :serial
  t
  :entry-point
  "cl-mpm-builder:start-app"
  :depends-on
  (#:cl-mpm/example
   #:clog "clog-plotly" "clog-ace")
  :components
  ((:module "src"
    :serial t
    :components ((:file "package")
                 (:file "cl-mpm-representation")
                 (:file "konva-link")
                 (:file "property-view")
                 (:file "render-manager")
                 (:file "cl-mpm-builder")))))
(asdf/parse-defsystem:defsystem #:cl-mpm-builder/tools
  :defsystem-depends-on
  (:clog)
  :depends-on
  (#:cl-mpm-builder #:clog/tools "clog-plotly/tools" "clog-ace/tools")
  :components
  nil)
