(defsystem lw-dash
  :description "A tool to convert LW documentations to Dash docset, based on dash-capi"
  :licence "MIT"
  :depends-on (:sqlite :lquery :alexandria :quri)
  :components ((:file "lw-dash")))
