(defsystem "lem-tutor"
  :depends-on ("lem/core")
  :components ((:file "tutorial-syntax-parser")
               (:file "tutorial-colors")
               (:file "lem-tutor")))