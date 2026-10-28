(asdf:defsystem #:cranelift-opts
  :description "Cranelift's mid-end ISLE rewrite rules, ported to ggs."
  :depends-on (:ggs :float-features)
  :serial t
  :components ((:file "package")
               (:file "numerics")
               (:file "prelude")
               (:module "rules"
                :serial t
                :components ((:file "support")
                             (:file "arithmetic")
                             (:file "bitops")
                             (:file "cprop")
                             (:file "extends")
                             (:file "icmp")
                             (:file "remat")
                             (:file "selects")
                             (:file "shifts")
                             (:file "skeleton")
                             (:file "spaceship")
                             (:file "spectre")
                             (:file "vector")))
               (:file "rulesets"))
  :in-order-to ((test-op (test-op "cranelift-opts/tests"))))

(asdf:defsystem #:cranelift-opts/tests
  :depends-on (:cranelift-opts)
  :components ((:file "tests"))
  :perform (test-op (o c) (uiop:symbol-call '#:cranelift-opts '#:run-tests)))
