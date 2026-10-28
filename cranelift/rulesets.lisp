;;;; Aggregate rulesets.

(in-package #:cranelift-opts)

;; Rulesets defined in rules/*.lisp, one per ISLE file.
(defparameter +opt-rulesets+
  '(arithmetic bitops cprop extends icmp remat selects shifts skeleton
    spaceship spectre vector))

;; All of Cranelift's mid-end rules.
(defrw* cranelift-opts
  arithmetic bitops cprop extends icmp remat selects shifts skeleton
  spaceship spectre vector)
