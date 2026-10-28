;;;; Package for the Cranelift mid-end rewrite rules ported from ISLE to ggs.

(uiop:define-package #:cranelift-opts
  (:use #:cl #:ggs)
  (:documentation "Cranelift's `cranelift/codegen/src/opts/*.isle` rewrite rules
as ggs rulesets, plus Lisp ports of the Rust helpers they call.

Term encoding (single-sorted, first order):

- Every CLIF value is a term whose head is the CLIF opcode, e.g. (iadd i32 x y).
  Value-producing opcodes take their result type as the first argument; side-
  effecting \"skeleton\" instructions (udiv, brif, trapz, ...) do not.
- Leaves (0-arity terms) carry immediates: types are symbols (i32, f64, i32x4),
  condition codes are symbols (eq, slt, ...), Imm64 is the immediate's bits as
  an unsigned integer masked to the type's width, Ieee16/32/64/128 and Constant
  are raw bit patterns as unsigned integers.
- Values defined outside the rewritten expression (function and block
  parameters) are written (param TYPE NAME), e.g. (param i32 x).
- Guards only ever inspect leaf-bound values, never engine data structures, so
  the rules run under any ggs backend. The one exception is the CLIF-TYPE
  analysis, used where ISLE asks for the type of an arbitrary value."))
