;;;; Lisp port of Cranelift's generated ISLE numerics (`numerics.isle`).
;;;;
;;;; Cranelift generates these from a schema in
;;;; `cranelift/codegen/meta/src/gen_isle.rs` (`gen_numerics_isle`); this file
;;;; reproduces that schema so every `{ty}_{op}` helper has the same semantics:
;;;;
;;;; - `checked_*` are partial: NIL when Rust returns None.
;;;; - `wrapping_*` wrap to the type's width.
;;;; - The plain variants (`add`, `sub`, `shl`, ...) signal an error where Rust
;;;;   panics.
;;;;
;;;; ISLE name `u64_wrapping_add` becomes the Lisp function `u64-wrapping-add`.
;;;;
;;;; There is no hand-written Rust for these: gen_isle.rs emits each Rust
;;;; function from the `body` string of a `NumericOp` schema entry. Each group
;;;; below is preceded by the schema entries it ports, with file, line numbers
;;;; and a GitHub link, all from wasmtime commit
;;;; 209d6c4d2963eed81edc839878cddb0a9dc15880. The generator itself:
;;;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/meta/src/gen_isle.rs#L909

(in-package #:cranelift-opts)

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; Rust: cranelift/codegen/meta/src/gen_isle.rs, line 545-561
  ;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/meta/src/gen_isle.rs#L545-L561
  ;;
  ;;     fn all() -> impl Iterator<Item = NumericType> {
  ;;         [1, 2, 4, 8, 16].into_iter().flat_map(|byte_width| {
  ;;             [true, false]
  ;;                 .into_iter()
  ;;                 .map(move |signed| NumericType { signed, byte_width })
  ;;         })
  ;;     }
  ;;
  ;;     fn name(&self) -> &'static str {
  ;;         let idx = self.byte_width.ilog2();
  ;;         let idx = usize::try_from(idx).unwrap();
  ;;         if self.signed {
  ;;             ["i8", "i16", "i32", "i64", "i128"][idx]
  ;;         } else {
  ;;             ["u8", "u16", "u32", "u64", "u128"][idx]
  ;;         }
  ;;     }
  (defparameter +numeric-types+
    '((i8 t 8) (u8 nil 8) (i16 t 16) (u16 nil 16) (i32 t 32) (u32 nil 32)
      (i64 t 64) (u64 nil 64) (i128 t 128) (u128 nil 128))
    "(NAME SIGNEDP BITS) for each ISLE numeric type.")

  (defun numeric-type-info (name)
    (or (assoc name +numeric-types+ :test #'string=)
        (error "Unknown numeric type ~a" name)))

  (defun numeric-fn-name (&rest parts)
    (intern (string-upcase (format nil "~{~a~^-~}" parts)) '#:cranelift-opts)))

;;; Range helpers

(declaim (inline wrap-to fits-p))
(defun wrap-to (x signedp bits)
  "Wrap integer X into the range of a SIGNEDP integer of BITS bits."
  (let ((u (ldb (byte bits 0) x)))
    (if (and signedp (logbitp (1- bits) u))
        (- u (ash 1 bits))
        u)))

(defun fits-p (x signedp bits)
  (if signedp
      (<= (- (ash 1 (1- bits))) x (1- (ash 1 (1- bits))))
      (<= 0 x (1- (ash 1 bits)))))

(defun checked (x signedp bits)
  "X if it is representable, else NIL (Rust's None)."
  (and (fits-p x signedp bits) x))

(defun unwrap-or-panic (x what &rest args)
  (or x (apply #'error what args)))

;;; Bit-counting helpers on the two's-complement bits of X in BITS.

(defun trailing-zeros (x bits)
  (let ((u (ldb (byte bits 0) x)))
    (if (zerop u) bits (1- (integer-length (logand u (- u)))))))

(defun leading-zeros (x bits)
  (- bits (integer-length (ldb (byte bits 0) x))))

(defun trailing-ones (x bits)
  (trailing-zeros (lognot x) bits))

(defun leading-ones (x bits)
  (leading-zeros (lognot x) bits))

(defun rotate-left (x n bits signedp)
  (let* ((u (ldb (byte bits 0) x))
         (n (mod n bits)))
    (wrap-to (logior (ash u n) (ash u (- n bits))) signedp bits)))

;;; Rust integer division truncates toward zero and so does CL:TRUNCATE.

(defun checked-div (a b signedp bits)
  (unless (zerop b)
    (checked (truncate a b) signedp bits)))

(defun checked-rem (a b signedp bits)
  ;; Rust's checked_rem is None for MIN % -1, since MIN / -1 overflows.
  (unless (or (zerop b) (null (checked (truncate a b) signedp bits)))
    (rem a b)))

(defun checked-shift (a b signedp bits leftp)
  ;; Rust's checked_shl/shr only fail when the shift amount is >= BITS.
  (when (< b bits)
    (wrap-to (ash a (if leftp b (- b))) signedp bits)))

(defun wrapping-shift (a b signedp bits leftp)
  (wrap-to (ash a (if leftp (mod b bits) (- (mod b bits)))) signedp bits))

(defmacro define-numeric-ops (ty)
  (destructuring-bind (ty signedp bits) (numeric-type-info ty)
    (flet ((fn (op) (numeric-fn-name ty op)))
      `(progn
         ;; Comparisons
         ;;
         ;; Rust: cranelift/codegen/meta/src/gen_isle.rs, line 640-670
         ;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/meta/src/gen_isle.rs#L640-L670
         ;;
         ;;     // Comparisons.
         ;;     ops.push(NumericOp {
         ;;         name: "eq",
         ;;         body: "a == b",
         ;;         ..comparison.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "ne",
         ;;         body: "a != b",
         ;;         ..comparison.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "lt",
         ;;         body: "a < b",
         ;;         ..comparison.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "lt_eq",
         ;;         body: "a <= b",
         ;;         ..comparison.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "gt",
         ;;         body: "a > b",
         ;;         ..comparison.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "gt_eq",
         ;;         body: "a >= b",
         ;;         ..comparison.clone()
         ;;     });
         (defun ,(fn "eq") (a b) (= a b))
         (defun ,(fn "ne") (a b) (/= a b))
         (defun ,(fn "lt") (a b) (< a b))
         (defun ,(fn "lt-eq") (a b) (<= a b))
         (defun ,(fn "gt") (a b) (> a b))
         (defun ,(fn "gt-eq") (a b) (>= a b))
         ;; Arithmetic: checked (partial), wrapping, and panicking variants
         ;;
         ;; Rust: cranelift/codegen/meta/src/gen_isle.rs, line 672-750
         ;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/meta/src/gen_isle.rs#L672-L750
         ;;
         ;;     // Arithmetic operations.
         ;;     //
         ;;     // For each operation (e.g. addition) we have three variants:
         ;;     //
         ;;     // * partial ctor `checked_add`: no return value on overflow
         ;;     // * ctor `wrapping_add`: wraps on overflow
         ;;     // * ctor `add`: non-partial but panics at runtime on overflow
         ;;     ops.push(NumericOp {
         ;;         name: "checked_add",
         ;;         body: "a.checked_add(b)",
         ;;         ..partial_binop.clone()
         ;;     });
         ;;
         ;;     ops.push(NumericOp {
         ;;         name: "wrapping_add",
         ;;         body: "a.wrapping_add(b)",
         ;;         ..binop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "add",
         ;;         body: r#"a.checked_add(b).unwrap_or_else(|| panic!("addition overflow: {a} + {b}"))"#,
         ;;         ..binop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "checked_sub",
         ;;         body: "a.checked_sub(b)",
         ;;         ..partial_binop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "wrapping_sub",
         ;;         body: "a.wrapping_sub(b)",
         ;;         ..binop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;             name: "sub",
         ;;             body: r#"a.checked_sub(b).unwrap_or_else(|| panic!("subtraction overflow: {a} - {b}"))"#,
         ;;             ..binop.clone()
         ;;         });
         ;;     ops.push(NumericOp {
         ;;         name: "checked_mul",
         ;;         body: "a.checked_mul(b)",
         ;;         ..partial_binop.clone()
         ;;     });
         ;;
         ;;     ops.push(NumericOp {
         ;;         name: "wrapping_mul",
         ;;         body: "a.wrapping_mul(b)",
         ;;         ..binop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;             name: "mul",
         ;;             body: r#"a.checked_mul(b).unwrap_or_else(|| panic!("multiplication overflow: {a} * {b}"))"#,
         ;;             ..binop.clone()
         ;;         });
         ;;     ops.push(NumericOp {
         ;;         name: "checked_div",
         ;;         body: "a.checked_div(b)",
         ;;         ..partial_binop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "wrapping_div",
         ;;         body: "a.wrapping_div(b)",
         ;;         ..binop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "div",
         ;;         body: r#"a.checked_div(b).unwrap_or_else(|| panic!("div failure: {a} / {b}"))"#,
         ;;         ..binop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "checked_rem",
         ;;         body: "a.checked_rem(b)",
         ;;         ..partial_binop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "rem",
         ;;         body: r#"a.checked_rem(b).unwrap_or_else(|| panic!("rem failure: {a} % {b}"))"#,
         ;;         ..binop.clone()
         ;;     });
         ,@(loop for (op lisp-op) in '(("add" +) ("sub" -) ("mul" *))
                 append
                 `((defun ,(fn (format nil "checked-~a" op)) (a b)
                     (checked (,lisp-op a b) ,signedp ,bits))
                   (defun ,(fn (format nil "wrapping-~a" op)) (a b)
                     (wrap-to (,lisp-op a b) ,signedp ,bits))
                   (defun ,(fn op) (a b)
                     (unwrap-or-panic (checked (,lisp-op a b) ,signedp ,bits)
                                      ,(format nil "~a ~a overflow: ~~a, ~~a" ty op) a b))))
         (defun ,(fn "checked-div") (a b) (checked-div a b ,signedp ,bits))
         (defun ,(fn "wrapping-div") (a b)
           (when (zerop b) (error ,(format nil "~a division by zero" ty)))
           (wrap-to (truncate a b) ,signedp ,bits))
         (defun ,(fn "div") (a b)
           (unwrap-or-panic (checked-div a b ,signedp ,bits)
                            ,(format nil "~a div failure: ~~a / ~~a" ty) a b))
         (defun ,(fn "checked-rem") (a b) (checked-rem a b ,signedp ,bits))
         (defun ,(fn "rem") (a b)
           (unwrap-or-panic (checked-rem a b ,signedp ,bits)
                            ,(format nil "~a rem failure: ~~a % ~~a" ty) a b))
         ;; Bitwise
         ;;
         ;; Rust: cranelift/codegen/meta/src/gen_isle.rs, line 751-814
         ;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/meta/src/gen_isle.rs#L751-L814
         ;;
         ;;     // Bitwise operations.
         ;;     //
         ;;     // When applicable (e.g. shifts) we have checked, wrapping, and
         ;;     // unwrapping variants, similar to arithmetic operations.
         ;;     ops.push(NumericOp {
         ;;         name: "and",
         ;;         body: "a & b",
         ;;         ..binop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "or",
         ;;         body: "a | b",
         ;;         ..binop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "xor",
         ;;         body: "a ^ b",
         ;;         ..binop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "not",
         ;;         body: "!a",
         ;;         ..unop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "checked_shl",
         ;;         body: "a.checked_shl(b)",
         ;;         ..partial_shift.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "wrapping_shl",
         ;;         body: "a.wrapping_shl(b)",
         ;;         ..shift.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "shl",
         ;;         body: r#"a.checked_shl(b).unwrap_or_else(|| panic!("shl overflow: {a} << {b}"))"#,
         ;;         ..shift.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "checked_shr",
         ;;         body: "a.checked_shr(b)",
         ;;         ..partial_shift.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "wrapping_shr",
         ;;         body: "a.wrapping_shr(b)",
         ;;         ..shift.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "shr",
         ;;         body: r#"a.checked_shr(b).unwrap_or_else(|| panic!("shr overflow: {a} >> {b}"))"#,
         ;;         ..shift.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "rotl",
         ;;         body: "a.rotate_left(b)",
         ;;         ..shift.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "rotr",
         ;;         body: "a.rotate_right(b)",
         ;;         ..shift.clone()
         ;;     });
         (defun ,(fn "and") (a b) (logand a b))
         (defun ,(fn "or") (a b) (logior a b))
         (defun ,(fn "xor") (a b) (logxor a b))
         (defun ,(fn "not") (a) (wrap-to (lognot a) ,signedp ,bits))
         ,@(loop for (dir leftp) in '(("shl" t) ("shr" nil))
                 append
                 `((defun ,(fn (format nil "checked-~a" dir)) (a b)
                     (checked-shift a b ,signedp ,bits ,leftp))
                   (defun ,(fn (format nil "wrapping-~a" dir)) (a b)
                     (wrapping-shift a b ,signedp ,bits ,leftp))
                   (defun ,(fn dir) (a b)
                     (unwrap-or-panic (checked-shift a b ,signedp ,bits ,leftp)
                                      ,(format nil "~a ~a overflow: ~~a, ~~a" ty dir) a b))))
         (defun ,(fn "rotl") (a b) (rotate-left a b ,bits ,signedp))
         (defun ,(fn "rotr") (a b) (rotate-left a (- b) ,bits ,signedp))
         ;; Predicates, each with the extractors gen_isle.rs derives from it:
         ;; {ty}_matches_{p} (bool <- ty), {ty}_extract_{p} and {ty}_when_{p}
         ;; are internal ISLE extractors expanded by the translator.
         ;;
         ;; Rust: cranelift/codegen/meta/src/gen_isle.rs, line 816-840
         ;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/meta/src/gen_isle.rs#L816-L840
         ;;
         ;;     // Predicates.
         ;;     //
         ;;     // We generate both pure constructors and a variety of extractors
         ;;     // for these. See the relevant comments in `gen_numerics_isle` about
         ;;     // the extractors.
         ;;     ops.push(NumericOp {
         ;;         name: "is_zero",
         ;;         body: "a == 0",
         ;;         ..predicate.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "is_non_zero",
         ;;         body: "a != 0",
         ;;         ..predicate.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "is_odd",
         ;;         body: "a & 1 == 1",
         ;;         ..predicate.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "is_even",
         ;;         body: "a & 1 == 0",
         ;;         ..predicate.clone()
         ;;     });
         ;;
         ;; Rust: cranelift/codegen/meta/src/gen_isle.rs, line 897-903
         ;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/meta/src/gen_isle.rs#L897-L903
         ;;
         ;;         // Operations that apply only to unsigned numbers.
         ;;         ops.push(NumericOp {
         ;;             name: "is_power_of_two",
         ;;             body: "a.is_power_of_two()",
         ;;             ..predicate.clone()
         ;;         });
         ;;     }
         ;;
         ;; Rust: cranelift/codegen/meta/src/gen_isle.rs, line 952-1005
         ;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/meta/src/gen_isle.rs#L952-L1005
         ;;
         ;;     // When generating extractors for a `{ty}_is_foo` predicate,
         ;;     // we generate the following:
         ;;     //
         ;;     // * bool <- ty etor: `{ty}_matches_foo`
         ;;     // * ty <- ty etor: `{ty}_extract_foo`
         ;;     // * () <- ty etor: `{ty}_when_foo`
         ;;     // * () <- ty etor: `{ty}_when_not_foo`
         ;;     //
         ;;     // The last three are defined as local extractors that are
         ;;     // implemented in terms of the first. This gives the ISLE compiler
         ;;     // visibility into the extractors' overlapping-ness.
         ;;     if op.etors {
         ;;         debug_assert_eq!(op.args.len(), 1);
         ;;         debug_assert_eq!(op.args[0].1, ty);
         ;;         debug_assert_eq!(op.ret, "bool");
         ;;         debug_assert!(op.name.starts_with("is_"));
         ;;
         ;;         // Cut of the `is_` prefix.
         ;;         let base_name = &op.name[3..];
         ;;         debug_assert!(base_name.len() > 0);
         ;;
         ;;         fmtln!(isle, "(decl pure {ty}_matches_{base_name} (bool) {ty})");
         ;;         fmtln!(
         ;;             isle,
         ;;             "(extern extractor {ty}_matches_{base_name} {ty}_matches_{base_name})"
         ;;         );
         ;;         fmtln!(rust, "#[inline]");
         ;;         fmtln!(
         ;;             rust,
         ;;             "fn {ty}_matches_{base_name}(&mut self, a: {ty}) -> Option<bool> {{"
         ;;         );
         ;;         rust.indent(|rust| {
         ;;             fmtln!(rust, "Some({body})");
         ;;         });
         ;;         fmtln!(rust, "}}");
         ;;
         ;;         fmtln!(isle, "(decl pure {ty}_extract_{base_name} ({ty}) {ty})");
         ;;         fmtln!(
         ;;             isle,
         ;;             "(extractor ({ty}_extract_{base_name} x) (and ({ty}_matches_{base_name} true) x))"
         ;;         );
         ;;
         ;;         fmtln!(isle, "(decl pure {ty}_when_{base_name} () {ty})");
         ;;         fmtln!(
         ;;             isle,
         ;;             "(extractor ({ty}_when_{base_name}) ({ty}_matches_{base_name} true))"
         ;;         );
         ;;
         ;;         fmtln!(isle, "(decl pure {ty}_when_not_{base_name} () {ty})");
         ;;         fmtln!(
         ;;             isle,
         ;;             "(extractor ({ty}_when_not_{base_name}) ({ty}_matches_{base_name} false))"
         ;;         );
         ;;     }
         ,@(loop for (p body) in `(("zero" (zerop a))
                                   ("non-zero" (not (zerop a)))
                                   ("odd" (oddp a))
                                   ("even" (evenp a))
                                   ,@(unless signedp
                                       '(("power-of-two" (and (plusp a) (zerop (logand a (1- a))))))))
                 append
                 `((defun ,(fn (format nil "is-~a" p)) (a) ,body)
                   (defun ,(numeric-fn-name "match" ty "matches" p) (a) ,body)))
         ;; Miscellaneous unary operations
         ;;
         ;; Rust: cranelift/codegen/meta/src/gen_isle.rs, line 841-877
         ;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/meta/src/gen_isle.rs#L841-L877
         ;;
         ;;     // Miscellaneous unary operations.
         ;;     ops.push(NumericOp {
         ;;         name: "checked_ilog2",
         ;;         body: "a.checked_ilog2()",
         ;;         ret: "u32",
         ;;         ..partial_unop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "ilog2",
         ;;         body: r#"a.checked_ilog2().unwrap_or_else(|| panic!("ilog2 overflow: {a}"))"#,
         ;;         ret: "u32",
         ;;         ..unop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "trailing_zeros",
         ;;         body: "a.trailing_zeros()",
         ;;         ret: "u32",
         ;;         ..unop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "trailing_ones",
         ;;         body: "a.trailing_ones()",
         ;;         ret: "u32",
         ;;         ..unop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "leading_zeros",
         ;;         body: "a.leading_zeros()",
         ;;         ret: "u32",
         ;;         ..unop.clone()
         ;;     });
         ;;     ops.push(NumericOp {
         ;;         name: "leading_ones",
         ;;         body: "a.leading_ones()",
         ;;         ret: "u32",
         ;;         ..unop.clone()
         ;;     });
         (defun ,(fn "checked-ilog2") (a) (when (plusp a) (1- (integer-length a))))
         (defun ,(fn "ilog2") (a)
           (if (plusp a)
               (1- (integer-length a))
               (error ,(format nil "~a ilog2 overflow: ~~a" ty) a)))
         (defun ,(fn "trailing-zeros") (a) (trailing-zeros a ,bits))
         (defun ,(fn "trailing-ones") (a) (trailing-ones a ,bits))
         (defun ,(fn "leading-zeros") (a) (leading-zeros a ,bits))
         (defun ,(fn "leading-ones") (a) (leading-ones a ,bits))

         ;; Rust: cranelift/codegen/meta/src/gen_isle.rs, line 879-895
         ;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/meta/src/gen_isle.rs#L879-L895
         ;;
         ;;     if ty.signed {
         ;;         // Operations that apply only to signed numbers.
         ;;         ops.push(NumericOp {
         ;;             name: "checked_neg",
         ;;             body: "a.checked_neg()",
         ;;             ..partial_unop.clone()
         ;;         });
         ;;         ops.push(NumericOp {
         ;;             name: "wrapping_neg",
         ;;             body: "a.wrapping_neg()",
         ;;             ..unop.clone()
         ;;         });
         ;;         ops.push(NumericOp {
         ;;             name: "neg",
         ;;             body: r#"a.checked_neg().unwrap_or_else(|| panic!("negation overflow: {a}"))"#,
         ;;             ..unop.clone()
         ;;         });
         ,@(when signedp
             `((defun ,(fn "checked-neg") (a) (checked (- a) t ,bits))
               (defun ,(fn "wrapping-neg") (a) (wrap-to (- a) t ,bits))
               (defun ,(fn "neg") (a)
                 (unwrap-or-panic (checked (- a) t ,bits)
                                  ,(format nil "~a negation overflow: ~~a" ty) a))))))))

;; Rust: cranelift/codegen/meta/src/gen_isle.rs, line 1012-1193
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/meta/src/gen_isle.rs#L1012-L1193
;;
;;     // Numeric type conversions.
;;     //
;;     // Naming and conventions:
;;     //
;;     // * Constructors:
;;     //   * "<from>_into_<to>" for lossless, infallible conversion
;;     //   * "<from>_try_into_<to>" for lossless, fallible conversions (exposed as
;;     //     partial constructors)
;;     //   * "<from>_unwrap_into_<to>" for lossless, fallible conversions that will
;;     //     panic at runtime if the conversion would be lossy
;;     //   * "<from>_truncate_into_<to>" for lossy, infallible conversions that
;;     //     ignore upper bits
;;     //   * "<from>_cast_[un]signed" for signed-to-unsigned (and vice versa)
;;     //     reinterpretation
;;     // * Extractors:
;;     //   * "<to>_from_<from>" for both fallible and infallible extractors
;;     //   * No unwrapping extractors
;;     //   * No truncating extractors
;;     //   * No signed-to-unsigned reinterpreting extractors
;;     for from in NumericType::all() {
;;         for to in NumericType::all() {
;;             if from == to {
;;                 continue;
;;             }
;;
;;             let from_name = from.name();
;;             let to_name = to.name();
;;
;;             let lossy = match (from.byte_width.cmp(&to.byte_width), from.signed, to.signed) {
;;                 // Widening with the same signedness is lossless.
;;                 (Ordering::Less, true, true) | (Ordering::Less, false, false) => false,
;;                 // Widening from unsigned to signed is lossless.
;;                 (Ordering::Less, false, true) => false,
;;                 // Widening from signed to unsigned is lossy.
;;                 (Ordering::Less, true, false) => true,
;;                 // Same width means we must be changing sign, since we skip
;;                 // `from == to`, and this is lossy.
;;                 (Ordering::Equal, _, _) => {
;;                     debug_assert_ne!(from.signed, to.signed);
;;                     true
;;                 }
;;                 // Narrowing is always lossy.
;;                 (Ordering::Greater, _, _) => true,
;;             };
;;
;;             let (ctor, partial, rust_ret) = if lossy {
;;                 (
;;                     "try_into",
;;                     " partial",
;;                     Cow::from(format!("Option<{to_name}>")),
;;                 )
;;             } else {
;;                 ("into", "", Cow::from(to_name))
;;             };
;;
;;             // Constructor.
;;             fmtln!(
;;                 isle,
;;                 "(decl pure{partial} {from_name}_{ctor}_{to_name} ({from_name}) {to_name})"
;;             );
;;             fmtln!(
;;                 isle,
;;                 "(extern constructor {from_name}_{ctor}_{to_name} {from_name}_{ctor}_{to_name})"
;;             );
;;             if !lossy {
;;                 fmtln!(
;;                     isle,
;;                     "(convert {from_name} {to_name} {from_name}_{ctor}_{to_name})"
;;                 );
;;             }
;;             fmtln!(rust, "#[inline]");
;;             fmtln!(
;;                 rust,
;;                 "fn {from_name}_{ctor}_{to_name}(&mut self, x: {from_name}) -> {rust_ret} {{"
;;             );
;;             rust.indent(|rust| {
;;                 if lossy {
;;                     fmtln!(rust, "{to_name}::try_from(x).ok()");
;;                 } else {
;;                     fmtln!(rust, "{to_name}::from(x)");
;;                 }
;;             });
;;             fmtln!(rust, "}}");
;;
;;             // Unwrapping constructor.
;;             if lossy {
;;                 fmtln!(
;;                     isle,
;;                     "(decl pure {from_name}_unwrap_into_{to_name} ({from_name}) {to_name})"
;;                 );
;;                 fmtln!(
;;                     isle,
;;                     "(extern constructor {from_name}_unwrap_into_{to_name} {from_name}_unwrap_into_{to_name})"
;;                 );
;;                 fmtln!(rust, "#[inline]");
;;                 fmtln!(
;;                     rust,
;;                     "fn {from_name}_unwrap_into_{to_name}(&mut self, x: {from_name}) -> {to_name} {{"
;;                 );
;;                 rust.indent(|rust| {
;;                     fmtln!(rust, "{to_name}::try_from(x).unwrap()");
;;                 });
;;                 fmtln!(rust, "}}");
;;             }
;;
;;             // Truncating constructor.
;;             if lossy && from.signed == to.signed {
;;                 fmtln!(
;;                     isle,
;;                     "(decl pure {from_name}_truncate_into_{to_name} ({from_name}) {to_name})"
;;                 );
;;                 fmtln!(
;;                     isle,
;;                     "(extern constructor {from_name}_truncate_into_{to_name} {from_name}_truncate_into_{to_name})"
;;                 );
;;                 fmtln!(rust, "#[inline]");
;;                 fmtln!(
;;                     rust,
;;                     "fn {from_name}_truncate_into_{to_name}(&mut self, x: {from_name}) -> {to_name} {{"
;;                 );
;;                 rust.indent(|rust| {
;;                     fmtln!(rust, "x as {to_name}");
;;                 });
;;                 fmtln!(rust, "}}");
;;             }
;;
;;             // Signed-to-unsigned reinterpreting constructor.
;;             if from.byte_width == to.byte_width {
;;                 debug_assert_ne!(from.signed, to.signed);
;;                 let cast_name = if to.signed {
;;                     "cast_signed"
;;                 } else {
;;                     "cast_unsigned"
;;                 };
;;                 fmtln!(
;;                     isle,
;;                     "(decl pure {from_name}_{cast_name} ({from_name}) {to_name})"
;;                 );
;;                 fmtln!(
;;                     isle,
;;                     "(extern constructor {from_name}_{cast_name} {from_name}_{cast_name})"
;;                 );
;;                 fmtln!(rust, "#[inline]");
;;                 fmtln!(
;;                     rust,
;;                     "fn {from_name}_{cast_name}(&mut self, x: {from_name}) -> {to_name} {{"
;;                 );
;;                 rust.indent(|rust| {
;;                     // TODO: Once our MSRV is >= 1.87, we should use
;;                     // `x.cast_[un]signed()` here.
;;                     fmtln!(rust, "x as {to_name}");
;;                 });
;;                 fmtln!(rust, "}}");
;;             }
;;
;;             // Extractor.
;;             fmtln!(
;;                 isle,
;;                 "(decl pure {to_name}_from_{from_name} ({to_name}) {from_name})"
;;             );
;;             fmtln!(
;;                 isle,
;;                 "(extern extractor {to_name}_from_{from_name} {from_name}_from_{to_name})"
;;             );
;;             fmtln!(rust, "#[inline]");
;;             fmtln!(
;;                 rust,
;;                 "fn {from_name}_from_{to_name}(&mut self, x: {from_name}) -> Option<{to_name}> {{"
;;             );
;;             rust.indent(|rust| {
;;                 if lossy {
;;                     fmtln!(rust, "x.try_into().ok()");
;;                 } else {
;;                     fmtln!(rust, "Some(x.into())");
;;                 }
;;             });
;;             fmtln!(rust, "}}");
;;
;;             isle.empty_line();
;;             rust.empty_line();
;;         }
;;     }
(defmacro define-numeric-conversions ()
  "Conversions between every pair of numeric types, named as in gen_isle.rs."
  (let (forms)
    (loop for (from fsigned fbits) in +numeric-types+ do
      (loop for (to tsigned tbits) in +numeric-types+
            unless (eq from to) do
              (let ((lossy (cond ((< fbits tbits) (and fsigned (not tsigned)))
                                 (t t))))
                (if lossy
                    (progn
                      (push `(defun ,(numeric-fn-name from "try-into" to) (x)
                               (checked x ,tsigned ,tbits))
                            forms)
                      (push `(defun ,(numeric-fn-name from "unwrap-into" to) (x)
                               (unwrap-or-panic (checked x ,tsigned ,tbits)
                                                ,(format nil "~a -> ~a conversion failed: ~~a" from to) x))
                            forms)
                      (when (eq fsigned tsigned)
                        (push `(defun ,(numeric-fn-name from "truncate-into" to) (x)
                                 (wrap-to x ,tsigned ,tbits))
                              forms)))
                    (push `(defun ,(numeric-fn-name from "into" to) (x) x) forms))
                (when (= fbits tbits)
                  (push `(defun ,(numeric-fn-name from (if tsigned "cast-signed" "cast-unsigned")) (x)
                           (wrap-to x ,tsigned ,tbits))
                        forms))
                ;; Extractor `{to}_from_{from}`: matches a FROM value that is
                ;; representable as TO, yielding it.
                (push `(defun ,(numeric-fn-name "match" to "from" from) (x)
                         (checked x ,tsigned ,tbits))
                      forms))))
    `(progn ,@(nreverse forms))))

(define-numeric-ops i8)
(define-numeric-ops u8)
(define-numeric-ops i16)
(define-numeric-ops u16)
(define-numeric-ops i32)
(define-numeric-ops u32)
(define-numeric-ops i64)
(define-numeric-ops u64)
(define-numeric-ops i128)
(define-numeric-ops u128)
(define-numeric-conversions)
