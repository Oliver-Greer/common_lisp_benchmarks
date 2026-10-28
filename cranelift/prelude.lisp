;;;; Lisp ports of the hand-written Rust externs used by the opts rules.
;;;;
;;;; Sources: cranelift/codegen/src/isle_prelude.rs, cranelift/codegen/src/opts.rs,
;;;; cranelift/codegen/src/ir/{types,immediates,condcodes}.rs and
;;;; cranelift/codegen/src/opts/div_const.rs.
;;;;
;;;; Each definition ported from Rust is preceded by the Rust code it ports,
;;;; with its file, line numbers and a GitHub link, all from wasmtime commit
;;;; 209d6c4d2963eed81edc839878cddb0a9dc15880. Definitions without such a
;;;; comment (PARSE-TYPE, IMM-SIGNED, ...) are Lisp-side plumbing for the
;;;; representation and have no Rust counterpart.
;;;;
;;;; Naming: an ISLE constructor `foo_bar` is the function FOO-BAR. An ISLE
;;;; extractor `foo_bar` is MATCH-FOO-BAR; it returns its outputs as multiple
;;;; values, with a NIL primary value meaning the extractor does not match.
;;;; Partial constructors return NIL for Rust's None.

(in-package #:cranelift-opts)

;;; CLIF types
;;;
;;; Types are symbols named as in CLIF text: i8 ... i128, f16 ... f128, and
;;; vectors <lane>x<count> such as i32x4. Dynamic vector types are not modeled.

(defun parse-type (ty)
  "Return (values lane-kind lane-bits lane-count) for type symbol TY."
  (let* ((name (string-downcase (symbol-name ty)))
         (x (position #\x name))
         (lane (subseq name 0 x))
         (count (if x (parse-integer name :start (1+ x)) 1))
         (kind (char lane 0))
         (bits (parse-integer lane :start 1)))
    (unless (and (member kind '(#\i #\f))
                 (member bits (if (char= kind #\i) '(8 16 32 64 128) '(16 32 64 128))))
      (error "Not a CLIF type: ~a" ty))
    (values (if (char= kind #\i) :int :float) bits count)))

(defun make-type (kind lane-bits count)
  (intern (string-upcase (format nil "~a~d~@[x~d~]" (if (eq kind :int) "i" "f")
                                 lane-bits (and (> count 1) count)))
          '#:cranelift-opts))

;; Rust: cranelift/codegen/src/ir/types.rs, line 68-77
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/types.rs#L68-L77
;;
;;     pub fn lane_bits(self) -> u32 {
;;         match self.lane_type() {
;;             I8 => 8,
;;             I16 | F16 => 16,
;;             I32 | F32 => 32,
;;             I64 | F64 => 64,
;;             I128 | F128 => 128,
;;             _ => 0,
;;         }
;;     }
(defun ty-lane-bits (ty) (nth-value 1 (parse-type ty)))

;; Rust: cranelift/codegen/src/ir/types.rs, line 275-281
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/types.rs#L275-L281
;;
;;     pub fn lane_count(self) -> u32 {
;;         if self.is_dynamic_vector() {
;;             0
;;         } else {
;;             1 << self.log2_lane_count()
;;         }
;;     }
(defun ty-lane-count (ty) (nth-value 2 (parse-type ty)))

;; Rust: cranelift/codegen/src/ir/types.rs, line 284-290
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/types.rs#L284-L290
;;
;;     pub fn bits(self) -> u32 {
;;         if self.is_dynamic_vector() {
;;             0
;;         } else {
;;             self.lane_bits() * self.lane_count()
;;         }
;;     }
(defun ty-bits* (ty)
  (multiple-value-bind (kind bits count) (parse-type ty)
    (declare (ignore kind))
    (* bits count)))

;; Rust: cranelift/codegen/src/ir/types.rs, line 221-223
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/types.rs#L221-L223
;;
;;     pub fn is_vector(self) -> bool {
;;         self.0 >= constants::VECTOR_BASE && !self.is_dynamic_vector()
;;     }
(defun ty-vector-p (ty) (> (ty-lane-count ty) 1))

;; Rust: cranelift/codegen/src/ir/types.rs, line 231-236
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/types.rs#L231-L236
;;
;;     pub fn is_int(self) -> bool {
;;         match self {
;;             I8 | I16 | I32 | I64 | I128 => true,
;;             _ => false,
;;         }
;;     }
(defun ty-is-int (ty) (and (eq (parse-type ty) :int) (not (ty-vector-p ty))))

;; Rust: cranelift/codegen/src/ir/types.rs, line 239-244
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/types.rs#L239-L244
;;
;;     pub fn is_float(self) -> bool {
;;         match self {
;;             F16 | F32 | F64 | F128 => true,
;;             _ => false,
;;         }
;;     }
(defun ty-is-float (ty) (and (eq (parse-type ty) :float) (not (ty-vector-p ty))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 788-790
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L788-L790
;;
;;     fn lane_type(&mut self, ty: Type) -> Type {
;;         ty.lane_type()
;;     }
;;
;; Rust: cranelift/codegen/src/ir/types.rs, line 40-46
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/types.rs#L40-L46
;;
;;     pub fn lane_type(self) -> Self {
;;         if self.0 < constants::VECTOR_BASE {
;;             self
;;         } else {
;;             Self(constants::LANE_BASE | (self.0 & 0x0f))
;;         }
;;     }
(defun lane-type (ty)
  (multiple-value-bind (kind bits) (parse-type ty)
    (make-type kind bits 1)))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 345-348
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L345-L348
;;
;;     fn ty_bits(&mut self, ty: Type) -> u8 {
;;         use core::convert::TryInto;
;;         ty.bits().try_into().unwrap()
;;     }
(defun ty-bits (ty)
  "ISLE `ty_bits` returns a u8, so it panics on wider types."
  (let ((bits (ty-bits* ty)))
    (if (< bits 256) bits (error "ty_bits: ~a does not fit in u8" ty))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 356-358
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L356-L358
;;
;;     fn ty_bits_u64(&mut self, ty: Type) -> u64 {
;;         ty.bits() as u64
;;     }
(defun ty-bits-u64 (ty) (ty-bits* ty))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 366-373
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L366-L373
;;
;;     fn ty_mask(&mut self, ty: Type) -> u64 {
;;         let ty_bits = ty.bits();
;;         debug_assert_ne!(ty_bits, 0);
;;         let shift = 64_u64
;;             .checked_sub(ty_bits.into())
;;             .expect("unimplemented for > 64 bits");
;;         u64::MAX >> shift
;;     }
(defun ty-shift (ty)
  (let ((bits (ty-bits* ty)))
    (when (> bits 64) (error "unimplemented for > 64 bits: ~a" ty))
    (- 64 bits)))
(defun ty-mask (ty) (ash (1- (ash 1 64)) (- (ty-shift ty))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 396-398
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L396-L398
;;
;;     fn ty_umax(&mut self, ty: Type) -> u64 {
;;         self.ty_mask(ty)
;;     }
(defun ty-umax (ty) (ty-mask ty))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 411-418
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L411-L418
;;
;;     fn ty_smax(&mut self, ty: Type) -> u64 {
;;         let ty_bits = ty.bits();
;;         debug_assert_ne!(ty_bits, 0);
;;         let shift = 64_u64
;;             .checked_sub(ty_bits.into())
;;             .expect("unimplemented for > 64 bits");
;;         (i64::MAX as u64) >> shift
;;     }
(defun ty-smax (ty) (ash (1- (ash 1 63)) (- (ty-shift ty))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 401-408
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L401-L408
;;
;;     fn ty_smin(&mut self, ty: Type) -> u64 {
;;         let ty_bits = ty.bits();
;;         debug_assert_ne!(ty_bits, 0);
;;         let shift = 64_u64
;;             .checked_sub(ty_bits.into())
;;             .expect("unimplemented for > 64 bits");
;;         (i64::MIN as u64) >> shift
;;     }
(defun ty-smin (ty) (ash (ash 1 63) (- (ty-shift ty))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 807-809
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L807-L809
;;
;;     fn ty_equal(&mut self, lhs: Type, rhs: Type) -> bool {
;;         lhs == rhs
;;     }
(defun ty-equal (a b) (eq a b))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 802-804
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L802-L804
;;
;;     fn ty_half_width(&mut self, ty: Type) -> Option<Type> {
;;         ty.half_width()
;;     }
;;
;; Rust: cranelift/codegen/src/ir/types.rs, line 173-184
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/types.rs#L173-L184
;;
;;     pub fn half_width(self) -> Option<Self> {
;;         Some(self.replace_lanes(match self.lane_type() {
;;             I16 => I8,
;;             I32 => I16,
;;             I64 => I32,
;;             I128 => I64,
;;             F32 => F16,
;;             F64 => F32,
;;             F128 => F64,
;;             _ => return None,
;;         }))
;;     }
(defun ty-half-width (ty)
  (multiple-value-bind (kind bits count) (parse-type ty)
    (when (if (eq kind :int) (> bits 8) (> bits 16))
      (make-type kind (/ bits 2) count))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 584-590
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L584-L590
;;
;;     fn ty_vector_not_float(&mut self, ty: Type) -> Option<Type> {
;;         if ty.is_vector() && !ty.lane_type().is_float() {
;;             Some(ty)
;;         } else {
;;             None
;;         }
;;     }
(defun ty-vector-not-float (ty)
  (and (ty-vector-p ty) (not (eq (parse-type ty) :float)) ty))

;;; Type extractors

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 420-426
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L420-L426
;;
;;     fn fits_in_16(&mut self, ty: Type) -> Option<Type> {
;;         if ty.bits() <= 16 && !ty.is_dynamic_vector() {
;;             Some(ty)
;;         } else {
;;             None
;;         }
;;     }
(defun match-fits-in-16 (ty) (and (<= (ty-bits* ty) 16) ty))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 449-455
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L449-L455
;;
;;     fn fits_in_64(&mut self, ty: Type) -> Option<Type> {
;;         if ty.bits() <= 64 && !ty.is_dynamic_vector() {
;;             Some(ty)
;;         } else {
;;             None
;;         }
;;     }
(defun match-fits-in-64 (ty) (and (<= (ty-bits* ty) 64) ty))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 543-545
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L543-L545
;;
;;     fn ty_int(&mut self, ty: Type) -> Option<Type> {
;;         ty.is_int().then(|| ty)
;;     }
(defun match-ty-int (ty) (and (ty-is-int ty) ty))

;; Rust: cranelift/codegen/src/opts.rs, line 213-215
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L213-L215
;;
;;     fn ty_vector(&mut self, ty: Type) -> Option<Type> {
;;         ty.is_vector().then_some(ty)
;;     }
(defun match-ty-vector (ty) (and (ty-vector-p ty) ty))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 611-617
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L611-L617
;;
;;     fn ty_vec128(&mut self, ty: Type) -> Option<Type> {
;;         if ty.is_vector() && ty.bits() == 128 {
;;             Some(ty)
;;         } else {
;;             None
;;         }
;;     }
(defun match-ty-vec128 (ty) (and (ty-vector-p ty) (= (ty-bits* ty) 128) ty))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 647-653
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L647-L653
;;
;;     fn ty_vec128_int(&mut self, ty: Type) -> Option<Type> {
;;         if ty.is_vector() && ty.bits() == 128 && ty.lane_type().is_int() {
;;             Some(ty)
;;         } else {
;;             None
;;         }
;;     }
(defun match-ty-vec128-int (ty)
  (and (match-ty-vec128 ty) (eq (parse-type ty) :int) ty))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 548-554
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L548-L554
;;
;;     fn ty_int_vec128(&mut self, ty: Type) -> Option<Type> {
;;         if ty.is_int() || (ty.is_vector() && ty.bits() == 128 && ty.lane_type().is_int()) {
;;             Some(ty)
;;         } else {
;;             None
;;         }
;;     }
(defun match-ty-int-vec128 (ty)
  (and (or (ty-is-int ty) (match-ty-vec128-int ty)) ty))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 467-469
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L467-L469
;;
;;     fn ty_int_ref_scalar_64_extract(&mut self, ty: Type) -> Option<Type> {
;;         self.ty_int_ref_scalar_64(ty)
;;     }
(defun match-ty-int-ref-scalar-64-extract (ty)
  ;; Reference types no longer exist in CLIF, so this is "scalar int <= 64".
  (and (<= (ty-bits* ty) 64) (not (ty-is-float ty)) (not (ty-vector-p ty)) ty))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 685-691
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L685-L691
;;
;;     fn multi_lane(&mut self, ty: Type) -> Option<(u32, u32)> {
;;         if ty.lane_count() > 1 {
;;             Some((ty.lane_bits(), ty.lane_count()))
;;         } else {
;;             None
;;         }
;;     }
(defun match-multi-lane (ty)
  (when (> (ty-lane-count ty) 1)
    (values (ty-lane-bits ty) (ty-lane-count ty))))

;;; Integer condition codes, as CLIF text names.

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 899-901
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L899-L901
;;
;;     fn intcc_complement(&mut self, cc: &IntCC) -> IntCC {
;;         cc.complement()
;;     }
;;
;; Rust: cranelift/codegen/src/ir/condcodes.rs, line 61-75
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/condcodes.rs#L61-L75
;;
;;     fn complement(self) -> Self {
;;         use self::IntCC::*;
;;         match self {
;;             Equal => NotEqual,
;;             NotEqual => Equal,
;;             SignedLessThan => SignedGreaterThanOrEqual,
;;             SignedGreaterThanOrEqual => SignedLessThan,
;;             SignedGreaterThan => SignedLessThanOrEqual,
;;             SignedLessThanOrEqual => SignedGreaterThan,
;;             UnsignedLessThan => UnsignedGreaterThanOrEqual,
;;             UnsignedGreaterThanOrEqual => UnsignedLessThan,
;;             UnsignedGreaterThan => UnsignedLessThanOrEqual,
;;             UnsignedLessThanOrEqual => UnsignedGreaterThan,
;;         }
;;     }
(defun intcc-complement (cc)
  (ecase cc
    (eq 'ne) (ne 'eq) (slt 'sge) (sge 'slt) (sgt 'sle) (sle 'sgt)
    (ult 'uge) (uge 'ult) (ugt 'ule) (ule 'ugt)))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 894-896
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L894-L896
;;
;;     fn intcc_swap_args(&mut self, cc: &IntCC) -> IntCC {
;;         cc.swap_args()
;;     }
;;
;; Rust: cranelift/codegen/src/ir/condcodes.rs, line 77-91
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/condcodes.rs#L77-L91
;;
;;     fn swap_args(self) -> Self {
;;         use self::IntCC::*;
;;         match self {
;;             Equal => Equal,
;;             NotEqual => NotEqual,
;;             SignedGreaterThan => SignedLessThan,
;;             SignedGreaterThanOrEqual => SignedLessThanOrEqual,
;;             SignedLessThan => SignedGreaterThan,
;;             SignedLessThanOrEqual => SignedGreaterThanOrEqual,
;;             UnsignedGreaterThan => UnsignedLessThan,
;;             UnsignedGreaterThanOrEqual => UnsignedLessThanOrEqual,
;;             UnsignedLessThan => UnsignedGreaterThan,
;;             UnsignedLessThanOrEqual => UnsignedGreaterThanOrEqual,
;;         }
;;     }
(defun intcc-swap-args (cc)
  (ecase cc
    (eq 'eq) (ne 'ne) (slt 'sgt) (sgt 'slt) (sge 'sle) (sle 'sge)
    (ult 'ugt) (ugt 'ult) (uge 'ule) (ule 'uge)))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 862-875
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L862-L875
;;
;;     fn signed_cond_code(&mut self, cc: &IntCC) -> Option<IntCC> {
;;         match cc {
;;             IntCC::Equal
;;             | IntCC::UnsignedGreaterThanOrEqual
;;             | IntCC::UnsignedGreaterThan
;;             | IntCC::UnsignedLessThanOrEqual
;;             | IntCC::UnsignedLessThan
;;             | IntCC::NotEqual => None,
;;             IntCC::SignedGreaterThanOrEqual
;;             | IntCC::SignedGreaterThan
;;             | IntCC::SignedLessThanOrEqual
;;             | IntCC::SignedLessThan => Some(*cc),
;;         }
;;     }
(defun signed-cond-code (cc)
  (and (member cc '(sge sgt sle slt)) cc))

;;; Imm64
;;;
;;; An Imm64 leaf is the immediate's bits as an unsigned 64-bit integer. CLIF's
;;; verifier requires iconst immediates to be zero-extended to the type width,
;;; so for iconst this is also the value masked to the type.

(defun imm-signed (x) (wrap-to x t 64))
(defun imm-unsigned (x) (wrap-to x nil 64))

;; Rust: cranelift/codegen/src/ir/immediates.rs, line 95-109
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/immediates.rs#L95-L109
;;
;;     pub fn sign_extend_from_width(&self, bit_width: u32) -> Self {
;;         debug_assert!(
;;             bit_width.is_power_of_two(),
;;             "{bit_width} is not a power of two"
;;         );
;;
;;         if bit_width >= 64 {
;;             return *self;
;;         }
;;
;;         let bit_width = i64::from(bit_width);
;;         let delta = 64 - bit_width;
;;         let sign_extended = (self.0 << delta) >> delta;
;;         Imm64(sign_extended)
;;     }
(defun sign-extend-from-width (x width)
  (if (>= width 64) (imm-unsigned x) (imm-unsigned (wrap-to x t width))))

;; Rust: cranelift/codegen/src/ir/immediates.rs, line 79-90
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/immediates.rs#L79-L90
;;
;;     pub(crate) fn mask_to_width(&self, bit_width: u32) -> Self {
;;         debug_assert!(bit_width.is_power_of_two());
;;
;;         if bit_width >= 64 {
;;             return *self;
;;         }
;;
;;         let bit_width = i64::from(bit_width);
;;         let mask = (1 << bit_width) - 1;
;;         let masked = self.0 & mask;
;;         Imm64(masked)
;;     }
(defun mask-to-width (x width)
  (if (>= width 64) (imm-unsigned x) (ldb (byte width 0) x)))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 773-775
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L773-L775
;;
;;     fn imm64(&mut self, x: u64) -> Imm64 {
;;         Imm64::new(x as i64)
;;     }
(defun imm64 (x) (imm-unsigned x))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 664-666
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L664-L666
;;
;;     fn u64_from_imm64(&mut self, imm: Imm64) -> u64 {
;;         imm.bits() as u64
;;     }
(defun match-u64-from-imm64 (imm) imm)

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 778-780
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L778-L780
;;
;;     fn imm64_masked(&mut self, ty: Type, x: u64) -> Imm64 {
;;         Imm64::new((x & self.ty_mask(ty)) as i64)
;;     }
(defun imm64-masked (ty x) (logand x (ty-mask ty)))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 319-321
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L319-L321
;;
;;     fn u64_uextend_imm64(&mut self, ty: Type, x: Imm64) -> u64 {
;;         (x.bits() as u64) & self.ty_mask(ty)
;;     }
(defun u64-uextend-imm64 (ty x) (logand x (ty-mask ty)))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 314-316
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L314-L316
;;
;;     fn i64_sextend_imm64(&mut self, ty: Type, x: Imm64) -> i64 {
;;         x.sign_extend_from_width(ty.bits()).bits()
;;     }
(defun i64-sextend-imm64 (ty x) (imm-signed (sign-extend-from-width x (ty-bits* ty))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 308-311
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L308-L311
;;
;;     fn i64_sextend_u64(&mut self, ty: Type, x: u64) -> i64 {
;;         let shift_amt = core::cmp::max(0, 64 - ty.bits());
;;         ((x as i64) << shift_amt) >> shift_amt
;;     }
(defun i64-sextend-u64 (ty x)
  (let ((shift (max 0 (- 64 (ty-bits* ty)))))
    (ash (wrap-to (ash x shift) t 64) (- shift))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 115-118
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L115-L118
;;
;;     fn imm64_add(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         let ty_mask = self.ty_mask(ty) as i64;
;;         Imm64::new(x.bits().wrapping_add(y.bits()) & ty_mask)
;;     }
;;
;; Rust: cranelift/codegen/src/isle_prelude.rs, line 121-124
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L121-L124
;;
;;     fn imm64_sub(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         let ty_mask = self.ty_mask(ty) as i64;
;;         Imm64::new(x.bits().wrapping_sub(y.bits()) & ty_mask)
;;     }
;;
;; Rust: cranelift/codegen/src/isle_prelude.rs, line 127-130
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L127-L130
;;
;;     fn imm64_mul(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         let ty_mask = self.ty_mask(ty) as i64;
;;         Imm64::new(x.bits().wrapping_mul(y.bits()) & ty_mask)
;;     }
;;
;; Rust: cranelift/codegen/src/isle_prelude.rs, line 133-136
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L133-L136
;;
;;     fn imm64_and(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         let ty_mask = self.ty_mask(ty) as i64;
;;         Imm64::new((x.bits() & y.bits()) & ty_mask)
;;     }
;;
;; Rust: cranelift/codegen/src/isle_prelude.rs, line 139-142
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L139-L142
;;
;;     fn imm64_or(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         let ty_mask = self.ty_mask(ty) as i64;
;;         Imm64::new((x.bits() | y.bits()) & ty_mask)
;;     }
;;
;; Rust: cranelift/codegen/src/isle_prelude.rs, line 145-148
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L145-L148
;;
;;     fn imm64_xor(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         let ty_mask = self.ty_mask(ty) as i64;
;;         Imm64::new((x.bits() ^ y.bits()) & ty_mask)
;;     }
(macrolet ((define-masked-binop (name op)
             `(defun ,name (ty x y) (logand (,op x y) (ty-mask ty)))))
  (define-masked-binop imm64-add +)
  (define-masked-binop imm64-sub -)
  (define-masked-binop imm64-mul *)
  (define-masked-binop imm64-and logand)
  (define-masked-binop imm64-or logior)
  (define-masked-binop imm64-xor logxor))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 157-160
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L157-L160
;;
;;     fn imm64_neg(&mut self, ty: Type, x: Imm64) -> Imm64 {
;;         let ty_mask = self.ty_mask(ty) as i64;
;;         Imm64::new(x.bits().wrapping_neg() & ty_mask)
;;     }
(defun imm64-neg (ty x) (logand (- x) (ty-mask ty)))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 151-154
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L151-L154
;;
;;     fn imm64_not(&mut self, ty: Type, x: Imm64) -> Imm64 {
;;         let ty_mask = self.ty_mask(ty) as i64;
;;         Imm64::new((!x.bits()) & ty_mask)
;;     }
(defun imm64-not (ty x) (logand (lognot x) (ty-mask ty)))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 223-231
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L223-L231
;;
;;     fn imm64_shl(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         // Mask off any excess shift bits.
;;         let shift_mask = (ty.bits() - 1) as u64;
;;         let y = (y.bits() as u64) & shift_mask;
;;
;;         // Mask the result to `ty` bits.
;;         let ty_mask = self.ty_mask(ty) as i64;
;;         Imm64::new((x.bits() << y) & ty_mask)
;;     }
(defun imm64-shl (ty x y)
  (let ((y (logand y (1- (ty-bits* ty)))))
    (logand (ash x y) (ty-mask ty))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 234-244
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L234-L244
;;
;;     fn imm64_ushr(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         let ty_mask = self.ty_mask(ty);
;;         let x = (x.bits() as u64) & ty_mask;
;;
;;         // Mask off any excess shift bits.
;;         let shift_mask = (ty.bits() - 1) as u64;
;;         let y = (y.bits() as u64) & shift_mask;
;;
;;         // NB: No need to mask off high bits because they are already zero.
;;         Imm64::new((x >> y) as i64)
;;     }
(defun imm64-ushr (ty x y)
  (let ((y (logand y (1- (ty-bits* ty)))))
    (ash (logand x (ty-mask ty)) (- y))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 247-259
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L247-L259
;;
;;     fn imm64_sshr(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         // Sign extend `x` from `ty.bits()`-width to the full 64 bits.
;;         let shift = u32::checked_sub(64, ty.bits()).unwrap_or(0);
;;         let x = (x.bits() << shift) >> shift;
;;
;;         // Mask off any excess shift bits.
;;         let shift_mask = (ty.bits() - 1) as i64;
;;         let y = y.bits() & shift_mask;
;;
;;         // Mask off sign bits that aren't part of `ty`.
;;         let ty_mask = self.ty_mask(ty) as i64;
;;         Imm64::new((x >> y) & ty_mask)
;;     }
(defun imm64-sshr (ty x y)
  (let* ((bits (ty-bits* ty))
         (sx (wrap-to x t (min bits 64)))
         (y (logand y (1- bits))))
    (logand (ash sx (- y)) (ty-mask ty))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 262-283
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L262-L283
;;
;;     fn imm64_rotl(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         let bits = ty.bits();
;;         assert!(bits <= 64);
;;         // This holds for all Cranelift types ({u/i}{8,16,32,64})
;;         debug_assert!(bits.is_power_of_two());
;;
;;         let ty_mask = self.ty_mask(ty);
;;         let x = (x.bits() as u64) & ty_mask;
;;
;;         // Mask off any excess rotate bits so the rotate stays within `ty`.
;;         let shift_mask = bits - 1;
;;         let y = ((y.bits() as u64) & u64::from(shift_mask)) as u32;
;;
;;         // In Rust, x >> 64 or x << 64 panics.
;;         let result = if y == 0 {
;;             x
;;         } else {
;;             (x << y) | (x >> (u32::from(bits) - y))
;;         };
;;
;;         Imm64::new((result & ty_mask) as i64)
;;     }
;;
;; Rust: cranelift/codegen/src/isle_prelude.rs, line 286-305
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L286-L305
;;
;;     fn imm64_rotr(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         let bits = ty.bits();
;;         assert!(bits <= 64);
;;         debug_assert!(bits.is_power_of_two());
;;
;;         let ty_mask = self.ty_mask(ty);
;;         let x = (x.bits() as u64) & ty_mask;
;;
;;         // Mask off any excess rotate bits so the rotate stays within `ty`.
;;         let shift_mask = bits - 1;
;;         let y = ((y.bits() as u64) & u64::from(shift_mask)) as u32;
;;
;;         let result = if y == 0 {
;;             x
;;         } else {
;;             (x >> y) | (x << (u32::from(bits) - y))
;;         };
;;
;;         Imm64::new((result & ty_mask) as i64)
;;     }
(defun imm64-rotate (ty x y leftp)
  (let* ((bits (ty-bits* ty))
         (mask (ty-mask ty))
         (x (logand x mask))
         (y (logand y (1- bits))))
    (if (zerop y)
        x
        (logand (if leftp
                    (logior (ash x y) (ash x (- y bits)))
                    (logior (ash x (- y)) (ash x (- bits y))))
                mask))))
(defun imm64-rotl (ty x y) (imm64-rotate ty x y t))
(defun imm64-rotr (ty x y) (imm64-rotate ty x y nil))

(defun imm64-signed-pair (ty x y)
  (let ((width (ty-bits* ty)))
    (assert (<= width 64))
    (values (imm-signed (sign-extend-from-width x width))
            (imm-signed (sign-extend-from-width y width))
            width)))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 55-75
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L55-L75
;;
;;     fn imm64_sdiv(&mut self, ty: Type, x: Imm64, y: Imm64) -> Option<Imm64> {
;;         // Sign extend `x` and `y`.
;;         let type_width = ty.bits();
;;         assert!(type_width <= 64);
;;         let x = x.sign_extend_from_width(type_width).bits();
;;         let y = y.sign_extend_from_width(type_width).bits();
;;         let shift = 64 - type_width;
;;
;;         // NB: We can't rely on `checked_div` to detect `ty::MIN / -1`
;;         // (which overflows and should trap) because we are working with
;;         // `i64` values here, and `i32::MIN != i64::MIN`, for
;;         // example. Therefore, we have to explicitly check for this case
;;         // ourselves.
;;         let min = ((self.ty_smin(ty) as i64) << shift) >> shift;
;;         if x == min && y == -1 {
;;             return None;
;;         }
;;
;;         let result = x.checked_div(y)?;
;;         Some(Imm64::new(result).mask_to_width(type_width))
;;     }
(defun imm64-sdiv (ty x y)
  (multiple-value-bind (x y width) (imm64-signed-pair ty x y)
    (let ((min (wrap-to (ty-smin ty) t width)))
      (unless (or (and (= x min) (= y -1)) (zerop y))
        (mask-to-width (imm-unsigned (truncate x y)) width)))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 89-101
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L89-L101
;;
;;     fn imm64_srem(&mut self, ty: Type, x: Imm64, y: Imm64) -> Option<Imm64> {
;;         // Sign extend `x` and `y`.
;;         let type_width = ty.bits();
;;         assert!(type_width <= 64);
;;         let x = x.sign_extend_from_width(type_width).bits();
;;         let y = y.sign_extend_from_width(type_width).bits();
;;
;;         // iN::min % -1 is defined as 0 in wasm so no need
;;         // to check for it
;;
;;         let result = x.checked_rem(y)?;
;;         Some(Imm64::new(result).mask_to_width(type_width))
;;     }
(defun imm64-srem (ty x y)
  (multiple-value-bind (x y width) (imm64-signed-pair ty x y)
    (let ((r (i64-checked-rem x y)))
      (and r (mask-to-width (imm-unsigned r) width)))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 78-86
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L78-L86
;;
;;     fn imm64_udiv(&mut self, ty: Type, x: Imm64, y: Imm64) -> Option<Imm64> {
;;         let type_width = ty.bits();
;;         assert!(type_width <= 64);
;;         let mask = self.ty_mask(ty);
;;         let x = (x.bits() as u64) & mask;
;;         let y = (y.bits() as u64) & mask;
;;         let result = x.checked_div(y)?;
;;         Some(Imm64::new(result as i64).mask_to_width(type_width))
;;     }
(defun imm64-udiv (ty x y)
  (let ((mask (ty-mask ty)))
    (unless (zerop (logand y mask))
      (values (truncate (logand x mask) (logand y mask))))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 104-112
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L104-L112
;;
;;     fn imm64_urem(&mut self, ty: Type, x: Imm64, y: Imm64) -> Option<Imm64> {
;;         let type_width = ty.bits();
;;         assert!(type_width <= 64);
;;         let mask = self.ty_mask(ty);
;;         let x = (x.bits() as u64) & mask;
;;         let y = (y.bits() as u64) & mask;
;;         let result = x.checked_rem(y)?;
;;         Some(Imm64::new(result as i64).mask_to_width(type_width))
;;     }
(defun imm64-urem (ty x y)
  (let ((mask (ty-mask ty)))
    (unless (zerop (logand y mask))
      (rem (logand x mask) (logand y mask)))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 203-210
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L203-L210
;;
;;     fn imm64_smin(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         let type_width = ty.bits();
;;         assert!(type_width <= 64);
;;         let x_s = x.sign_extend_from_width(type_width).bits();
;;         let y_s = y.sign_extend_from_width(type_width).bits();
;;         let selected = if x_s <= y_s { x_s } else { y_s };
;;         Imm64::new(selected).mask_to_width(type_width)
;;     }
(defun imm64-smin (ty x y)
  (multiple-value-bind (x y width) (imm64-signed-pair ty x y)
    (mask-to-width (imm-unsigned (min x y)) width)))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 213-220
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L213-L220
;;
;;     fn imm64_smax(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         let type_width = ty.bits();
;;         assert!(type_width <= 64);
;;         let x_s = x.sign_extend_from_width(type_width).bits();
;;         let y_s = y.sign_extend_from_width(type_width).bits();
;;         let selected = if x_s >= y_s { x_s } else { y_s };
;;         Imm64::new(selected).mask_to_width(type_width)
;;     }
(defun imm64-smax (ty x y)
  (multiple-value-bind (x y width) (imm64-signed-pair ty x y)
    (mask-to-width (imm-unsigned (max x y)) width)))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 187-192
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L187-L192
;;
;;     fn imm64_umin(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         let ty_mask = self.ty_mask(ty);
;;         let x_u = (x.bits() as u64) & ty_mask;
;;         let y_u = (y.bits() as u64) & ty_mask;
;;         Imm64::new((if x_u <= y_u { x_u } else { y_u }) as i64)
;;     }
(defun imm64-umin (ty x y) (min (logand x (ty-mask ty)) (logand y (ty-mask ty))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 195-200
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L195-L200
;;
;;     fn imm64_umax(&mut self, ty: Type, x: Imm64, y: Imm64) -> Imm64 {
;;         let ty_mask = self.ty_mask(ty);
;;         let x_u = (x.bits() as u64) & ty_mask;
;;         let y_u = (y.bits() as u64) & ty_mask;
;;         Imm64::new((if x_u >= y_u { x_u } else { y_u }) as i64)
;;     }
(defun imm64-umax (ty x y) (max (logand x (ty-mask ty)) (logand y (ty-mask ty))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 31-38
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L31-L38
;;
;;     fn imm64_clz(&mut self, ty: Type, a: Imm64) -> Imm64 {
;;         let bits = ty.bits();
;;         assert!(bits <= 64);
;;         let clz_offset = 64 - bits;
;;         let a_v: u64 = a.bits().cast_unsigned();
;;         let lz = a_v.leading_zeros() - clz_offset;
;;         Imm64::new(i64::from(lz))
;;     }
(defun imm64-clz (ty a)
  (let ((bits (ty-bits* ty)))
    (assert (<= bits 64))
    (- (leading-zeros a 64) (- 64 bits))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 41-52
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L41-L52
;;
;;     fn imm64_ctz(&mut self, ty: Type, a: Imm64) -> Imm64 {
;;         let bits = ty.bits();
;;         assert!(bits <= 64);
;;         let a_v: u64 = a.bits().cast_unsigned();
;;         if a_v == 0 {
;;             // ctz(0) is defined to be the number of bits in the type.
;;             Imm64::new(i64::from(bits))
;;         } else {
;;             let lz = a_v.trailing_zeros();
;;             Imm64::new(i64::from(lz))
;;         }
;;     }
(defun imm64-ctz (ty a)
  (let ((bits (ty-bits* ty)))
    (assert (<= bits 64))
    (if (zerop a) bits (trailing-zeros a 64))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 324-342
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L324-L342
;;
;;     fn imm64_icmp(&mut self, ty: Type, cc: &IntCC, x: Imm64, y: Imm64) -> Imm64 {
;;         let ux = self.u64_uextend_imm64(ty, x);
;;         let uy = self.u64_uextend_imm64(ty, y);
;;         let sx = self.i64_sextend_imm64(ty, x);
;;         let sy = self.i64_sextend_imm64(ty, y);
;;         let result = match cc {
;;             IntCC::Equal => ux == uy,
;;             IntCC::NotEqual => ux != uy,
;;             IntCC::UnsignedGreaterThanOrEqual => ux >= uy,
;;             IntCC::UnsignedGreaterThan => ux > uy,
;;             IntCC::UnsignedLessThanOrEqual => ux <= uy,
;;             IntCC::UnsignedLessThan => ux < uy,
;;             IntCC::SignedGreaterThanOrEqual => sx >= sy,
;;             IntCC::SignedGreaterThan => sx > sy,
;;             IntCC::SignedLessThanOrEqual => sx <= sy,
;;             IntCC::SignedLessThan => sx < sy,
;;         };
;;         Imm64::new(result.into())
;;     }
(defun imm64-icmp (ty cc x y)
  (let ((ux (u64-uextend-imm64 ty x)) (uy (u64-uextend-imm64 ty y))
        (sx (i64-sextend-imm64 ty x)) (sy (i64-sextend-imm64 ty y)))
    (if (ecase cc
          (eq (= ux uy)) (ne (/= ux uy))
          (uge (>= ux uy)) (ugt (> ux uy)) (ule (<= ux uy)) (ult (< ux uy))
          (sge (>= sx sy)) (sgt (> sx sy)) (sle (<= sx sy)) (slt (< sx sy)))
        1 0)))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 669-677
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L669-L677
;;
;;     fn imm64_power_of_two(&mut self, x: Imm64) -> Option<u64> {
;;         let x = i64::from(x);
;;         let x = u64::try_from(x).ok()?;
;;         if x.is_power_of_two() {
;;             Some(x.trailing_zeros().into())
;;         } else {
;;             None
;;         }
;;     }
(defun match-imm64-power-of-two (x)
  (let ((x (imm-signed x)))
    (when (and (plusp x) (zerop (logand x (1- x))))
      (trailing-zeros x 64))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 19-23
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L19-L23
;;
;;     fn checked_add_with_type(&mut self, ty: Type, a: u64, b: u64) -> Option<u64> {
;;         let c = a.checked_add(b)?;
;;         let ty_mask = self.ty_mask(ty);
;;         if (c & !ty_mask) == 0 { Some(c) } else { None }
;;     }
(defun checked-add-with-type (ty a b)
  (let ((c (u64-checked-add a b)))
    (and c (zerop (logand c (lognot (ty-mask ty)))) c)))

;; Rust: cranelift/codegen/src/opts.rs, line 390-392
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L390-L392
;;
;;     fn u64_bswap16(&mut self, n: u64) -> u64 {
;;         (n as u16).swap_bytes() as u64
;;     }
;;
;; Rust: cranelift/codegen/src/opts.rs, line 394-396
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L394-L396
;;
;;     fn u64_bswap32(&mut self, n: u64) -> u64 {
;;         (n as u32).swap_bytes() as u64
;;     }
;;
;; Rust: cranelift/codegen/src/opts.rs, line 398-400
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L398-L400
;;
;;     fn u64_bswap64(&mut self, n: u64) -> u64 {
;;         n.swap_bytes()
;;     }
(defun u64-bswap (n bytes)
  (loop with r = 0
        for i below bytes
        do (setf r (logior (ash r 8) (ldb (byte 8 (* 8 i)) n)))
        finally (return r)))
(defun u64-bswap16 (n) (u64-bswap n 2))
(defun u64-bswap32 (n) (u64-bswap n 4))
(defun u64-bswap64 (n) (u64-bswap n 8))

;;; Constants
;;;
;;; A Constant leaf (vconst, f128const) is the constant's bytes read as a
;;; little-endian unsigned integer; its length is implied by the type.

;; Rust: cranelift/codegen/src/opts.rs, line 204-207
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L204-L207
;;
;;     fn zero_constant(&mut self, ty: Type) -> Constant {
;;         let data = vec![0; ty.bytes() as usize];
;;         self.ctx.func.dfg.constants.insert(data.into())
;;     }
(defun zero-constant (ty) (declare (ignore ty)) 0)

;; Rust: cranelift/codegen/src/opts.rs, line 349-354
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L349-L354
;;
;;     fn splat64(&mut self, val: u64) -> Constant {
;;         let val = u128::from(val);
;;         let val = val | (val << 64);
;;         let imm = V128Imm(val.to_le_bytes());
;;         self.ctx.func.dfg.constants.insert(imm.into())
;;     }
(defun splat64 (val) (logior val (ash val 64)))

;; Rust: cranelift/codegen/src/opts.rs, line 356-359
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L356-L359
;;
;;     fn scalar_to_vector_const64(&mut self, val: u64) -> Constant {
;;         let imm = V128Imm(u128::from(val).to_le_bytes());
;;         self.ctx.func.dfg.constants.insert(imm.into())
;;     }
(defun scalar-to-vector-const64 (val) val)

;; Rust: cranelift/codegen/src/opts.rs, line 406-408
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L406-L408
;;
;;     fn ieee128_constant(&mut self, n: Ieee128) -> Constant {
;;         self.ctx.func.dfg.constants.insert(n.into())
;;     }
(defun ieee128-constant (n) n)

;; Rust: cranelift/codegen/src/opts.rs, line 402-404
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L402-L404
;;
;;     fn ieee128_constant_extractor(&mut self, n: Constant) -> Option<Ieee128> {
;;         self.ctx.func.dfg.constants.get(n).try_into().ok()
;;     }
(defun match-ieee128-constant (c) c)

;;; Floats
;;;
;;; Ieee16/32/64/128 leaves are raw bit patterns. f32 and f64 arithmetic uses
;;; the host's IEEE operations with traps masked; f16 and f128 only need bit-
;;; level operations and comparisons, done exactly on rationals.

(defun float-sign-mask (width) (ash 1 (1- width)))

;; Rust: cranelift/codegen/src/ir/immediates.rs, line 765-771
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/immediates.rs#L765-L771
;;
;;     impl Neg for $name {
;;         type Output = Self;
;;
;;         fn neg(self) -> Self {
;;             Self::with_bits(self.bits() ^ Self::SIGN_MASK)
;;         }
;;     }
(defun float-neg (bits width) (logxor bits (float-sign-mask width)))

;; Rust: cranelift/codegen/src/ir/immediates.rs, line 584-586
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/immediates.rs#L584-L586
;;
;;     pub fn abs(self) -> Self {
;;         Self::with_bits(self.bits() & !Self::SIGN_MASK)
;;     }
(defun float-abs (bits width) (logandc2 bits (float-sign-mask width)))

;; Rust: cranelift/codegen/src/ir/immediates.rs, line 589-591
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/immediates.rs#L589-L591
;;
;;     pub fn copysign(self, sign: Self) -> Self {
;;         Self::with_bits((self.bits() & !Self::SIGN_MASK) | (sign.bits() & Self::SIGN_MASK))
;;     }
(defun float-copysign (a b width)
  (logior (float-abs a width) (logand b (float-sign-mask width))))

(defun float-format (width)
  "(values exponent-bits mantissa-bits) of the IEEE binary format of WIDTH."
  (ecase width (16 (values 5 10)) (32 (values 8 23)) (64 (values 11 52)) (128 (values 15 112))))

(defun decode-float-bits (bits width)
  "Return :NAN, or (values sign magnitude) where magnitude is a rational or :INF."
  (multiple-value-bind (ebits mbits) (float-format width)
    (let* ((sign (if (logbitp (1- width) bits) -1 1))
           (exp (ldb (byte ebits mbits) bits))
           (mant (ldb (byte mbits 0) bits))
           (emax (1- (ash 1 ebits)))
           (bias (1- (ash 1 (1- ebits)))))
      (cond ((and (= exp emax) (plusp mant)) :nan)
            ((= exp emax) (values sign :inf))
            ((zerop exp) (values sign (* mant (expt 2 (- 1 bias mbits)))))
            (t (values sign (* (+ mant (ash 1 mbits)) (expt 2 (- exp bias mbits)))))))))

(defun float-nan-bits-p (bits width) (eq (decode-float-bits bits width) :nan))

(defun float-compare (a b width)
  "-1, 0 or 1 comparing non-NaN floats A and B by value (so -0 = +0)."
  (flet ((key (bits)
           (multiple-value-bind (sign mag) (decode-float-bits bits width)
             (if (eq mag :inf) (* sign (expt 2 20000)) (* sign mag)))))
    (let ((ka (key a)) (kb (key b)))
      (cond ((< ka kb) -1) ((> ka kb) 1) (t 0)))))

(defun float-zero-p (bits width) (zerop (float-abs bits width)))

;; Rust: cranelift/codegen/src/ir/immediates.rs, line 594-609
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/immediates.rs#L594-L609
;;
;;     pub fn minimum(self, other: Self) -> Self {
;;         // FIXME: Replace with Rust float method once it is stabilised.
;;         if self.is_nan() || other.is_nan() {
;;             Self::NAN
;;         } else if self.is_zero() && other.is_zero() {
;;             if self.is_negative() {
;;                 self
;;             } else {
;;                 other
;;             }
;;         } else if self <= other {
;;             self
;;         } else {
;;             other
;;         }
;;     }
;;
;; Rust: cranelift/codegen/src/ir/immediates.rs, line 678-680
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/immediates.rs#L678-L680
;;
;;     pub fn non_nan(self) -> Option<Self> {
;;         Some(self).filter(|f| !f.is_nan())
;;     }
(defun float-minimum (a b width)
  "IEEE 754-2019 minimum, then `non_nan`: NIL if either input is NaN."
  (cond ((or (float-nan-bits-p a width) (float-nan-bits-p b width)) nil)
        ((and (float-zero-p a width) (float-zero-p b width))
         (if (logbitp (1- width) a) a b))
        ((<= (float-compare a b width) 0) a)
        (t b)))

;; Rust: cranelift/codegen/src/ir/immediates.rs, line 612-627
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/immediates.rs#L612-L627
;;
;;     pub fn maximum(self, other: Self) -> Self {
;;         // FIXME: Replace with Rust float method once it is stabilised.
;;         if self.is_nan() || other.is_nan() {
;;             Self::NAN
;;         } else if self.is_zero() && other.is_zero() {
;;             if self.is_positive() {
;;                 self
;;             } else {
;;                 other
;;             }
;;         } else if self >= other {
;;             self
;;         } else {
;;             other
;;         }
;;     }
(defun float-maximum (a b width)
  (cond ((or (float-nan-bits-p a width) (float-nan-bits-p b width)) nil)
        ((and (float-zero-p a width) (float-zero-p b width))
         (if (logbitp (1- width) a) b a))
        ((>= (float-compare a b width) 0) a)
        (t b)))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 1007-1025
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L1007-L1025
;;
;;     fn f16_min(&mut self, a: Ieee16, b: Ieee16) -> Option<Ieee16> {
;;         a.minimum(b).non_nan()
;;     }
;;
;;     fn f16_max(&mut self, a: Ieee16, b: Ieee16) -> Option<Ieee16> {
;;         a.maximum(b).non_nan()
;;     }
;;
;;     fn f16_neg(&mut self, n: Ieee16) -> Ieee16 {
;;         -n
;;     }
;;
;;     fn f16_abs(&mut self, n: Ieee16) -> Ieee16 {
;;         n.abs()
;;     }
;;
;;     fn f16_copysign(&mut self, a: Ieee16, b: Ieee16) -> Ieee16 {
;;         a.copysign(b)
;;     }
;;
;; Rust: cranelift/codegen/src/isle_prelude.rs, line 1063-1081
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L1063-L1081
;;
;; Rust: cranelift/codegen/src/isle_prelude.rs, line 1119-1137
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L1119-L1137
;;
;; Rust: cranelift/codegen/src/isle_prelude.rs, line 1139-1157
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L1139-L1157
(macrolet ((define-bit-float-ops (prefix width)
             (flet ((name (op) (intern (format nil "~a-~a" prefix op) '#:cranelift-opts)))
               `(progn
                  (defun ,(name "NEG") (n) (float-neg n ,width))
                  (defun ,(name "ABS") (n) (float-abs n ,width))
                  (defun ,(name "COPYSIGN") (a b) (float-copysign a b ,width))
                  (defun ,(name "MIN") (a b) (float-minimum a b ,width))
                  (defun ,(name "MAX") (a b) (float-maximum a b ,width))))))
  (define-bit-float-ops "F16" 16)
  (define-bit-float-ops "F32" 32)
  (define-bit-float-ops "F64" 64)
  (define-bit-float-ops "F128" 128))

;; Rust: cranelift/codegen/src/opts.rs, line 209-211
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L209-L211
;;
;;     fn f16_zero(&mut self) -> Ieee16 {
;;         Ieee16::with_bits(0)
;;     }
(defun f16-zero () 0)

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 720-722
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L720-L722
;;
;;     fn u16_from_ieee16(&mut self, val: Ieee16) -> u16 {
;;         val.bits()
;;     }
(defun match-u16-from-ieee16 (bits) bits)

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 724-726
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L724-L726
;;
;;     fn u32_from_ieee32(&mut self, val: Ieee32) -> u32 {
;;         val.bits()
;;     }
(defun match-u32-from-ieee32 (bits) bits)

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 728-730
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L728-L730
;;
;;     fn u64_from_ieee64(&mut self, val: Ieee64) -> u64 {
;;         val.bits()
;;     }
(defun match-u64-from-ieee64 (bits) bits)

(defun bits->float (bits width)
  (ecase width
    (32 (float-features:bits-single-float bits))
    (64 (float-features:bits-double-float bits))))

(defun float->bits (x width)
  (ldb (byte width 0)
       (ecase width
         (32 (float-features:single-float-bits x))
         (64 (float-features:double-float-bits x)))))

;; Rust: cranelift/codegen/src/ir/immediates.rs, line 782-812
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/immediates.rs#L782-L812
;;
;;     impl Add for $name {
;;         type Output = Self;
;;
;;         fn add(self, rhs: Self) -> Self {
;;             Self::with_float(self.$as_float() + rhs.$as_float())
;;         }
;;     }
;;
;;     impl Sub for $name {
;;         type Output = Self;
;;
;;         fn sub(self, rhs: Self) -> Self {
;;             Self::with_float(self.$as_float() - rhs.$as_float())
;;         }
;;     }
;;
;;     impl Mul for $name {
;;         type Output = Self;
;;
;;         fn mul(self, rhs: Self) -> Self {
;;             Self::with_float(self.$as_float() * rhs.$as_float())
;;         }
;;     }
;;
;;     impl Div for $name {
;;         type Output = Self;
;;
;;         fn div(self, rhs: Self) -> Self::Output {
;;             Self::with_float(self.$as_float() / rhs.$as_float())
;;         }
;;     }
(defun host-float-op (fn width &rest bit-args)
  "Apply FN to the floats BIT-ARGS denote; the result's bits, or NIL if it is NaN."
  (float-features:with-float-traps-masked t
    (let ((r (apply fn (mapcar (lambda (b) (bits->float b width)) bit-args))))
      (unless (float-features:float-nan-p r)
        (float->bits r width)))))

(defun float-integral-p (x)
  (or (float-features:float-infinity-p x)
      (float-features:float-nan-p x)
      (>= (abs x) (expt 2 (float-digits x)))))

;; Rust: cranelift/codegen/src/ir/immediates.rs, line 689-707
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/immediates.rs#L689-L707
;;
;;     pub fn ceil(self) -> Self {
;;         Self::with_float(Libm::<$float_ty>::ceil(self.$as_float()))
;;     }
;;
;;     /// Returns the largest integer less than or equal to `self`.
;;     pub fn floor(self) -> Self {
;;         Self::with_float(Libm::<$float_ty>::floor(self.$as_float()))
;;     }
;;
;;     /// Returns the integer part of `self`. This means that non-integer numbers are always truncated towards zero.
;;     pub fn trunc(self) -> Self {
;;         Self::with_float(Libm::<$float_ty>::trunc(self.$as_float()))
;;     }
;;
;;     /// Returns the nearest integer to `self`. Rounds half-way cases to the number
;;     /// with an even least significant digit.
;;     pub fn round_ties_even(self) -> Self {
;;         Self::with_float(Libm::<$float_ty>::roundeven(self.$as_float()))
;;     }
(defun float-round (rounder)
  "A function rounding a float to an integral float with ROUNDER, keeping the
sign of zero results like libm's floor/ceil/trunc/roundeven."
  (lambda (x)
    (if (float-integral-p x)
        x
        (float-sign x (abs (float (funcall rounder x) x))))))

;; Rust: cranelift/codegen/src/ir/immediates.rs, line 684-686
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/ir/immediates.rs#L684-L686
;;
;;     pub fn sqrt(self) -> Self {
;;         Self::with_float(Libm::<$float_ty>::sqrt(self.$as_float()))
;;     }
(defun float-sqrt (x)
  (cond ((float-features:float-nan-p x) x)
        ((zerop x) x)
        ((minusp x) float-features:double-float-nan)
        (t (sqrt x))))

;; Rust: cranelift/codegen/src/isle_prelude.rs, line 1027-1061
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L1027-L1061
;;
;;     fn f32_add(&mut self, lhs: Ieee32, rhs: Ieee32) -> Option<Ieee32> {
;;         (lhs + rhs).non_nan()
;;     }
;;
;;     fn f32_sub(&mut self, lhs: Ieee32, rhs: Ieee32) -> Option<Ieee32> {
;;         (lhs - rhs).non_nan()
;;     }
;;
;;     fn f32_mul(&mut self, lhs: Ieee32, rhs: Ieee32) -> Option<Ieee32> {
;;         (lhs * rhs).non_nan()
;;     }
;;
;;     fn f32_div(&mut self, lhs: Ieee32, rhs: Ieee32) -> Option<Ieee32> {
;;         (lhs / rhs).non_nan()
;;     }
;;
;;     fn f32_sqrt(&mut self, n: Ieee32) -> Option<Ieee32> {
;;         n.sqrt().non_nan()
;;     }
;;
;;     fn f32_ceil(&mut self, n: Ieee32) -> Option<Ieee32> {
;;         n.ceil().non_nan()
;;     }
;;
;;     fn f32_floor(&mut self, n: Ieee32) -> Option<Ieee32> {
;;         n.floor().non_nan()
;;     }
;;
;;     fn f32_trunc(&mut self, n: Ieee32) -> Option<Ieee32> {
;;         n.trunc().non_nan()
;;     }
;;
;;     fn f32_nearest(&mut self, n: Ieee32) -> Option<Ieee32> {
;;         n.round_ties_even().non_nan()
;;     }
;;
;; Rust: cranelift/codegen/src/isle_prelude.rs, line 1083-1117
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/isle_prelude.rs#L1083-L1117
(macrolet ((define-host-float-ops (prefix width)
             (flet ((name (op) (intern (format nil "~a-~a" prefix op) '#:cranelift-opts)))
               `(progn
                  (defun ,(name "ADD") (a b) (host-float-op #'+ ,width a b))
                  (defun ,(name "SUB") (a b) (host-float-op #'- ,width a b))
                  (defun ,(name "MUL") (a b) (host-float-op #'* ,width a b))
                  (defun ,(name "DIV") (a b) (host-float-op #'/ ,width a b))
                  (defun ,(name "SQRT") (n) (host-float-op #'float-sqrt ,width n))
                  (defun ,(name "CEIL") (n) (host-float-op (float-round #'ceiling) ,width n))
                  (defun ,(name "FLOOR") (n) (host-float-op (float-round #'floor) ,width n))
                  (defun ,(name "TRUNC") (n) (host-float-op (float-round #'truncate) ,width n))
                  (defun ,(name "NEAREST") (n) (host-float-op (float-round #'round) ,width n))
                  (defun ,(name "FROM-UINT") (n) (float->bits (int->float n ,width) ,width))
                  (defun ,(name "FROM-SINT") (n) (float->bits (int->float n ,width) ,width))))))
  (define-host-float-ops "F32" 32)
  (define-host-float-ops "F64" 64))

;; Rust: cranelift/codegen/src/opts.rs, line 374-388
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L374-L388
;;
;;     fn f32_from_uint(&mut self, n: u64) -> Ieee32 {
;;         Ieee32::with_float(n as f32)
;;     }
;;
;;     fn f64_from_uint(&mut self, n: u64) -> Ieee64 {
;;         Ieee64::with_float(n as f64)
;;     }
;;
;;     fn f32_from_sint(&mut self, n: i64) -> Ieee32 {
;;         Ieee32::with_float(n as f32)
;;     }
;;
;;     fn f64_from_sint(&mut self, n: i64) -> Ieee64 {
;;         Ieee64::with_float(n as f64)
;;     }
(defun int->float (n width)
  "N rounded to nearest-even, as Rust's `n as f32` / `n as f64`."
  (multiple-value-bind (ebits mbits) (float-format width)
    (declare (ignore ebits))
    (let* ((mag (abs n))
           (len (integer-length mag))
           (excess (max 0 (- len (1+ mbits))))
           (q (ash mag (- excess)))
           (rem (ldb (byte excess 0) mag))
           (half (if (plusp excess) (ash 1 (1- excess)) 0)))
      ;; Round the dropped bits to nearest, ties to even.
      (when (and (plusp excess) (or (> rem half) (and (= rem half) (oddp q))))
        (incf q))
      (let ((r (scale-float (coerce q (if (= width 32) 'single-float 'double-float))
                            excess)))
        (if (minusp n) (- r) r)))))

;;; Control flow
;;;
;;; Blocks, block calls, jump tables and trap codes are opaque leaves. The few
;;; rules that need to look inside the function body consult these tables,
;;; which a benchmark binds to describe its CFG.

(defvar *block-call-blocks* (make-hash-table :test 'eql)
  "BlockCall leaf -> Block leaf. A BlockCall missing here is its own Block.")
(defvar *trap-blocks* (make-hash-table :test 'eql)
  "Block leaf -> TrapCode leaf, for blocks that consist only of a trap.")
(defvar *jump-tables* (make-hash-table :test 'eql)
  "JumpTable leaf -> (DEFAULT-BLOCK-CALL . VECTOR-OF-BLOCK-CALLS).")

;; Rust: cranelift/codegen/src/opts.rs, line 279-281
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L279-L281
;;
;;     fn block_call_block(&mut self, block_call: BlockCall) -> Block {
;;         block_call.block(&self.ctx.func.dfg.value_lists)
;;     }
(defun block-call-block (block-call)
  (gethash block-call *block-call-blocks* block-call))

;; Rust: cranelift/codegen/src/opts.rs, line 283-287
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L283-L287
;;
;;     fn just_trap_block(&mut self, block: &Block) -> Option<TrapCode> {
;;         self.ctx
;;             .branch_to_trap_analysis
;;             .analyze_block(self.ctx.func, *block)
;;     }
(defun just-trap-block (block)
  (values (gethash block *trap-blocks*)))

;; Rust: cranelift/codegen/src/opts.rs, line 267-277
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L267-L277
;;
;;     fn resolve_jump_table_entry(&mut self, table: JumpTable, index: u64) -> BlockCall {
;;         let jt_data = &self.ctx.func.dfg.jump_tables[table];
;;         let entries = jt_data.as_slice();
;;         if let Ok(index) = usize::try_from(index)
;;             && index < entries.len()
;;         {
;;             entries[index]
;;         } else {
;;             jt_data.default_block()
;;         }
;;     }
(defun resolve-jump-table-entry (table index)
  (let ((jt (gethash table *jump-tables*)))
    (when jt
      (destructuring-bind (default . entries) jt
        (if (< index (length entries)) (aref entries index) default)))))

;;; Value annotations. Cranelift uses these to steer its own e-graph; as
;;; rewrites they are the identity, so the translator drops them.

;; Rust: cranelift/codegen/src/opts.rs, line 342-347
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L342-L347
;;
;;     fn subsume(&mut self, value: Value) -> Value {
;;         trace!("subsume: {}", value);
;;         self.ctx.subsume_values.insert(value);
;;         self.ctx.stats.subsume += 1;
;;         value
;;     }
(defun subsume (value) value)

;; Rust: cranelift/codegen/src/opts.rs, line 335-340
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L335-L340
;;
;;     fn remat(&mut self, value: Value) -> Value {
;;         trace!("remat: {}", value);
;;         self.ctx.remat_values.insert(value);
;;         self.ctx.stats.remat += 1;
;;         value
;;     }
(defun remat (value) value)

;;; Division by constants (div_const.rs). Results are ISLE enum values,
;;; represented as lists headed by the variant name as a keyword.

;; Rust: cranelift/codegen/src/opts/div_const.rs, line 41-85
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts/div_const.rs#L41-L85
;;
;;     pub fn magic_u32(d: u32) -> MU32 {
;;         debug_assert_ne!(d, 0);
;;         debug_assert_ne!(d, 1); // d==1 generates out of range shifts.
;;
;;         let mut do_add: bool = false;
;;         let mut p: i32 = 31;
;;         let nc: u32 = 0xFFFFFFFFu32 - u32::wrapping_neg(d) % d;
;;         let mut q1: u32 = 0x80000000u32 / nc;
;;         let mut r1: u32 = 0x80000000u32 - q1 * nc;
;;         let mut q2: u32 = 0x7FFFFFFFu32 / d;
;;         let mut r2: u32 = 0x7FFFFFFFu32 - q2 * d;
;;         loop {
;;             p = p + 1;
;;             if r1 >= nc - r1 {
;;                 q1 = u32::wrapping_add(u32::wrapping_mul(2, q1), 1);
;;                 r1 = u32::wrapping_sub(u32::wrapping_mul(2, r1), nc);
;;             } else {
;;                 q1 = u32::wrapping_mul(2, q1);
;;                 r1 = 2 * r1;
;;             }
;;             if r2 + 1 >= d - r2 {
;;                 if q2 >= 0x7FFFFFFFu32 {
;;                     do_add = true;
;;                 }
;;                 q2 = 2 * q2 + 1;
;;                 r2 = u32::wrapping_sub(u32::wrapping_add(u32::wrapping_mul(2, r2), 1), d);
;;             } else {
;;                 if q2 >= 0x80000000u32 {
;;                     do_add = true;
;;                 }
;;                 q2 = u32::wrapping_mul(2, q2);
;;                 r2 = 2 * r2 + 1;
;;             }
;;             let delta: u32 = d - 1 - r2;
;;             if !(p < 64 && (q1 < delta || (q1 == delta && r1 == 0))) {
;;                 break;
;;             }
;;         }
;;
;;         MU32 {
;;             mul_by: q2 + 1,
;;             do_add,
;;             shift_by: p - 32,
;;         }
;;     }
;;
;; Rust: cranelift/codegen/src/opts/div_const.rs, line 87-131
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts/div_const.rs#L87-L131
;;
;;     pub fn magic_u64(d: u64) -> MU64 {
;;         debug_assert_ne!(d, 0);
;;         debug_assert_ne!(d, 1); // d==1 generates out of range shifts.
;;
;;         let mut do_add: bool = false;
;;         let mut p: i32 = 63;
;;         let nc: u64 = 0xFFFFFFFFFFFFFFFFu64 - u64::wrapping_neg(d) % d;
;;         let mut q1: u64 = 0x8000000000000000u64 / nc;
;;         let mut r1: u64 = 0x8000000000000000u64 - q1 * nc;
;;         let mut q2: u64 = 0x7FFFFFFFFFFFFFFFu64 / d;
;;         let mut r2: u64 = 0x7FFFFFFFFFFFFFFFu64 - q2 * d;
;;         loop {
;;             p = p + 1;
;;             if r1 >= nc - r1 {
;;                 q1 = u64::wrapping_add(u64::wrapping_mul(2, q1), 1);
;;                 r1 = u64::wrapping_sub(u64::wrapping_mul(2, r1), nc);
;;             } else {
;;                 q1 = u64::wrapping_mul(2, q1);
;;                 r1 = 2 * r1;
;;             }
;;             if r2 + 1 >= d - r2 {
;;                 if q2 >= 0x7FFFFFFFFFFFFFFFu64 {
;;                     do_add = true;
;;                 }
;;                 q2 = 2 * q2 + 1;
;;                 r2 = u64::wrapping_sub(u64::wrapping_add(u64::wrapping_mul(2, r2), 1), d);
;;             } else {
;;                 if q2 >= 0x8000000000000000u64 {
;;                     do_add = true;
;;                 }
;;                 q2 = u64::wrapping_mul(2, q2);
;;                 r2 = 2 * r2 + 1;
;;             }
;;             let delta: u64 = d - 1 - r2;
;;             if !(p < 128 && (q1 < delta || (q1 == delta && r1 == 0))) {
;;                 break;
;;             }
;;         }
;;
;;         MU64 {
;;             mul_by: q2 + 1,
;;             do_add,
;;             shift_by: p - 64,
;;         }
;;     }
(defun magic-u (d bits)
  (assert (> d 1))
  (let* ((mod (ash 1 bits))
         (top (ash 1 (1- bits)))
         (do-add nil)
         (p (1- bits))
         (nc (- (1- mod) (mod (mod (- d) mod) d)))
         (q1 (floor top nc))
         (r1 (- top (* q1 nc)))
         (q2 (floor (1- top) d))
         (r2 (- (1- top) (* q2 d))))
    (loop
      (incf p)
      (if (>= r1 (- nc r1))
          (setf q1 (mod (1+ (* 2 q1)) mod)
                r1 (mod (- (* 2 r1) nc) mod))
          (setf q1 (mod (* 2 q1) mod)
                r1 (* 2 r1)))
      (if (>= (1+ r2) (- d r2))
          (progn (when (>= q2 (1- top)) (setf do-add t))
                 (setf q2 (1+ (* 2 q2))
                       r2 (mod (- (1+ (* 2 r2)) d) mod)))
          (progn (when (>= q2 top) (setf do-add t))
                 (setf q2 (mod (* 2 q2) mod)
                       r2 (1+ (* 2 r2)))))
      (let ((delta (- d 1 r2)))
        (unless (and (< p (* 2 bits)) (or (< q1 delta) (and (= q1 delta) (zerop r1))))
          (return))))
    (values (1+ q2) do-add (- p bits))))

;; Rust: cranelift/codegen/src/opts/div_const.rs, line 133-174
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts/div_const.rs#L133-L174
;;
;;     pub fn magic_s32(d: i32) -> MS32 {
;;         debug_assert_ne!(d, -1);
;;         debug_assert_ne!(d, 0);
;;         debug_assert_ne!(d, 1);
;;         let two31: u32 = 0x80000000u32;
;;         let mut p: i32 = 31;
;;         let ad: u32 = i32::wrapping_abs(d) as u32;
;;         let t: u32 = two31 + ((d as u32) >> 31);
;;         let anc: u32 = u32::wrapping_sub(t - 1, t % ad);
;;         let mut q1: u32 = two31 / anc;
;;         let mut r1: u32 = two31 - q1 * anc;
;;         let mut q2: u32 = two31 / ad;
;;         let mut r2: u32 = two31 - q2 * ad;
;;         loop {
;;             p = p + 1;
;;             q1 = 2 * q1;
;;             r1 = 2 * r1;
;;             if r1 >= anc {
;;                 q1 = q1 + 1;
;;                 r1 = r1 - anc;
;;             }
;;             q2 = 2 * q2;
;;             r2 = 2 * r2;
;;             if r2 >= ad {
;;                 q2 = q2 + 1;
;;                 r2 = r2 - ad;
;;             }
;;             let delta: u32 = ad - r2;
;;             if !(q1 < delta || (q1 == delta && r1 == 0)) {
;;                 break;
;;             }
;;         }
;;
;;         MS32 {
;;             mul_by: (if d < 0 {
;;                 u32::wrapping_neg(q2 + 1)
;;             } else {
;;                 q2 + 1
;;             }) as i32,
;;             shift_by: p - 32,
;;         }
;;     }
;;
;; Rust: cranelift/codegen/src/opts/div_const.rs, line 176-217
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts/div_const.rs#L176-L217
;;
;;     pub fn magic_s64(d: i64) -> MS64 {
;;         debug_assert_ne!(d, -1);
;;         debug_assert_ne!(d, 0);
;;         debug_assert_ne!(d, 1);
;;         let two63: u64 = 0x8000000000000000u64;
;;         let mut p: i32 = 63;
;;         let ad: u64 = i64::wrapping_abs(d) as u64;
;;         let t: u64 = two63 + ((d as u64) >> 63);
;;         let anc: u64 = u64::wrapping_sub(t - 1, t % ad);
;;         let mut q1: u64 = two63 / anc;
;;         let mut r1: u64 = two63 - q1 * anc;
;;         let mut q2: u64 = two63 / ad;
;;         let mut r2: u64 = two63 - q2 * ad;
;;         loop {
;;             p = p + 1;
;;             q1 = 2 * q1;
;;             r1 = 2 * r1;
;;             if r1 >= anc {
;;                 q1 = q1 + 1;
;;                 r1 = r1 - anc;
;;             }
;;             q2 = 2 * q2;
;;             r2 = 2 * r2;
;;             if r2 >= ad {
;;                 q2 = q2 + 1;
;;                 r2 = r2 - ad;
;;             }
;;             let delta: u64 = ad - r2;
;;             if !(q1 < delta || (q1 == delta && r1 == 0)) {
;;                 break;
;;             }
;;         }
;;
;;         MS64 {
;;             mul_by: (if d < 0 {
;;                 u64::wrapping_neg(q2 + 1)
;;             } else {
;;                 q2 + 1
;;             }) as i64,
;;             shift_by: p - 64,
;;         }
;;     }
(defun magic-s (d bits)
  (assert (not (member d '(-1 0 1))))
  (let* ((mod (ash 1 bits))
         (two (ash 1 (1- bits)))
         (p (1- bits))
         (ad (abs d))
         (tt (+ two (if (minusp d) 1 0)))
         (anc (mod (- (1- tt) (mod tt ad)) mod))
         (q1 (floor two anc))
         (r1 (- two (* q1 anc)))
         (q2 (floor two ad))
         (r2 (- two (* q2 ad))))
    (loop
      (incf p)
      (setf q1 (* 2 q1) r1 (* 2 r1))
      (when (>= r1 anc) (incf q1) (decf r1 anc))
      (setf q2 (* 2 q2) r2 (* 2 r2))
      (when (>= r2 ad) (incf q2) (decf r2 ad))
      (let ((delta (- ad r2)))
        (unless (or (< q1 delta) (and (= q1 delta) (zerop r1)))
          (return))))
    (values (wrap-to (if (minusp d) (- (1+ q2)) (1+ q2)) t bits)
            (- p bits))))

;; Rust: cranelift/codegen/src/opts.rs, line 410-421
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L410-L421
;;
;;     fn div_const_magic_u32(&mut self, d: u32) -> generated_code::DivConstMagicU32 {
;;         let div_const::MU32 {
;;             mul_by,
;;             do_add,
;;             shift_by,
;;         } = div_const::magic_u32(d);
;;         generated_code::DivConstMagicU32::U32 {
;;             mul_by,
;;             do_add,
;;             shift_by: shift_by.try_into().unwrap(),
;;         }
;;     }
(defun div-const-magic-u32 (d)
  (multiple-value-bind (mul-by do-add shift-by) (magic-u d 32)
    (list :u32 mul-by do-add shift-by)))

;; Rust: cranelift/codegen/src/opts.rs, line 423-434
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L423-L434
;;
;;     fn div_const_magic_u64(&mut self, d: u64) -> generated_code::DivConstMagicU64 {
;;         let div_const::MU64 {
;;             mul_by,
;;             do_add,
;;             shift_by,
;;         } = div_const::magic_u64(d);
;;         generated_code::DivConstMagicU64::U64 {
;;             mul_by,
;;             do_add,
;;             shift_by: shift_by.try_into().unwrap(),
;;         }
;;     }
(defun div-const-magic-u64 (d)
  (multiple-value-bind (mul-by do-add shift-by) (magic-u d 64)
    (list :u64 mul-by do-add shift-by)))

;; Rust: cranelift/codegen/src/opts.rs, line 436-442
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L436-L442
;;
;;     fn div_const_magic_s32(&mut self, d: i32) -> generated_code::DivConstMagicS32 {
;;         let div_const::MS32 { mul_by, shift_by } = div_const::magic_s32(d);
;;         generated_code::DivConstMagicS32::S32 {
;;             mul_by,
;;             shift_by: shift_by.try_into().unwrap(),
;;         }
;;     }
(defun div-const-magic-s32 (d)
  (multiple-value-bind (mul-by shift-by) (magic-s d 32)
    (list :s32 mul-by shift-by)))

;; Rust: cranelift/codegen/src/opts.rs, line 444-450
;; https://github.com/bytecodealliance/wasmtime/blob/209d6c4d2963eed81edc839878cddb0a9dc15880/cranelift/codegen/src/opts.rs#L444-L450
;;
;;     fn div_const_magic_s64(&mut self, d: i64) -> generated_code::DivConstMagicS64 {
;;         let div_const::MS64 { mul_by, shift_by } = div_const::magic_s64(d);
;;         generated_code::DivConstMagicS64::S64 {
;;             mul_by,
;;             shift_by: shift_by.try_into().unwrap(),
;;         }
;;     }
(defun div-const-magic-s64 (d)
  (multiple-value-bind (mul-by shift-by) (magic-s d 64)
    (list :s64 mul-by shift-by)))
