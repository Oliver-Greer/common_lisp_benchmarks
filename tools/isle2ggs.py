#!/usr/bin/env python3
"""Translate Cranelift's mid-end ISLE rules (cranelift_ref/opts/*.isle) into
ggs rulesets (cranelift/rules/*.lisp).

The translation is a small partial evaluator over ISLE:

* CLIF opcodes become ordinary terms. Value-producing opcodes keep their result
  type as the first argument, so the term encoding is single-sorted.
* Primitive values (types, immediates, condition codes, ...) are leaves. When a
  rule needs a leaf's value it binds it with `(?v)` and computes in Lisp, in
  `:guard` or `:eval` bodies. Guards never touch engine data structures.
* Internal ISLE extractors are expanded like macros. Internal constructors whose
  arguments and result are primitive become Lisp functions (support.lisp);
  other internal constructors are inlined: `multi` ones produce one ggs rule per
  alternative, deterministic ones a first-match-wins `block` inside one rule.
* `x @ (value_type ty)` uses the CLIF-TYPE analysis (support.lisp).
* `subsume` and `remat` are dropped: they steer Cranelift's e-graph, they are
  not rewrites. Rule priorities are dropped for the same reason.

Usage: python3 tools/isle2ggs.py   (from the repository root)
"""

import itertools
import sys
from dataclasses import dataclass, field, replace
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from isle_parse import Int, Program  # noqa: E402

ROOT = Path(__file__).resolve().parent.parent
PRELUDES = ["numerics", "prelude", "prelude_opt", "clif_opt"]
OPT_FILES = ["arithmetic", "bitops", "cprop", "extends", "icmp", "remat",
             "selects", "shifts", "skeleton", "spaceship", "spectre", "vector"]

STRUCTURAL_TYPES = {"Value", "Inst", "SkeletonInstSimplification"}

INTCC = {
    "Equal": "eq", "NotEqual": "ne",
    "SignedLessThan": "slt", "SignedGreaterThanOrEqual": "sge",
    "SignedGreaterThan": "sgt", "SignedLessThanOrEqual": "sle",
    "UnsignedLessThan": "ult", "UnsignedGreaterThanOrEqual": "uge",
    "UnsignedGreaterThan": "ugt", "UnsignedLessThanOrEqual": "ule",
}
FLOATCC = {
    "Ordered": "ord", "Unordered": "uno", "Equal": "eq", "NotEqual": "ne",
    "OrderedNotEqual": "one", "UnorderedOrEqual": "ueq",
    "LessThan": "lt", "LessThanOrEqual": "le",
    "GreaterThan": "gt", "GreaterThanOrEqual": "ge",
    "UnorderedOrLessThan": "ult", "UnorderedOrLessThanOrEqual": "ule",
    "UnorderedOrGreaterThan": "ugt", "UnorderedOrGreaterThanOrEqual": "uge",
}

# Constant-producing opcodes recognized by `all_zero` (see `all_zero_etor`).
ZERO_CONST_OPS = ["iconst", "f16const", "f32const", "f64const", "f128const", "vconst"]

# Extern extractors that are the identity under our leaf representation.
IDENTITY_ETORS = {"u64_from_imm64", "u16_from_ieee16", "u32_from_ieee32",
                  "u64_from_ieee64", "ieee128_constant"}

# Type extractors that only match vector types.
VECTOR_ETORS = {"ty_vector", "ty_vec128", "ty_vec128_int", "multi_lane"}

# Skeleton instructions that produce a value of their first operand's type.
VALUE_SKELETON_OPS = {"udiv", "sdiv", "urem", "srem", "uadd_overflow_trap"}

# Value annotations that are the identity as rewrites.
DROPPED_CTORS = {"subsume", "remat"}


class Unsupported(Exception):
    pass


# ---------------------------------------------------------------------------
# Lisp code as nested Python lists: str = symbol, int = number,
# ("quote", x) = 'x.

def lisp(x):
    if isinstance(x, tuple) and x[0] == "quote":
        return "'" + lisp(x[1])
    if isinstance(x, list):
        return "(" + " ".join(lisp(y) for y in x) + ")"
    if isinstance(x, bool):
        return "t" if x else "nil"
    if x is None:
        return "nil"
    if isinstance(x, int):
        return str(int(x))
    return str(x)


def kebab(name):
    return name.replace("_", "-").lower()


def lisp_var(isle_var):
    return "?" + isle_var.lower()


def is_lisp_var(x):
    return isinstance(x, str) and x.startswith("?")


def is_literal(x):
    return isinstance(x, int) or (isinstance(x, tuple) and x[0] == "quote") \
        or x in ("t", "nil")


def symbols_in(x, acc):
    if isinstance(x, list):
        for y in x:
            symbols_in(y, acc)
    elif is_lisp_var(x):
        acc.add(x)
    return acc


# Pattern / template ASTs for ggs:
#   ("var", "?x")     pattern variable            ?x
#   ("leaf", "?x")    leaf bound to Lisp var      (?x)
#   ("lit", atom)     literal leaf                atom
#   ("node", head, [args])                        (head args...)   head may be ?var

def show(ast):
    kind = ast[0]
    if kind == "var":
        return ast[1]
    if kind == "leaf":
        return "(" + ast[1] + ")"
    if kind == "lit":
        return lisp(ast[1])
    head, args = ast[1], ast[2]
    if not args:
        return "(" + head + ")"
    return "(" + " ".join([head] + [show(a) for a in args]) + ")"


def subst_ast(ast, old, new):
    if ast == old:
        return new
    if ast[0] == "node":
        return ("node", ast[1], [subst_ast(a, old, new) for a in ast[2]])
    return ast


def ast_vars(ast, acc):
    if ast[0] in ("var", "leaf"):
        acc.add(ast[1])
    elif ast[0] == "node":
        if is_lisp_var(ast[1]):
            acc.add(ast[1])
        for a in ast[2]:
            ast_vars(a, acc)
    return acc


# ---------------------------------------------------------------------------
# Static knowledge of CLIF types, used only to fold guards on literal types.

def parse_type(sym):
    name = sym.lower()
    lane, _, count = name.partition("x")
    try:
        bits = int(lane[1:])
    except ValueError:
        return None
    if lane[0] not in "if":
        return None
    return lane[0], bits, int(count) if count else 1


def fold_type_etor(name, ty):
    """Result of type extractor NAME on literal type TY: a type, or None (no
    match); raise KeyError if NAME is not foldable."""
    info = parse_type(ty)
    if info is None:
        raise KeyError(name)
    kind, bits, count = info
    total = bits * count
    vec = count > 1
    ok = {
        "fits_in_16": total <= 16,
        "fits_in_64": total <= 64,
        "ty_int": kind == "i" and not vec,
        "ty_vector": vec,
        "ty_vec128": vec and total == 128,
        "ty_vec128_int": vec and total == 128 and kind == "i",
        "ty_int_vec128": (kind == "i" and not vec) or (vec and total == 128 and kind == "i"),
        "ty_int_ref_scalar_64_extract": total <= 64 and kind == "i" and not vec,
    }[name]
    return ty if ok else None


def all_types():
    scalars = ["i8", "i16", "i32", "i64", "i128", "f16", "f32", "f64", "f128"]
    vectors = [f"{lane}x{n}" for lane in ["i8", "i16", "i32", "i64", "f16", "f32", "f64"]
               for n in [2, 4, 8, 16, 32, 64, 128, 256]
               if int(lane[1:]) * n <= 2048]
    return frozenset(scalars + vectors)


UNIVERSE = all_types()

# Type extractors the static domain understands (see fold_type_etor).
FOLDABLE_TYPE_ETORS = {"fits_in_16", "fits_in_64", "ty_int", "ty_vector", "ty_vec128",
                       "ty_vec128_int", "ty_int_vec128", "ty_int_ref_scalar_64_extract"}


def lane_of(ty):
    kind, bits, _ = parse_type(ty)
    return f"{kind}{bits}"


# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class Bind:
    kind: str          # "term" (AST) or "val" (Lisp expr)
    value: object
    ty: str
    plain: bool = False  # val bound as a plain pattern variable (no Lisp use)


@dataclass
class State:
    env: dict
    steps: tuple = ()
    lhs: object = None       # root pattern AST, refined by multi helpers
    det: int = 0             # >0 while matching a deterministic helper's LHS
    tys: dict = field(default_factory=dict)  # Lisp var -> possible CLIF types

    def fork(self, **kw):
        return replace(self, env=dict(kw.pop("env", self.env)),
                       tys=dict(kw.pop("tys", self.tys)), **kw)

    def add(self, *steps):
        return self.fork(steps=self.steps + tuple(steps))


class Translator:
    def __init__(self, program):
        self.p = program
        self.clif_ops = {
            n for n, d in program.decls.items()
            if d.source == "clif_opt.isle" and d.ret in ("Value", "Inst")
            and d.etor is not None
        }
        self.fresh_counter = itertools.count()
        self.choice_counter = itertools.count()
        self.plain = set()  # ISLE var names compiled as plain pattern vars
        self.plain_fresh = set()  # fresh leaf vars compiled as plain pattern vars
        self.root_op = None
        self.helpers_used = set()
        self.depth = 0

    # -- naming ------------------------------------------------------------

    def fresh(self, base="g"):
        return f"?{base}{next(self.fresh_counter)}"

    def decl(self, name):
        d = self.p.decls.get(name)
        if d is None:
            raise Unsupported(f"unknown term {name}")
        return d

    def is_structural(self, ty):
        return ty in STRUCTURAL_TYPES

    def enum_symbol(self, name):
        """Lisp value of a 0-field enum variant like IntCC.Equal."""
        tyname, _, variant = name.partition(".")
        if tyname == "IntCC":
            return INTCC[variant]
        if tyname == "FloatCC":
            return FLOATCC[variant]
        if tyname == "Opcode":
            return "".join("_" + c.lower() if c.isupper() else c for c in variant).lstrip("_")
        return ":" + kebab(variant)

    def const_symbol(self, tok):
        """`$I32` -> i32."""
        name = tok[1:]
        if parse_type(name) is None:
            raise Unsupported(f"unknown constant {tok}")
        return name.lower()

    # -- primitive leaves and Lisp values ----------------------------------

    def ast_value(self, ast):
        """Lisp expression for a leaf AST in a primitive slot."""
        if ast[0] == "lit":
            v = ast[1]
            return v if isinstance(v, int) else ("quote", v)
        if ast[0] == "leaf":
            return ast[1]
        raise Unsupported(f"no Lisp value for {show(ast)}")

    def to_leaf(self, code, st):
        """Template AST for a Lisp value."""
        if isinstance(code, int):
            if code < 0:
                raise Unsupported(f"negative leaf {code}")
            return ("lit", code), st
        if isinstance(code, tuple) and code[0] == "quote":
            return ("lit", code[1]), st
        if is_lisp_var(code):
            return ("leaf", code), st
        if isinstance(code, tuple) and code[0] == "plain":
            return ("var", code[1]), st
        v = self.fresh("v")
        return ("leaf", v), st.add(("let", v, code))

    def type_of(self, ast, st):
        """Lisp expression for the CLIF type of the value AST denotes."""
        if ast[0] == "var":
            v = self.fresh("ty")
            return v, st.add(("let", v, ["clif-type", ast[1]]), ("test", v))
        if ast[0] == "node":
            head, args = ast[1], ast[2]
            if is_lisp_var(head):  # all_zero's ?op: always a typed constant
                return self.leaf_value(args[0], st)
            d = self.decl(head)
            if d.args and d.args[0] == "Type":
                return self.leaf_value(args[0], st)
            if head in VALUE_SKELETON_OPS:
                return self.type_of(args[0], st)
        raise Unsupported(f"type of {show(ast)}")

    def leaf_value(self, ast, st):
        if ast[0] == "var":
            raise Unsupported("type leaf compiled as plain var")  # retried w/o plain
        return self.ast_value(ast), st

    # -- static folding ----------------------------------------------------

    def possible(self, expr, st):
        """Set of CLIF types the type-valued Lisp EXPR may evaluate to."""
        if isinstance(expr, tuple) and expr[0] == "quote" and expr[1] in UNIVERSE:
            return frozenset([expr[1]])
        if is_lisp_var(expr):
            return st.tys.get(expr, UNIVERSE)
        if isinstance(expr, list) and expr[0] == "lane-type":
            return frozenset(lane_of(t) for t in self.possible(expr[1], st))
        return UNIVERSE

    def narrow(self, st, expr, types):
        """ST with type-valued EXPR known to be in TYPES (None if impossible)."""
        if not types:
            return None
        if is_lisp_var(expr):
            st = st.fork()
            st.tys[expr] = frozenset(types)
        return st

    def test(self, st, expr):
        """Add test EXPR, folding it when its value is known."""
        if expr in ("t",) or (isinstance(expr, int)):
            return st
        if expr == "nil":
            return None
        if isinstance(expr, list) and expr[0] in ("eql", "eq") and \
                is_literal(expr[1]) and is_literal(expr[2]):
            return st if expr[1] == expr[2] else None
        if isinstance(expr, list) and expr[0] == "not" and is_literal(expr[1]):
            return None if expr[1] != "nil" else st
        return st.add(("test", expr))

    # -- patterns in ggs slots ---------------------------------------------

    def match_slot(self, pat, ty, st, name=None):
        """Compile PAT, expected to have ISLE type TY, into a ggs pattern.
        Yields (ast, state)."""
        if self.is_structural(ty):
            yield from self.match_term_slot(pat, ty, st, name)
        else:
            yield from self.match_prim_slot(pat, ty, st)

    def match_prim_slot(self, pat, ty, st):
        if isinstance(pat, Int):
            if pat < 0:
                raise Unsupported(f"negative literal {pat} in {ty} slot")
            yield ("lit", int(pat)), st
            return
        if isinstance(pat, str):
            if pat.startswith("$"):
                yield ("lit", self.const_symbol(pat)), st
                return
            if pat == "_":
                if ty == "Type":
                    v = self.fresh("t")
                    yield (("var", v) if v in self.plain_fresh else ("leaf", v)), st
                else:
                    yield ("var", self.fresh("_")), st
                return
            if pat in ("true", "false"):
                raise Unsupported("bool literal in slot")
            b = st.env.get(pat)
            if b is None:
                v = lisp_var(pat)
                st = st.fork()
                if pat in self.plain:
                    st.env[pat] = Bind("val", ("plain", v), ty, plain=True)
                    yield ("var", v), st
                else:
                    st.env[pat] = Bind("val", v, ty)
                    yield ("leaf", v), st
                return
            if b.kind != "val":
                raise Unsupported(f"{pat} used at two types")
            if b.plain:
                self.fresh(pat.lower())  # keep fresh names aligned with the first pass
                yield ("var", b.value[1]), st
                return
            if is_literal(b.value):
                yield self.to_leaf(b.value, st)[0], st
                return
            v = self.fresh(pat.lower())
            yield ("leaf", v), st.add(("test", ["eql", v, b.value], "nonlinear"))
            return
        if isinstance(pat, list) and pat and isinstance(pat[0], str):
            head = pat[0]
            if head in self.p.enum_variants and len(pat) == 1:
                yield ("lit", self.enum_symbol(head)), st
                return
            if head in IDENTITY_ETORS:
                d = self.decl(head)
                yield from self.match_prim_slot(pat[1], d.args[0], st)
                return
        # General case: bind the leaf and match its value in Lisp.
        v = self.fresh("k")
        for st2 in self.match_val(pat, ty, v, st):
            yield ("leaf", v), st2

    def match_term_slot(self, pat, ty, st, name=None):
        if isinstance(pat, Int) or pat in ("true", "false"):
            raise Unsupported(f"literal {pat} in a {ty} position")
        if isinstance(pat, str):
            if pat == "_":
                yield ("var", self.fresh("_")), st
                return
            b = st.env.get(pat)
            if b is None:
                v = lisp_var(pat)
                st = st.fork()
                st.env[pat] = Bind("term", ("var", v), ty)
                yield ("var", v), st
                return
            if b.kind != "term":
                raise Unsupported(f"{pat} used at two types")
            yield b.value, st
            return
        if not isinstance(pat, list):
            raise Unsupported(f"bad term pattern {pat!r}")
        head = pat[0]
        if head == "@":
            x, sub = pat[1], pat[2]
            if x in st.env:
                raise Unsupported(f"rebinding {x}")
            if isinstance(sub, list) and sub[0] == "value_type":
                gen = self.match_term_slot(sub, ty, st, name=x)
            else:
                gen = self.match_term_slot(sub, ty, st)
            for ast, st2 in gen:
                st2 = st2.fork()
                st2.env[x] = Bind("term", ast, ty)
                yield ast, st2
            return
        if head == "value_type":
            if name is not None and name not in st.env:
                v = lisp_var(name)
            else:
                v = self.fresh("val")
            ast = ("var", v)
            tyexpr, st2 = self.type_of(ast, st)
            for st3 in self.match_val(pat[1], "Type", tyexpr, st2):
                yield ast, st3
            return
        if head in self.clif_ops:
            d = self.decl(head)
            if len(pat) - 1 != len(d.args):
                raise Unsupported(f"arity of {head}")
            yield from self.match_node(head, pat[1:], d.args, st)
            return
        if head == "iconst_s":
            yield from self.match_iconst_s(pat[1], pat[2], st)
            return
        if head == "all_zero":
            yield from self.match_all_zero(pat[1], st)
            return
        if head in ("uextend_maybe", "sextend_maybe"):
            op = head.split("_")[0]
            # Either an explicit extend ...
            yield from self.match_node(op, [pat[1], pat[2]], ["Type", "Value"], st)
            # ... or the value itself when it already has the type.
            for ast, st2 in self.match_term_slot(pat[2], "Value", st):
                tyexpr, st3 = self.type_of(ast, st2)
                for st4 in self.match_val(pat[1], "Type", tyexpr, st3):
                    yield ast, st4
            return
        d = self.decl(head)
        if d.etor is not None and not d.extern_etor:
            yield from self.match_term_slot(self.expand_etor(d, pat[1:]), ty, st, name)
            return
        raise Unsupported(f"term pattern {head}")

    def match_node(self, head, arg_pats, arg_tys, st):
        def go(i, st, acc):
            if i == len(arg_pats):
                yield ("node", head, acc), st
                return
            for ast, st2 in self.match_slot(arg_pats[i], arg_tys[i], st):
                yield from go(i + 1, st2, acc + [ast])
        yield from go(0, st, [])

    def match_iconst_s(self, ty_pat, c_pat, st):
        for ty_ast, st2 in self.match_slot(ty_pat, "Type", st):
            k = self.fresh("imm")
            tyexpr, st3 = self.leaf_value(ty_ast, st2)
            ast = ("node", "iconst", [ty_ast, ("leaf", k)])
            for st4 in self.match_val(c_pat, "i64", ["i64-sextend-imm64", tyexpr, k], st3):
                yield ast, st4

    def match_all_zero(self, ty_pat, st):
        for ty_ast, st2 in self.match_slot(ty_pat, "Type", st):
            op = self.fresh("op")
            st3 = st2.add(("test", ["member", op, ("quote", ZERO_CONST_OPS)]))
            yield ("node", op, [ty_ast, ("lit", 0)]), st3

    def expand_etor(self, d, args):
        params, body = d.etor
        if len(params) != len(args):
            raise Unsupported(f"arity of extractor {d.name}")
        sub = dict(zip(params, args))

        def walk(x):
            if isinstance(x, list):
                return [walk(y) for y in x]
            if isinstance(x, str) and x in sub:
                return sub[x]
            return x
        return walk(body)

    # -- patterns against Lisp values --------------------------------------

    def bind_val(self, name, ty, expr, st):
        st = st.fork()
        if is_lisp_var(expr) or is_literal(expr):
            st.env[name] = Bind("val", expr, ty)
            return st
        v = lisp_var(name)
        st.env[name] = Bind("val", v, ty)
        if ty == "Type":
            st = self.narrow(st, v, self.possible(expr, st))
        return st.add(("let", v, expr))

    def match_val(self, pat, ty, expr, st):
        """Match PAT against Lisp expression EXPR of ISLE type TY. Yields states."""
        if isinstance(pat, Int):
            st2 = self.test(st, ["eql", expr, int(pat)])
            if st2 is not None:
                yield st2
            return
        if isinstance(pat, str):
            if pat == "_":
                yield st
                return
            if pat == "true":
                st2 = self.test(st, expr)
                if st2 is not None:
                    yield st2
                return
            if pat == "false":
                st2 = self.test(st, ["not", expr])
                if st2 is not None:
                    yield st2
                return
            if pat.startswith("$"):
                sym = self.const_symbol(pat)
                dom = self.possible(expr, st)
                if sym not in dom:
                    return
                if dom != {sym}:
                    st = self.narrow(st.add(("test", ["eq", expr, ("quote", sym)])), expr, {sym})
                yield st
                return
            b = st.env.get(pat)
            if b is None:
                yield self.bind_val(pat, ty, expr, st)
                return
            if b.kind != "val" or b.plain:
                raise Unsupported(f"{pat}: value comparison needs a Lisp value")
            st2 = self.test(st, ["equal" if ty.startswith("DivConst") else "eql", expr, b.value])
            if st2 is not None:
                yield st2
            return
        head = pat[0]
        if head == "@":
            for st2 in self.match_val(pat[1], ty, expr, st):
                yield from self.match_val(pat[2], ty, self.env_val(pat[1], st2), st2)
            return
        if head == "and":
            if not (is_lisp_var(expr) or is_literal(expr)):
                v = self.fresh("a")
                st = st.add(("let", v, expr))
                expr = v

            def go(i, st):
                if i == len(pat):
                    yield st
                    return
                for st2 in self.match_val(pat[i], ty, expr, st):
                    yield from go(i + 1, st2)
            yield from go(1, st)
            return
        if head in self.p.enum_variants:
            enum_ty, fields = self.p.enum_variants[head]
            if not fields:
                st2 = self.test(st, ["eq", expr, ("quote", self.enum_symbol(head))])
                if st2 is not None:
                    yield st2
                return
            if not is_lisp_var(expr):
                v = self.fresh("e")
                st = st.add(("let", v, expr))
                expr = v
            st = st.add(("test", ["eq", ["car", expr], self.enum_symbol(head)]))

            def go(i, st):
                if i == len(fields):
                    yield st
                    return
                for st2 in self.match_val(pat[i + 1], fields[i], ["nth", i + 1, expr], st):
                    yield from go(i + 1, st2)
            yield from go(0, st)
            return
        if head in IDENTITY_ETORS:
            d = self.decl(head)
            yield from self.match_val(pat[1], d.args[0], expr, st)
            return
        d = self.decl(head)
        if d.etor is not None and not d.extern_etor:
            yield from self.match_val(self.expand_etor(d, pat[1:]), ty, expr, st)
            return
        if not d.extern_etor:
            raise Unsupported(f"value pattern {head}")
        # Extern extractor: (match-NAME expr) returns the outputs as values.
        outs = d.args
        if len(pat) - 1 != len(outs):
            raise Unsupported(f"arity of extractor {head}")
        call = ["match-" + kebab(head), expr]
        if head in FOLDABLE_TYPE_ETORS or head == "multi_lane":
            dom = self.possible(expr, st)
            if head == "multi_lane":
                ok = {t for t in dom if parse_type(t)[2] > 1}
            else:
                ok = {t for t in dom if fold_type_etor(head, t) is not None}
            if not ok:
                return
            if head != "multi_lane":
                if ok == dom:  # always matches; the output is the input
                    yield from self.match_val(pat[1], outs[0], expr, st)
                    return
                v = self.fresh("x")
                st = st.add(("let", v, call), ("test", v))
                st = self.narrow(self.narrow(st, v, ok), expr, ok)
                yield from self.match_val(pat[1], outs[0], v, st)
                return
            st = self.narrow(st, expr, ok)
        infallible = "_matches_" in head
        if len(outs) == 1:
            v = self.fresh("x")
            st = st.add(("let", v, call))
            if not infallible:
                st = st.add(("test", v))
            yield from self.match_val(pat[1], outs[0], v, st)
            return
        vs = [self.fresh("x") for _ in outs]
        st = st.add(("mvb", vs, call))

        def go(i, st):
            if i == len(outs):
                yield st
                return
            for st2 in self.match_val(pat[i + 1], outs[i], vs[i], st):
                yield from go(i + 1, st2)
        yield from go(0, st)

    def env_val(self, name, st):
        b = st.env[name]
        if b.kind != "val" or b.plain:
            raise Unsupported(f"{name}: needs a Lisp value")
        return b.value

    # -- patterns against templates (inlining, if-let results) -------------

    def match_ast(self, pat, ty, ast, st):
        """Match PAT against the term AST denotes. Yields states."""
        if not self.is_structural(ty):
            for code, st2 in [(self.ast_value(ast), st)] if ast[0] != "var" else \
                    [(("plain", ast[1]), st)]:
                if isinstance(code, tuple) and code[0] == "plain":
                    if isinstance(pat, str) and pat not in st2.env and pat != "_":
                        st3 = st2.fork()
                        st3.env[pat] = Bind("val", code, ty, plain=True)
                        yield st3
                        return
                    if pat == "_":
                        yield st2
                        return
                    raise Unsupported("matching a plain leaf")
                yield from self.match_val(pat, ty, code, st2)
            return
        if isinstance(pat, Int) or pat in ("true", "false"):
            raise Unsupported(f"literal {pat} in a {ty} position")
        if isinstance(pat, str):
            if pat == "_":
                yield st
                return
            b = st.env.get(pat)
            if b is None:
                st = st.fork()
                st.env[pat] = Bind("term", ast, ty)
                yield st
                return
            if b.value == ast:
                yield st
                return
            raise Unsupported(f"non-linear helper pattern on {pat}")
        head = pat[0]
        if head == "@":
            for st2 in self.match_ast(pat[1], ty, ast, st):
                yield from self.match_ast(pat[2], ty, ast, st2)
            return
        if head == "value_type":
            tyexpr, st2 = self.type_of(ast, st)
            yield from self.match_val(pat[1], "Type", tyexpr, st2)
            return
        if ast[0] == "var" and st.lhs is not None and ast_vars(st.lhs, set()) >= {ast[1]}:
            yield from self.refine(pat, ty, ast, st)
            return
        if ast[0] == "node" and head in self.clif_ops:
            if ast[1] != head:
                return
            d = self.decl(head)

            def go(i, st):
                if i == len(d.args):
                    yield st
                    return
                for st2 in self.match_ast(pat[i + 1], d.args[i], ast[2][i], st):
                    yield from go(i + 1, st2)
            yield from go(0, st)
            return
        d = self.decl(head)
        if d.etor is not None and not d.extern_etor:
            yield from self.match_ast(self.expand_etor(d, pat[1:]), ty, ast, st)
            return
        raise Unsupported(f"helper pattern {head} on {show(ast)}")

    def refine(self, pat, ty, var_ast, st):
        """Constrain LHS variable VAR_AST to match PAT by substituting it."""
        if st.det:
            raise Unsupported("structural match inside a deterministic helper")
        used = set()
        for step in st.steps:
            symbols_in(list(step[1:]), used)
        if var_ast[1] in used:
            raise Unsupported(f"refining {var_ast[1]} after Lisp use")
        for new, st2 in self.match_term_slot(pat, ty, st):
            st3 = st2.fork(lhs=subst_ast(st2.lhs, var_ast, new))
            st3.env = {k: (replace(b, value=subst_ast(b.value, var_ast, new))
                           if b.kind == "term" else b)
                       for k, b in st3.env.items()}
            yield st3

    # -- expressions -------------------------------------------------------

    def expr_type(self, e, st, local=None):
        local = local or {}
        if isinstance(e, str) and e in local:
            return local[e]
        if isinstance(e, str) and e in st.env:
            return st.env[e].ty
        if isinstance(e, list) and e and e[0] == "let":
            local = dict(local)
            for name, ty, _ in e[1]:
                local[name] = ty
            return self.expr_type(e[2], st, local)
        if isinstance(e, list) and isinstance(e[0], str) and e[0] in self.p.decls:
            return self.p.decls[e[0]].ret
        return None

    def compile_expr(self, e, ty, st):
        """Compile expression E of ISLE type TY. Yields (result, state): a
        template AST for structural types, else a Lisp expression."""
        if isinstance(e, Int):
            yield int(e), st
            return
        if isinstance(e, str):
            if e == "true":
                yield "t", st
            elif e == "false":
                yield "nil", st
            elif e.startswith("$"):
                yield ("quote", self.const_symbol(e)), st
            else:
                b = st.env.get(e)
                if b is None:
                    raise Unsupported(f"unbound {e}")
                yield b.value, st
            return
        head = e[0]
        if head == "let":
            yield from self.compile_let(e[1], e[2], ty, st)
            return
        if head in self.p.enum_variants:
            _, fields = self.p.enum_variants[head]
            if not fields:
                sym = self.enum_symbol(head)
                yield (("lit", sym) if self.is_structural(ty) else ("quote", sym)), st
                return
            for args, st2 in self.compile_args(e[1:], fields, st):
                yield ["list", self.enum_symbol(head)] + args, st2
            return
        if head in DROPPED_CTORS:
            yield from self.compile_expr(e[1], ty, st)
            return
        if head == "replace_branch_cond":
            root = st.lhs
            if root[0] != "node" or root[1] not in ("brif", "trapz", "trapnz"):
                raise Unsupported("replace_branch_cond outside a branch")
            for cond, st2 in self.compile_expr(e[1], "Value", st):
                yield ("node", root[1], [cond] + root[2][1:]), st2
            return
        if head == "remove_inst":
            yield ("lit", "remove_inst"), st
            return
        if head == "replace_with_two":
            for args, st2 in self.compile_args(e[1:], ["Inst", "Inst"], st, allow_plain=True):
                yield ("node", "replace_with_two", args), st2
            return
        d = self.decl(head)
        self.check_type(d.ret, ty, head)
        if head in self.clif_ops:
            for args, st2 in self.compile_args(e[1:], d.args, st, allow_plain=True):
                leaves = []
                for a, t in zip(args, d.args):
                    if self.is_structural(t):
                        leaves.append(a)
                    else:
                        leaf, st2 = self.to_leaf(a, st2)
                        leaves.append(leaf)
                yield ("node", head, leaves), st2
            return
        if d.extern_ctor:
            for args, st2 in self.compile_args(e[1:], d.args, st):
                yield from self.call(kebab(head), args, d, st2)
            return
        if d.rules:
            if self.is_primitive_helper(d):
                self.helpers_used.add(head)
                for args, st2 in self.compile_args(e[1:], d.args, st):
                    yield from self.call(kebab(head), args, d, st2)
                return
            yield from self.inline_ctor(d, e[1:], st)
            return
        raise Unsupported(f"constructor {head}")

    def check_type(self, actual, expected, what):
        """ISLE type checking: ACTUAL must be EXPECTED or convert to it."""
        if expected is None or actual == expected:
            return
        if self.is_structural(actual) and self.is_structural(expected):
            return  # Inst/Value -> SkeletonInstSimplification conversions
        if (actual, expected) in self.p.converts:
            return
        raise Unsupported(f"{what} has type {actual}, expected {expected}")

    def call(self, fn, args, d, st):
        code = [fn] + args
        if "partial" not in d.flags:
            yield code, st
            return
        if d.ret == "bool":
            r, ok = self.fresh("r"), self.fresh("ok")
            yield r, st.add(("mvb", [r, ok], code, ok))
            return
        v = self.fresh("r")
        yield v, st.add(("let", v, code), ("test", v))

    def compile_args(self, exprs, tys, st, allow_plain=False):
        if len(exprs) != len(tys):
            raise Unsupported("arity mismatch")

        def go(i, st, acc):
            if i == len(exprs):
                yield acc, st
                return
            for r, st2 in self.compile_expr(exprs[i], tys[i], st):
                if not allow_plain and isinstance(r, tuple) and r[0] == "plain":
                    raise Unsupported("plain var used as Lisp value")
                yield from go(i + 1, st2, acc + [r])
        yield from go(0, st, [])

    def compile_let(self, bindings, body, ty, st):
        if not bindings:
            yield from self.compile_expr(body, ty, st)
            return
        (name, vty, e), rest = bindings[0], bindings[1:]
        for r, st2 in self.compile_expr(e, vty, st):
            st2 = st2.fork()
            if self.is_structural(vty):
                st2.env[name] = Bind("term", r, vty)
            else:
                st2 = self.bind_val(name, vty, r, st2)
            yield from self.compile_let(rest, body, ty, st2)

    def is_primitive_helper(self, d):
        return not d.extern_ctor and not self.is_structural(d.ret) and \
            not any(self.is_structural(a) for a in d.args)

    def inline_ctor(self, d, arg_exprs, st):
        """Inline internal constructor D applied to ARG_EXPRS."""
        self.depth += 1
        try:
            if self.depth > 12:
                raise Unsupported(f"unbounded inlining of {d.name}")
            yield from self.inline_ctor_1(d, arg_exprs, st)
        finally:
            self.depth -= 1

    def inline_ctor_1(self, d, arg_exprs, st):
        for args, st2 in self.compile_args(arg_exprs, d.args, st, allow_plain=True):
            rules = sorted(d.rules, key=lambda r: -r.prio)
            if "multi" in d.flags:
                for r in rules:
                    yield from self.inline_rule(d, r, args, st2, None)
            else:
                cid = f"c{next(self.choice_counter)}"
                for k, r in enumerate(rules):
                    yield from self.inline_rule(d, r, args, st2.add(("alt", cid, k)), cid)

    def inline_rule(self, d, r, args, st, cid):
        r = wildcard_unused(r)
        outer_env = st.env
        hst = st.fork(env={}, det=st.det + (1 if cid else 0))

        def chain(i, hst):
            if i == len(args):
                yield hst
                return
            for h2 in match_args_one(i, hst):
                yield from chain(i + 1, h2)

        def match_args_one(i, hst):
            pat = r.lhs[i + 1]
            ty = d.args[i]
            a = args[i]
            if self.is_structural(ty):
                yield from self.match_ast(pat, ty, a, hst)
            elif isinstance(a, tuple) and a[0] == "plain":
                yield from self.match_ast(pat, ty, ("var", a[1]), hst)
            else:
                yield from self.match_val(pat, ty, a, hst)

        for h2 in chain(0, hst):
            for h3 in self.clauses(r.clauses, h2):
                if cid:
                    h3 = h3.add(("commit", cid)).fork(det=h3.det - 1)
                for res, h4 in self.compile_expr(r.rhs, d.ret, h3):
                    # Back to the caller's environment, keeping new steps/LHS.
                    yield res, h4.fork(env=outer_env)

    def clauses(self, clauses, st):
        if not clauses:
            yield st
            return
        (kind, pat, e), rest = clauses[0], clauses[1:]
        ty = self.expr_type(e, st)
        if ty is None:
            raise Unsupported(f"cannot infer the type of {e!r}")
        for r, st2 in self.compile_expr(e, ty, st):
            if kind == "if":
                gen = [st2]
            elif self.is_structural(ty):
                gen = self.match_ast(pat, ty, r, st2)
            else:
                gen = self.match_val(pat, ty, r, st2)
            for st3 in gen:
                yield from self.clauses(rest, st3)

    # -- whole rules -------------------------------------------------------

    def compile_rule(self, rule):
        """List of (lhs_ast, steps, template) alternatives for a simplify rule."""
        head = rule.lhs[0]
        if head not in ("simplify", "simplify_skeleton"):
            return None
        ty = "Value" if head == "simplify" else "Inst"
        out = []
        for ast, st in self.match_slot(rule.lhs[1], ty, State(env={})):
            st = st.fork(lhs=ast)
            self.root_op = ast[1] if ast[0] == "node" else None
            for st2 in self.clauses(rule.clauses, st):
                for tmpl, st3 in self.compile_expr(rule.rhs, ty, st2):
                    if isinstance(tmpl, (str, list, int)) and not isinstance(tmpl, tuple):
                        raise Unsupported("rule result is not a term")
                    out.append((st3.lhs, st3.steps, tmpl))
        return out

    def translate_rule(self, rule):
        """Compile RULE twice: first to find primitive variables that never need
        a Lisp value, then again with those as plain pattern variables."""
        rule = wildcard_unused(rule)
        self.plain = set()
        self.plain_fresh = set()
        self.fresh_counter = itertools.count()
        self.choice_counter = itertools.count()
        alts = self.compile_rule(rule)
        if alts is None:
            return None
        used = set()
        for _, steps, tmpl in alts:
            for s in steps:
                if s[-1] != "nonlinear":
                    symbols_in(list(s[1:]), used)
        candidates = set()
        for lhs, _, _ in alts:
            leaves = set()
            collect_leaves(lhs, leaves)
            candidates |= {v for v in leaves if v not in used}
        names = {v[1:] for v in candidates}
        isle_vars = set()
        collect_isle_vars(rule.lhs, isle_vars)
        self.plain = {n for n in isle_vars if n.lower() in names}
        self.plain_fresh = {v for v in candidates if v.startswith("?t")}
        if self.plain or self.plain_fresh:
            self.fresh_counter = itertools.count()
            self.choice_counter = itertools.count()
            try:
                alts = self.compile_rule(rule)
            except Unsupported:
                self.plain = set()
                self.plain_fresh = set()
                self.fresh_counter = itertools.count()
                self.choice_counter = itertools.count()
                alts = self.compile_rule(rule)
        return alts


def wildcard_unused(rule):
    """ISLE `_name` variables used once are wildcards; make them `_`."""
    counts = {}

    def count(x):
        if isinstance(x, list):
            for y in x:
                count(y)
        elif isinstance(x, str) and x.startswith("_") and x != "_":
            counts[x] = counts.get(x, 0) + 1
    body = [rule.lhs, [list(c[1:]) for c in rule.clauses], rule.rhs]
    count(body)
    single = {v for v, n in counts.items() if n == 1}
    if not single:
        return rule

    def walk(x):
        if isinstance(x, list):
            return [walk(y) for y in x]
        return "_" if x in single else x
    return replace(rule, lhs=walk(rule.lhs), rhs=walk(rule.rhs),
                   clauses=[(k, walk(p), walk(e)) for k, p, e in rule.clauses])


def collect_leaves(ast, acc):
    if ast[0] == "leaf":
        acc.add(ast[1])
    elif ast[0] == "node":
        for a in ast[2]:
            collect_leaves(a, acc)


def collect_isle_vars(x, acc):
    if isinstance(x, list):
        for y in x:
            collect_isle_vars(y, acc)
    elif isinstance(x, str) and not isinstance(x, Int) and x[:1].isalpha() or \
            (isinstance(x, str) and x.startswith("_") and x != "_"):
        acc.add(x)


# ---------------------------------------------------------------------------
# Emitting Lisp from step lists.

def emit_steps(alts, final):
    """Lisp body for alternatives ALTS, each a list of steps ending in a final
    step. FINAL renders a final step."""
    if len(alts) == 1 and not alts[0]:
        raise AssertionError
    firsts = [a[0] for a in alts]
    if all(f == firsts[0] for f in firsts):
        step = firsts[0]
        rest = [a[1:] for a in alts]
        if step[0] == "final":
            return final(step[1])
        return wrap_step(step, lambda: emit_steps(rest, final))
    # Diverging alternatives: a deterministic choice, or independent branches.
    groups = []
    for a in alts:
        for g in groups:
            if g[0][0] == a[0]:
                g.append(a)
                break
        else:
            groups.append([a])
    cids = {f[1] for f in firsts if f[0] == "alt"}
    if len(cids) == 1 and all(f[0] == "alt" for f in firsts):
        cid = cids.pop()
        groups.sort(key=lambda g: g[0][0][2])
        branches = [emit_steps([a[1:] for a in g], final) for g in groups]
        return ["block", cid] + branches
    return ["progn"] + [emit_steps(g, final) for g in groups]


def wrap_step(step, body):
    kind = step[0]
    if kind == "let":
        return ["let", [[step[1], step[2]]], body()]
    if kind == "test":
        return ["when", step[1], body()]
    if kind == "mvb":
        vs, call = step[1], step[2]
        check = step[3] if len(step) > 3 else vs[0]
        return ["multiple-value-bind", vs, call,
                ["declare", ["ignorable"] + vs],
                ["when", check, body()]]
    if kind == "alt":
        return body()
    if kind == "commit":
        return ["progn", body(), ["return-from", step[1], "nil"]]
    raise AssertionError(step)


def simplify_lisp(x):
    """Merge nested lets into let* and nested whens into and."""
    if not isinstance(x, list):
        return x
    x = [simplify_lisp(y) for y in x]
    if x and x[0] in ("let", "let*") and len(x) == 3 and isinstance(x[2], list) \
            and x[2] and x[2][0] in ("let", "let*") and len(x[2]) == 3:
        return ["let*", x[1] + x[2][1], x[2][2]]
    if x and x[0] == "when" and len(x) == 3 and isinstance(x[2], list) \
            and x[2] and x[2][0] == "when" and len(x[2]) == 3:
        a = x[1][1:] if isinstance(x[1], list) and x[1][0] == "and" else [x[1]]
        return ["when", ["and"] + a + [x[2][1]], x[2][2]]
    return x


def pretty(x, indent=0, width=100):
    """Pretty-print Lisp code X."""
    flat = lisp(x)
    if not isinstance(x, list) or len(flat) + indent <= width:
        return flat
    head = x[0]
    pad = " " * (indent + 2)
    if head in ("let", "let*", "multiple-value-bind", "when", "block", "dolist", "progn",
                "return-from") and len(x) >= 2:
        nhead = {"let": 2, "let*": 2, "multiple-value-bind": 3, "when": 2, "block": 2,
                 "dolist": 2, "progn": 1, "return-from": 2}[head]
        if head in ("let", "let*"):
            binds = x[1]
            inner = " " * (indent + len(head) + 3)
            bstr = ("(" + ("\n" + inner).join(pretty(b, indent + len(head) + 3, width)
                                               for b in binds) + ")")
            first = f"({head} {bstr}"
        else:
            first = "(" + " ".join(lisp(y) for y in x[:nhead])
        body = [pretty(y, indent + 2, width) for y in x[nhead:]]
        return first + "".join("\n" + pad + b for b in body) + ")"
    if head in ("and", "or"):
        inner = " " * (indent + len(head) + 2)
        return f"({head} " + ("\n" + inner).join(pretty(y, indent + len(head) + 2, width)
                                                  for y in x[1:]) + ")"
    return "(" + lisp(head) + "".join("\n" + pad + pretty(y, indent + 2, width)
                                      for y in x[1:]) + ")"


def isle_text(x):
    if isinstance(x, list):
        if x and x[0] == "@":
            return f"{isle_text(x[1])} @ {isle_text(x[2])}"
        return "(" + " ".join(isle_text(y) for y in x) + ")"
    return str(x)


def rule_isle(rule):
    parts = ["rule"]
    if rule.name:
        parts.append(rule.name)
    if rule.prio:
        parts.append(str(rule.prio))
    s = "(" + " ".join(parts) + " " + isle_text(rule.lhs)
    for kind, pat, e in rule.clauses:
        s += f" ({kind} {isle_text(pat) + ' ' if pat is not None else ''}{isle_text(e)})"
    return s + " " + isle_text(rule.rhs) + ")"


def comment_block(text, prefix=";; "):
    out, line = [], ""
    for word in text.split(" "):
        if line and len(line) + len(word) + 1 > 96:
            out.append(prefix + line)
            line = "    " + word
        else:
            line = (line + " " + word) if line else word
    out.append(prefix + line)
    return "\n".join(out)


def render_alts(alts):
    """ggs rule forms for the alternatives of one ISLE rule."""
    ks = {}
    for _, steps, _ in alts:
        for st in steps:
            if st[0] == "alt":
                ks.setdefault(st[1], set()).add(st[2])
    single = {cid for cid, k in ks.items() if len(k) == 1}
    alts = [(lhs, [st for st in steps if not (st[0] in ("alt", "commit") and st[1] in single)],
             tmpl) for lhs, steps, tmpl in alts]
    by_lhs = {}
    for lhs, steps, tmpl in alts:
        by_lhs.setdefault(show(lhs), []).append(list(steps) + [("final", tmpl)])
    forms = []
    for lhs, seqs in by_lhs.items():
        if len(seqs) == 1 and len(seqs[0]) == 1:
            forms.append(f"({lhs}\n {show(seqs[0][0][1])})")
            continue
        if len(seqs) == 1 and all(s[0] == "test" for s in seqs[0][:-1]):
            tests = [s[1] for s in seqs[0][:-1]]
            guard = tests[0] if len(tests) == 1 else ["and"] + tests
            forms.append(f"({lhs}\n {show(seqs[0][-1][1])}\n :guard {pretty(guard, 8)})")
            continue
        body = simplify_lisp(emit_steps(seqs, lambda t: ["yield-rewrite", Raw(show(t))]))
        forms.append(f"({lhs}\n :eval\n {pretty(body, 1)})")
    return forms


class Raw(str):
    """Already-rendered Lisp text."""


# ---------------------------------------------------------------------------

def generate_helper(tr, d):
    """Lisp DEFUN for a primitive-only internal constructor."""
    params = [f"?arg{i}" for i in range(len(d.args))]
    partial = "partial" in d.flags
    bool_partial = partial and d.ret == "bool"
    name = kebab(d.name)
    seqs = []
    for r in sorted(d.rules, key=lambda r: -r.prio):
        r = wildcard_unused(r)
        tr.fresh_counter = itertools.count()
        st = State(env={})

        def match(i, st):
            if i == len(params):
                yield st
                return
            for st2 in tr.match_val(r.lhs[i + 1], d.args[i], params[i], st):
                yield from match(i + 1, st2)
        for st2 in match(0, st):
            for st3 in tr.clauses(r.clauses, st2):
                for code, st4 in tr.compile_expr(r.rhs, d.ret, st3):
                    seqs.append(list(st4.steps) + [("final", code)])
    ret = (lambda c: ["return-from", name, ["values", c, "t"]]) if bool_partial else \
        (lambda c: ["return-from", name, c])
    branches = []
    for s in seqs:
        branches.append(simplify_lisp(emit_steps([s], ret)) if len(s) > 1 else ret(s[0][1]))
    fail = ["values", "nil", "nil"] if bool_partial else \
        "nil" if partial else ["error", f'"No rule of {d.name} matched"']
    params_l = "(" + " ".join(params) + ")"
    body = "\n  ".join(pretty(b, 2) for b in [["block", name] + branches + [fail]])
    doc = f"ISLE {'partial ' if partial else ''}constructor `{d.name}` ({d.source})."
    return f"(defun {name} {params_l}\n  \"{doc}\"\n  {body})"


def generate_analysis(tr):
    clauses = []
    for name in sorted(tr.clif_ops):
        d = tr.p.decls[name]
        args = [f"?a{i}" for i in range(len(d.args))]
        if d.args and d.args[0] == "Type":
            pat = f"({name} (?ty){''.join(' ' + a for a in args[1:])})"
            clauses.append(f"  ({pat} ?ty)")
        elif name in VALUE_SKELETON_OPS:
            pat = f"({name} {' '.join(args)})"
            clauses.append(f"  ({pat} (clif-type ?a0))")
    # Values defined outside the rewritten expression (function and block
    # parameters) are written (param TYPE NAME).
    clauses.append("  ((param (?ty) ?name) ?ty)")
    return ("(define-analysis (clif-type :merge #'or-eql)\n"
            + "\n".join(clauses) + ")")


def main():
    p = Program()
    for f in PRELUDES:
        p.load(ROOT / "cranelift_ref" / "prelude" / f"{f}.isle", keep_rules=False)
    per_file = {}
    for f in OPT_FILES:
        n = len(p.rules)
        p.load(ROOT / "cranelift_ref" / "opts" / f"{f}.isle")
        per_file[f] = p.rules[n:]
    p.link()

    tr = Translator(p)
    outdir = ROOT / "cranelift" / "rules"
    outdir.mkdir(parents=True, exist_ok=True)
    total = failed = generated = 0
    for f, rules in per_file.items():
        chunks = []
        for rule in rules:
            if rule.lhs[0] not in ("simplify", "simplify_skeleton"):
                continue
            total += 1
            text = rule_isle(rule)
            try:
                alts = tr.translate_rule(rule)
                if not alts:
                    raise Unsupported("no satisfiable alternative")
            except Unsupported as exc:
                failed += 1
                chunks.append(comment_block(text, ";; ISLE: ") + "\n"
                              + f";; NOT TRANSLATED: {exc}")
                continue
            forms = render_alts(alts)
            generated += len(forms)
            chunks.append(comment_block(text, ";; ISLE: ") + "\n" + "\n".join(forms))
        header = (f";;;; Generated by tools/isle2ggs.py from cranelift_ref/opts/{f}.isle.\n"
                  ";;;; Do not edit; regenerate instead.\n\n"
                  "(in-package #:cranelift-opts)\n\n")
        body = "\n\n".join("  " + c.replace("\n", "\n  ") for c in chunks)
        (outdir / f"{f}.lisp").write_text(header + f"(defrw* {f}\n" + body + ")\n")

    helpers = []
    done = set()
    while tr.helpers_used - done:
        name = sorted(tr.helpers_used - done)[0]
        done.add(name)
        helpers.append(generate_helper(tr, p.decls[name]))
    support = (";;;; Generated by tools/isle2ggs.py. Do not edit; regenerate instead.\n\n"
               "(in-package #:cranelift-opts)\n\n"
               ";;; Type of each CLIF value: the type argument of value-producing opcodes,\n"
               ";;; or the first operand's type for value-producing skeleton instructions.\n"
               + generate_analysis(tr) + "\n\n"
               ";;; Internal ISLE constructors whose arguments and result are primitive.\n\n"
               + "\n\n".join(helpers) + "\n")
    (outdir / "support.lisp").write_text(support)
    print(f"{total} ISLE rules: {total - failed} translated into {generated} ggs rules, "
          f"{failed} not translated")


if __name__ == "__main__":
    main()
