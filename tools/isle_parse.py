"""Minimal ISLE reader and declaration table.

ISLE source is read into nested Python lists of atoms.  Atoms are kept as
strings except integer literals, which become `Int`.  `x @ PAT` (with or
without spaces around `@`) is read as the list ['@', 'x', PAT].
"""

import re
from dataclasses import dataclass, field
from pathlib import Path


class Int(int):
    """An ISLE integer literal (distinguished from symbols)."""


_TOKEN = re.compile(r"""\s+|;[^\n]*|(\()|(\))|(@)|([^\s();@]+)""")


def _int_literal(tok):
    t = tok.replace("_", "")
    neg = t.startswith("-")
    if neg:
        t = t[1:]
    try:
        if t[:2] in ("0x", "0X"):
            v = int(t[2:], 16)
        elif t[:2] == "0b":
            v = int(t[2:], 2)
        elif t[:2] == "0o":
            v = int(t[2:], 8)
        elif t.isdigit():
            v = int(t)
        else:
            return None
    except ValueError:
        return None
    return Int(-v if neg else v)


def tokenize(text):
    pos = 0
    while pos < len(text):
        m = _TOKEN.match(text, pos)
        if not m:
            raise SyntaxError(f"bad ISLE input at {text[pos:pos + 40]!r}")
        pos = m.end()
        if m.group(1):
            yield "("
        elif m.group(2):
            yield ")"
        elif m.group(3):
            yield "@"
        elif m.group(4):
            tok = m.group(4)
            lit = _int_literal(tok)
            yield lit if lit is not None else tok


def _fold_at(items):
    """Rewrite [..., x, '@', PAT, ...] into [..., ['@', x, PAT], ...]."""
    out = []
    i = 0
    while i < len(items):
        if i + 2 < len(items) + 1 and i + 1 < len(items) and items[i + 1] == "@":
            out.append(["@", items[i], items[i + 2]])
            i += 3
        else:
            out.append(items[i])
            i += 1
    return out


def read_all(text):
    stack = [[]]
    for tok in tokenize(text):
        if tok == "(":
            stack.append([])
        elif tok == ")":
            done = _fold_at(stack.pop())
            stack[-1].append(done)
        else:
            stack[-1].append(tok)
    if len(stack) != 1:
        raise SyntaxError("unbalanced parentheses")
    return _fold_at(stack[0])


def read_file(path):
    return read_all(Path(path).read_text())


@dataclass
class Decl:
    name: str
    args: list
    ret: str
    flags: set = field(default_factory=set)  # pure, multi, partial, rec
    extern_ctor: bool = False
    extern_etor: bool = False
    etor: object = None  # (params, body) of an internal extractor
    rules: list = field(default_factory=list)  # internal constructor rules
    source: str = ""


@dataclass
class Rule:
    name: object
    prio: int
    lhs: object
    clauses: list  # [('if-let', pat, expr)]
    rhs: object
    source: str


DECL_FLAGS = {"pure", "multi", "partial", "rec"}


def parse_rule(form, source):
    body = form[1:]
    name = None
    prio = 0
    if isinstance(body[0], str) and not isinstance(body[0], Int):
        name = body[0]
        body = body[1:]
    if isinstance(body[0], Int):
        prio = int(body[0])
        body = body[1:]
    lhs, *rest = body
    clauses = []
    for c in rest[:-1]:
        if c[0] == "if-let":
            clauses.append(("if-let", c[1], c[2]))
        elif c[0] == "if":
            clauses.append(("if", None, c[1]))
        else:
            raise SyntaxError(f"unknown rule clause {c!r}")
    return Rule(name, prio, lhs, clauses, rest[-1], source)


class Program:
    """All declarations, types, conversions and rules of a set of ISLE files."""

    def __init__(self):
        self.decls = {}
        self.types = {}  # name -> form
        self.enum_variants = {}  # "Type.Variant" -> (type, [field types])
        self.converts = {}  # (from, to) -> term
        self.rules = []  # top-level rules of files loaded with keep_rules
        self.all_rules = []

    def link(self):
        """Attach every rule to the declaration of its head term. Rules may
        precede their `decl`, so this runs after all files are loaded."""
        for d in self.decls.values():
            d.rules = []
        for rule in self.all_rules:
            if isinstance(rule.lhs, list) and rule.lhs[0] in self.decls:
                self.decls[rule.lhs[0]].rules.append(rule)

    def load(self, path, keep_rules=True):
        source = Path(path).name
        for form in read_file(path):
            self._toplevel(form, source, keep_rules)

    def _toplevel(self, form, source, keep_rules):
        head = form[0]
        if head == "decl":
            rest = form[1:]
            flags = set()
            while rest[0] in DECL_FLAGS:
                flags.add(rest[0])
                rest = rest[1:]
            name, args, ret = rest
            self.decls[name] = Decl(name, list(args), ret, flags, source=source)
        elif head == "extern":
            kind = form[1]
            if kind == "constructor":
                self.decls[form[2]].extern_ctor = True
            elif kind == "extractor":
                name = form[3] if form[2] == "infallible" else form[2]
                self.decls[name].extern_etor = True
            elif kind == "const":
                pass
        elif head == "extractor":
            pat, body = form[1], form[2]
            self.decls[pat[0]].etor = (pat[1:], body)
        elif head == "type":
            name = form[1]
            self.types[name] = form
            enum = next((f for f in form[2:] if isinstance(f, list) and f[0] == "enum"), None)
            if enum:
                for v in enum[1:]:
                    vname = v[0] if isinstance(v, list) else v
                    fields = [f[1] for f in v[1:]] if isinstance(v, list) else []
                    self.enum_variants[f"{name}.{vname}"] = (name, fields)
        elif head == "convert":
            self.converts[(form[1], form[2])] = form[3]
        elif head == "rule":
            rule = parse_rule(form, source)
            self.all_rules.append(rule)
            if keep_rules:
                self.rules.append(rule)
        # spec, model, form, instantiate, attr, state, macro: verification only
