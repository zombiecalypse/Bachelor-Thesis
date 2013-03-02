from collections import defaultdict
from .ast import *
from .collectors import NullCollector
from .exceptions import AtomicError

class Tree(object):
    def __getitem__(self, i):
        if i == 0: return hd(self)

        return tl(self)[i-1]
    def size(self):
        raise NotImplementedError()
 
class Nil(Tree): 
    def __repr__(self):
        return "Nil"
    def __eq__(self, o):
        return isinstance(o, Nil)
    def size(self):
        return 0

class Cons(Tree):
    def __init__(self, l, r):
        Tree.__init__(self)
        self.left = l
        self.right = r
    def __repr__(self):
        return "ConsCell({}, {})".format(self.left, self.right)
    def __eq__(self, o):
        if not isinstance(o, Cons): return False
        return self.left == o.left and self.right == o.right
    def size(self):
        return 1 + self.left.size() + self.right.size()

class Symbol(Tree):
    def __init__(self, name):
        Tree.__init__(self)
        self.name = name
    def __repr__(self):
        return "Sym[{}]".format(self.name)
    def __eq__(self, o):
        if not isinstance(o, Symbol): return False
        return self.name == o.name
    def size(self):
        return 1

nil = Nil()
def cons(l, r):
    return Cons(l, r)
def sym(name):
    return Symbol(name)
def hd(e):
    if not isinstance(e, Cons): raise AtomicError("Head of nil")
    return e.left
def tl(e):
    if not isinstance(e, Cons): raise AtomicError("Tail of nil")
    return e.right


class Context(object):
    def __init__(self, ast, collector = NullCollector(), parent = None):
        self.programs = {}
        self.collector = collector
        self.parent = parent
        for prog in ast:
            self.programs[prog.name] = prog
        assert self.collector

    def executeBlock(self, block):
        for statement in block:
            self.executeStatement(statement)

    def executeStatement(self, state):
        assert self.collector
        self.collector.statement(self, state)
        if isinstance(state, Assignment):
            eval = self.executeExpression(state.expr)
            self.context[state.var] = eval
        elif isinstance(state, If):
            evals = self.executeExpression(state.expr)
            if evals != nil:
                self.executeBlock(state.block)
            else:
                self.executeBlock(state.eblock)
        elif isinstance(state, For):
            eval = self.executeExpression(state.expr)
            while eval != nil:
                self.context[state.ident] = hd(eval)
                self.executeBlock(state.block)
                eval = tl(eval)
        elif isinstance(state, While):
            while self.executeExpression(state.expr) != nil:
                self.executeBlock(state.block)
        else:
            assert False, "Weird statement %s (%s)" % (state, state.__class__)

    def executeExpression(self, exp):
        assert self.collector
        self.collector.expression(self, exp)
        if isinstance(exp, SymExp):
            return sym(exp.name)
        elif isinstance(exp, Var):
            return self.context[exp.name]
        elif isinstance(exp, NilExp):
            return nil
        elif isinstance(exp, HdExp):
            return hd(self.executeExpression(exp.expr))
        elif isinstance(exp, TlExp):
            return tl(self.executeExpression(exp.expr))
        elif isinstance(exp, ConsExp):
            left = self.executeExpression(exp.left)
            right = self.executeExpression(exp.right)
            return cons(left, right)
        elif isinstance(exp, FuncCall):
            return Context(self.programs.values(), parent = self).executeProgram(exp.func, self.executeExpression(exp.argument))
        elif isinstance(exp, UniversalCall):
            eval = self.executeExpression(exp.func_coded)
            eval_arg = self.executeExpression(exp.argument)
            return self.executeUniversal(eval, eval_arg)
        elif isinstance(exp, EqExp):
            left = self.executeExpression(exp.left)
            right = self.executeExpression(exp.right)
            return self.bool(left == right)
        elif isinstance(exp, SourceExp):
            return self.dumpSource(exp.func)
        elif isinstance(exp, AtomExp):
            eval = self.executeExpression(exp.exp)
            return self.bool(isinstance(eval, Symbol) or isinstance(eval, Nil))

    def executeUniversal(self, data, arg):
        parsed = self.parseSource(data)
        return Context(self.programs.values()).executeAst(parsed, arg)

    def asList(self, data):
        x = []
        while data != nil:
            x.append(hd(data))
            data = tl(data)
        return x

    def parseSource(self, ast):
        s, name, input, output, block_lst = self.asList(ast)
        assert s == sym('program'), "AST is no program %s" % ast
        name = name.name
        input = input.name
        output = output.name
        block = self.parseBlock(block_lst)
        return Program(name, input, output, block)

    def parseBlock(self, l):
        return map(self.parseStatement, self.asList(l))

    def parseStatement(self, s):
        n = s[0].name
        if n == 'STif':
            expr = self.parseExpression(s[1])
            block = self.parseBlock(s[2])
            eblock = self.parseBlock(s[3])
            return If(expr, block, eblock)
        elif n == 'STfor':
            ident = s[1].name
            expr = self.parseExpression(s[2])
            block = self.parseBlock(s[3])
            return For(ident, expr, block)
        elif n == 'STwhile':
            expr = self.parseExpression(s[1])
            block = self.parseBlock(s[2])
            return While(expr, block)
        elif n == 'STassignment':
            ident = s[1].name
            expr = self.parseExpression(s[2])
            return Assignment(ident, expr)
        assert False, "Is not a valid statement: %s" % (s,)

    def parseExpression(self, e):
        e = self.asList(e)
        n = e[0].name
        if n == 'EXsymbol':
            return SymExp(e[1].name)
        elif n == 'EXvar':
            return Var(SymExp(e[1]))
        elif n == 'EXnil':
            return NilExp()
        elif n == 'EXcons':
            return ConsExp(self.parseExpression(e[1]), self.parseExpression(e[2]))
        elif n == 'EXhd':
            return HdExp(self.parseExpression(e[1]))
        elif n == 'EXtl':
            return TlExp(self.parseExpression(e[1]))
        elif n == 'EXeq':
            return EqExp(self.parseExpression(e[1]), self.parseExpression(e[2]))
        elif n == 'EXcall':
            return FuncCall(e[1].name, self.parseExpression(e[2]))
        elif n == 'EXuniversal':
            return UniversalCall(self.parseExpression(e[1]), self.parseExpression(e[2]))
        elif n == 'EXsource':
            return SourceExp(e[1].name)
        elif n == 'EXatom':
            return AtomExp(self.parseExpression(e[1]))
        assert False, "Is not a valid Expression: %s" % (e,)

    def dumpSource(self, name):
        ast = self.programs[name]
        return self.listData([
                sym('program'),
                sym(ast.name),
                sym(ast.input),
                sym(ast.output),
                self.dumpBlock(ast.block)])

    def dumpBlock(self, b):
        return self.listData(self.dumpStatement(s) for s in b)

    def dumpStatement(self, s):
        if isinstance(s, If):
            return self.listData([
                sym('STif'),
                self.dumpExpression(s.expr),
                self.dumpBlock(s.block),
                self.dumpBlock(s.eblock)])
        elif isinstance(s, For):
            return self.listData([
                sym('STfor'),
                sym(s.ident),
                self.dumpExpression(s.expr),
                self.dumpBlock(s.block)])
        elif isinstance(s, While):
            return self.listData([
                sym('STwhile'),
                self.dumpExpression(s.expr),
                self.dumpBlock(s.block)])
        elif isinstance(s, Assignment):
            return self.listData([
                sym('STassignment'),
                sym(s.var),
                self.dumpExpression(s.expr)])

    def dumpExpression(self, e):
        if isinstance(e, SymExp):
            return self.listData([
                sym('EXsymbol'),
                sym(e.name)])
        elif isinstance(e, Var):
            return self.listData([
                sym('EXvar'),
                sym(e.name)])
        elif isinstance(e, NilExp):
            return self.listData([sym('EXnil')])
        elif isinstance(e, HdExp):
            return self.listData([
                sym('EXhd'),
                self.dumpExpression(e.expr)])
        elif isinstance(e, TlExp):
            return self.listData([
                sym('EXtl'),
                self.dumpExpression(e.expr)])
        elif isinstance(e, ConsExp):
            return self.listData([
                sym('EXcons'),
                self.dumpExpression(e.left),
                self.dumpExpression(e.right)])
        elif isinstance(e, EqExp):
            return self.listData([
                sym('EXeq'),
                self.dumpExpression(e.left),
                self.dumpExpression(e.right)])
        elif isinstance(e, FuncCall):
            return self.listData([
                sym('EXcall'),
                sym(e.func),
                self.dumpExpression(e.argument)])
        elif isinstance(e, UniversalCall):
            return self.listData([
                sym('EXuniversal'),
                self.dumpExpression(e.func_coded),
                self.dumpExpression(e.argument)])
        elif isinstance(e, SourceExp):
            return self.listData([
                sym('EXsource'),
                sym(e.func)])
        elif isinstance(e, AtomExp):
            return self.listData([
                sym('EXatom'),
                self.dumpExpression(e.exp)])

    def listData(self, l):
        x = nil
        for i in reversed(list(l)):
            x = cons(i, x)
        return x

    def bool(self, x):
        if x:
            return cons(nil, nil)
        else:
            return nil

    def executeProgram(self, name, input):
        try:
            return self.executeAst(self.programs[name], input)
        except AssertionError as e:
            print self.collector.report()
            raise e

    def executeAst(self, ast, input):
        assert isinstance(input, Tree), "Input must be data, but isn't: %s" % input
        self.context = defaultdict(lambda : nil)
        self.context[ast.input] = input
        self.executeBlock(ast.block)
        self.collector.end(self)
        return self.context[ast.output]
