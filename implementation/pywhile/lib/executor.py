from collections import defaultdict
from .ast import *

class Tree(object):
    def __getitem__(self, i):
        if i == 0: return hd(self)

        return tl(self)[i-1]
 
class Nil(Tree): 
    def __repr__(self):
        return "Nil"
    def __eq__(self, o):
        return isinstance(o, Nil)

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

class Symbol(Tree):
    def __init__(self, name):
        Tree.__init__(self)
        self.name = name
    def __repr__(self):
        return "Sym[{}]".format(self.name)
    def __eq__(self, o):
        if not isinstance(o, Symbol): return False
        return self.name == o.name

nil = Nil()
def cons(l, r):
    return Cons(l, r)
def sym(name):
    return Symbol(name)
def hd(e):
    assert isinstance(e, Cons), "Head of nil"
    return e.left
def tl(e):
    assert isinstance(e, Cons), "Tail of nil"
    return e.right

class Context(object):
    def __init__(self, ast):
        self.programs = {}
        self.trace = []
        for prog in ast:
            self.programs[prog.name] = prog

    def executeBlock(self, block):
        for statement in block:
            self.executeStatement(statement)

    def executeStatement(self, state):
        self.trace.append(state)
        if isinstance(state, Assignment):
            self.context[state.var] = self.executeExpression(state.expr)
        elif isinstance(state, If):
            evals = self.executeExpression(state.expr)
            if evals != nil:
                self.executeBlock(state.block)
            else:
                self.executeStatement(state.eblock)
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
        self.trace.append(exp)
        if isinstance(exp, Symbol):
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
            return Context(self.programs.values()).executeProgram(exp.func, self.executeExpression(exp.argument))
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
        if n == 'if':
            expr = self.parseExpression(s[1])
            block = self.parseBlock(s[2])
            eblock = self.parseBlock(s[3])
            return If(expr, block, eblock)
        elif n == 'for':
            ident = s[1].name
            expr = self.parseExpression(s[2])
            block = self.parseBlock(s[3])
            return For(ident, expr, block)
        elif n == 'while':
            expr = self.parseExpression(s[1])
            block = self.parseBlock(s[2])
            return While(expr, block)
        elif n == 'assignment':
            ident = s[1].name
            expr = self.parseExpression(s[2])
            return Assignment(ident, expr)
        assert False, "Is not a valid statement: %s" % (s,)

    def parseExpression(self, e):
        e = self.asList(e)
        n = e[0].name
        if n == 'symbol':
            return Symbol(e[1].name)
        elif n == 'var':
            return Var(Symbol(e[1]))
        elif n == 'nil':
            return NilExp()
        elif n == 'cons':
            return ConsExp(self.parseExpression(e[1]), self.parseExpression(e[2]))
        elif n == 'hd':
            return HdExp(self.parseExpression(e[1]))
        elif n == 'tl':
            return TlExp(self.parseExpression(e[1]))
        elif n == 'eq':
            return EqExp(self.parseExpression(e[1]), self.parseExpression(e[2]))
        elif n == 'call':
            return FuncCall(e[1].name, self.parseExpression(e[2]))
        elif n == 'universal':
            return UniversalCall(self.parseExpression(e[1]), self.parseExpression(e[2]))
        elif n == 'source':
            return SourceExp(e[1].name)
        elif n == 'atom':
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
                sym('if'),
                self.dumpExpression(s.expr),
                self.dumpBlock(s.block),
                self.dumpBlock(s.eblock)])
        elif isinstance(s, For):
            return self.listData([
                sym('for'),
                sym(s.ident),
                self.dumpExpression(s.expr),
                self.dumpBlock(s.block)])
        elif isinstance(s, While):
            return self.listData([
                sym('while'),
                self.dumpExpression(s.expr),
                self.dumpBlock(s.block)])
        elif isinstance(s, Assignment):
            return self.listData([
                sym('assignment'),
                sym(s.var),
                self.dumpExpression(s.expr)])

    def dumpExpression(self, e):
        if isinstance(e, Symbol):
            return self.listData([
                sym('symbol'),
                sym(e.name)])
        elif isinstance(e, Var):
            return self.listData([
                sym('var'),
                sym(e.name)])
        elif isinstance(e, NilExp):
            return self.listData([sym('nil')])
        elif isinstance(e, HdExp):
            return self.listData([
                sym('hd'),
                self.dumpExpression(e.expr)])
        elif isinstance(e, TlExp):
            return self.listData([
                sym('tl'),
                self.dumpExpression(e.expr)])
        elif isinstance(e, ConsExp):
            return self.listData([
                sym('cons'),
                self.dumpExpression(e.left),
                self.dumpExpression(e.right)])
        elif isinstance(e, EqExp):
            return self.listData([
                sym('eq'),
                self.dumpExpression(e.left),
                self.dumpExpression(e.right)])
        elif isinstance(e, FuncCall):
            return self.listData([
                sym('call'),
                sym(e.func),
                self.dumpExpression(e.argument)])
        elif isinstance(e, UniversalCall):
            return self.listData([
                sym('universal'),
                self.dumpExpression(e.func_coded),
                self.dumpExpression(e.argument)])
        elif isinstance(e, SourceExp):
            return self.listData([
                sym('source'),
                sym(e.func)])
        elif isinstance(e, AtomExp):
            return self.listData([
                sym('atom'),
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
            for s in self.trace:
                print "* {}".format(s)
            raise e

    def executeAst(self, ast, input):
        assert isinstance(input, Tree), "Input must be data, but isn't: %s" % input
        self.context = defaultdict(lambda : nil)
        self.context[ast.input] = input
        self.executeBlock(ast.block)
        return self.context[ast.output]
