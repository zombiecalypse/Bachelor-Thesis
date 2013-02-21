def disp_block(block, indent):
    ind = "  " * indent
    ret = ""
    ret += '{\n'
    ret += ";\n".join(statement._repr(indent + 1) for statement in block)
    ret += "\n{ind}".format(ind = ind)
    ret += '}\n'
    return ret

class DataExp(object): pass

class SymExp(DataExp):
    def __init__(self, name):
        assert isinstance(name, basestring), "%s is no string" % name
        DataExp.__init__(self)
        self.name = name
    def __repr__(self):
        return ":{}".format(self.name)

class Var(DataExp):
    def __init__(self, name):
        assert isinstance(name, basestring),  "%s is no string" % name
        DataExp.__init__(self)
        self.name = name
    def __repr__(self):
        return "Var[{}]".format(self.name)

class NilExp(DataExp):
    def __repr__(self):
        return "NilExp"

class HdExp(DataExp):
    def __init__(self, exp):
        assert isinstance(exp, DataExp), "%s is no data" % exp
        DataExp.__init__(self)
        self.expr = exp

    def __repr__(self):
        return "hd({})".format(self.expr)
class TlExp(DataExp):
    def __init__(self, exp):
        assert isinstance(exp, DataExp), "%s is no data" % exp
        DataExp.__init__(self)
        self.expr = exp

    def __repr__(self):
        return "tl({})".format(self.expr)

class ConsExp(DataExp):
    def __init__(self, left, right):
        assert isinstance(left, DataExp), "%s is no data" % left
        assert isinstance(right, DataExp), "%s is no data" % right
        DataExp.__init__(self)
        self.left = left
        self.right = right
    def __repr__(self):
        return "Cons({}, {})".format(self.left, self.right)
    def list(self):
        current = self
        list = []
        while not isinstance(current, NilExp):
            list.append(current.left)
            current = current.right
        return list
class EqExp(DataExp):
    def __init__(self, left, right):
        assert isinstance(left, DataExp), "%s is no data" % left
        assert isinstance(right, DataExp), "%s is no data" % right
        DataExp.__init__(self)
        self.left = left
        self.right = right
    def __repr__(self):
        return "({}) = ({})".format(self.left, self.right)

class FuncCall(DataExp):
    def __init__(self, func, argument):
        assert isinstance(func, basestring), "%s is no string" % func
        assert isinstance(argument, DataExp), "%s is no data" % argument
        DataExp.__init__(self)
        self.func = func
        self.argument = argument

    def __repr__(self):
        return "[{}]({})".format(self.func, self.argument)
class UniversalCall(DataExp):
    def __init__(self, func_coded, argument):
        assert isinstance(func_coded, DataExp), "%s is no data" % func_coded
        assert isinstance(argument, DataExp), "%s is no data" % argument
        DataExp.__init__(self)
        self.func_coded = func_coded
        self.argument = argument

    def __repr__(self):
        return "[[{}]]({})".format(self.func_coded, self.argument)
class SourceExp(DataExp):
    def __init__(self, func):
        assert isinstance(func, basestring), "%s is no string" % func
        DataExp.__init__(self)
        self.func = func
    def __repr__(self):
        return "<{}>".format(self.func)
class AtomExp(DataExp):
    def __init__(self, exp):
        assert isinstance(exp, DataExp), "%s is no data" % exp
        DataExp.__init__(self)
        self.exp = exp

    def __repr__(self):
        return "atom?({})".format(self.exp)

class Statement(object):
    def disp_block(self, block, indent):
        return disp_block(block, indent)
    def __repr__(self):
        return self._repr()
class Assignment(Statement):
    def __init__(self, var, expr):
        Statement.__init__(self)
        self.var = var
        self.expr = expr
    def __repr__(self):
        return self._repr()
    def _repr(self, indent = 0):
        ind = "  " * indent
        return "{ind}{} := {}".format(self.var, self.expr, ind = ind)
class If(Statement):
    def __init__(self, expr, block, eblock):
        Statement.__init__(self)
        self.expr = expr
        self.block = block
        self.eblock = eblock
    def _repr(self, indent = 0):
        ind = "  " * indent
        ret = ""
        ret += "{ind}if {expr} {blc}".format(ind = ind, expr = self.expr, blc =
                self.disp_block(self.block, indent))
        if self.eblock:
            ret += ' else {blc}'.format(blc = self.disp_block(self.eblock, indent))
        return ret
    def __repr__(self):
        return self._repr()

class For(Statement):
    def __init__(self, ident, expr, block):
        Statement.__init__(self)
        self.ident = ident
        self.expr = expr
        self.block = block
    def _repr(self, indent = 0):
        ind = "  " * indent
        ret = ""
        ret += "{ind}for {ident} in {expr} {blc}".format(ind = ind, ident =
                self.ident, expr = self.expr, 
                blc = self.disp_block(self.block, indent))
        return ret
class While(Statement):
    def __init__(self, expr, block):
        Statement.__init__(self)
        self.expr = expr
        self.block = block
    def _repr(self, indent = 0):
        ind = "  " * indent
        ret = ""
        ret += "{ind}while ({expr}) {blc}".format(ind = ind, expr = self.expr, 
                blc = self.disp_block(self.block, indent))
        return ret

class Program(object):
    def __init__(self, name, inp, outp, block):
        self.name = name
        self.input = inp
        self.output = outp
        self.block = block
    def __repr__(self):
        return "{name} read {inp} {blc} write {outp}".format(name = self.name,
                inp = self.input, outp = self.output, blc =
                disp_block(self.block, 0))
