import pyparsing as PP
from .ast import *

def between(l,ex,r):
    if isinstance(l, basestring):
        l = PP.Literal(l)
    if isinstance(r, basestring):
        r = PP.Literal(r)
    return PP.Suppress(l) + ex + PP.Suppress(r)


keywords = nil, cons, hd, tl, atomwd, ifwd, elsewd, forwd, inwd, whilewd, readwd, writewd= \
        map(PP.Keyword, ['nil', 'cons', 'hd', 'tl', 'atom?', 'if', 'else',
            'for', 'in', 'while', 'read', 'write'])

keywords = PP.MatchFirst(keywords)('keywords')

identifier = (~keywords + PP.Regex('[a-zA-Z]\w*'))

dot, eq = map(PP.Literal, ['.', '='])

symbol = (PP.Literal(':') + identifier)
symbol.setParseAction(lambda x: SymExp(x[1]))

variable = identifier.copy()
variable.setParseAction(lambda x: Var(x[0]))


nil.setParseAction(lambda x: NilExp())

expr = PP.Forward()

hd = (PP.Literal('hd') + expr).setParseAction(lambda x: HdExp(x[1]))
tl = (PP.Literal('tl') + expr).setParseAction(lambda x: TlExp(x[1]))

consexp_lit = (cons + expr + expr).setParseAction(lambda x: ConsExp(x[1], x[2]))

eqexp = (expr + eq + expr).setParseAction(lambda x: EqExp(x[0],x[2]))


parended = between('(', expr, ')')
parended.setParseAction(lambda x: x[0])

numexp = PP.Word(PP.nums)('number')
numexp.setParseAction(lambda x: toBinaryCons(long(x[0])))

atom =  (nil | symbol | variable | numexp)


evalexp = (between('[',identifier, ']') + between('(', expr, ')'))
evalexp.setParseAction(lambda x: FuncCall(x[0],x[1]))

universalexp = between('[[', expr, ']]') + between('(', expr, ')')
universalexp.setParseAction(lambda x: UniversalCall(x[0], x[1]))

sourceexp = between('<', identifier, '>')
sourceexp.setParseAction(lambda x: SourceExp(x[0]))

atomexp = PP.Suppress(atomwd) + expr
atomexp.setParseAction(lambda x: AtomExp(x[0]))

expr << PP.operatorPrecedence(atom | parended | consexp_lit | universalexp |
        evalexp | atomexp | sourceexp, 
        [
            ('hd', 1, PP.opAssoc.RIGHT, lambda x: HdExp(x[0][1])),
            ('tl', 1, PP.opAssoc.RIGHT, lambda x: TlExp(x[0][1])),
            ("=", 2, PP.opAssoc.LEFT, lambda x: EqExp(x[0][0], x[0][2])),
            (".", 2, PP.opAssoc.RIGHT, lambda x: ConsExp(x[0][0],x[0][2])),
            ])

statement = PP.Forward()

block = between('{', PP.Group(PP.delimitedList(statement, PP.White())), '}')

ifstm = PP.Suppress(ifwd) + expr + block + PP.Optional(PP.Suppress(elsewd) + block, [])
ifstm.setParseAction(lambda x: If(x[0], x[1], x[2]))

assignstm = identifier + PP.Suppress(PP.Literal(':=')) + expr + PP.Suppress(PP.Literal(';'))
assignstm.setParseAction(lambda x: Assignment(x[0], x[1]))
assignstm("assignment")

forstm = PP.Suppress(forwd) + identifier + PP.Suppress(inwd) + expr + block
forstm.setParseAction(lambda x: For(x[0], x[1], x[2]))
forstm("for statement")

whilestm = PP.Suppress(whilewd) + expr + block
whilestm("while statement")
whilestm.setParseAction(lambda x: While(x[0], x[1]))

statement << (ifstm | forstm | whilestm | assignstm)

program = identifier + PP.Suppress(readwd) + identifier + block + PP.Suppress(writewd) + identifier
program.setParseAction(lambda x: Program(x[0], x[1], x[3], x[2]))

file = PP.OneOrMore(program)

def asList(*exprs):
    return reduce(lambda x,y: ConsExp(y,x), reversed(exprs), NilExp())

def toBinaryCons(integer):
    assert integer >= 0
    binary_digits = []
    while True:
        digit = integer % 2
        integer = integer // 2
        binary_digits.append(ConsExp(NilExp(), NilExp()) if digit else NilExp())
        if integer == 0:
            break
    return asList(*binary_digits)

def parseFile(f):
    return file.parseFile(f, True)
def parseExpression(e):
    return expr.parseString(e)
