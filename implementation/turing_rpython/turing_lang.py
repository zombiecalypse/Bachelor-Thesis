from pypy.rlib.parsing.ebnfparse import parse_ebnf, make_parse_function
from pypy.rlib.parsing.regexparse import parse_regex as reg
from pypy.rlib.parsing.tree import Nonterminal

grammar = """
NAME: "\\w+";
IGNORE: " |\n";
SEPARATOR: ";";
MOVEMENT: "L|N|R";
file: expr [SEPARATOR] >file< | expr [SEPARATOR];
expr: <transition> | <endstate>;
state: NAME;
value: NAME;
mov: <MOVEMENT>;
input: state value;
output: state value mov;
transition: input ["=>"] output;
endstate: [":"] <state>;
"""

class TuringVisitor(object):
    def __init__(self, list_of_endstates, list_of_transitions):
        self.list_of_endstates = list_of_endstates
        self.list_of_transitions = list_of_transitions
    def visit_transition(self, node):
        read = ReadHead(node.children[0].additional_info,
                node.children[1].additional_info)
        write = Command(
                node.children[2].additional_info,
                node.children[3].additional_info,
                node.children[4].additional_info)
        self.list_of_transitions.append((read, write))
    def visit_endstate(self, node):
        self.list_of_endstates.append(node.children[0].additional_info)
    def dispatch(self, node):
        if node.symbol == 'endstate':
            return self.visit_endstate(node)
        elif node.symbol == 'transition':
            return self.visit_transition(node)
        elif isinstance(node, Nonterminal):
            for c in node.children:
                self.dispatch(c)

regexs, rules, ast_visitor = parse_ebnf(grammar)
parsef = make_parse_function(regexs, rules)

class Command:
    def __init__(self, val, state, mov):
        self.value = val
        self.state = state
        self.movement = mov

class ReadHead:
    def __init__(self, val, state):
        self.value = val
        self.state = state

def parse(code):
    t = parsef(code)
    return ast_visitor().transform(t)

def state_name(pt):
    return pt.children[0].additional_info

def build_information(text):
    ast = parse(text)
    l1,l2 = [], []
    turing_parser = TuringVisitor(l1, l2)
    turing_parser.dispatch(ast)

    return l1, l2
