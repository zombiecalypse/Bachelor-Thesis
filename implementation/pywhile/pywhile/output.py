from .executor import Nil, Cons, nil, hd, tl
from .exceptions import AtomicError
def identity(i):
    return i

def bool(i):
    return i != nil

def list(i):
    o = []
    while i != nil:
        o.append(hd(i))
        i = tl(i)
    return o

def integer(i):
    digits = reversed(list(i))
    x = 0
    for d in digits:
        x *= 2
        if d != nil:
            x += 1
    return x

def lispify(x, indent = 1):
    if not isinstance(x, Cons): return x
    try:
        as_list = list(x)
        assert as_list, "Should be handled as nil"
    except AtomicError:
        return x
    tokens = [lispify(e, indent + 1) for e in as_list]

    if len(tokens) == 1: return "({})".format(tokens[0])

    string_lst = ['({} '.format(tokens[0])] #)
    for t in tokens[1:]:
        string_lst.append("{spaces}{token}".format(spaces='  ' * indent, token=t))
    return "\n".join(string_lst) + ')'
