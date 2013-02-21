from .executor import Nil, Cons, nil, hd, tl
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
