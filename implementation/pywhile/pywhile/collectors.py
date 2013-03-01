from .ast import Assignment

class Collector(object):
    "Provides logging for time, size, ..."

    def __add__(self, o):
        return CombinedCollector(self, o)

    def statement(self, context, s):
        raise NotImplementedError()
    def expression(self, context, s):
        raise NotImplementedError()
    def end(self, context):
        pass
    def report(self):
        raise NotImplementedError()

class NullCollector(Collector):
    def statement(self, c, s): pass
    def expression(self, c, e): pass
    def report(self): 
        return ""
    def __add__(self, o):
        return o
    __radd__ = __add__
class TraceCollector(Collector):
    def __init__(self):
        self.trace = []
    def statement(self, c, stat):
        self.trace.append(stat)
    def expression(self, c, expr):
        self.trace.append(expr)
    def report(self):
        return "Trace:\n" + "\n".join("* {}".format(x) for x in self.trace)

class CombinedCollector(Collector):
    def __init__(self, *others):
        self.others = others
    def statement(self, context, s):
        return map(lambda x: x.statement(context, s), self.others)
    def expression(self, context, e):
        return map(lambda x: x.expression(context, e), self.others)
    def __add__(self, o):
        return CombinedCollector(self.others + [o])
    __radd__ = __add__
    def report(self):
        return "\n".join(o.report() for o in self.others)

class TimeCollector(Collector):
    def __init__(self):
        self.time = 0
    def statement(self, context, s):
        self.time += 1
    def expression(self, context, e):
        self.time += 1
    def report(self):
        return "Time:".ljust(20) + str(self.time)

class SpaceCollector(Collector):
    def __init__(self):
        self.space_max = 0
    def statement(self, context, s):
        if not isinstance(s, Assignment): return None
        self.update_max(context)
    def end(self, context):
        self.update_max(context)
    def update_max(self, context):
        s = sum(v.size() for v in context.context.values())
        self.space_max = max(self.space_max, s)
    def expression(self, context, e):
        pass 
    def report(self):
        return "Space:".ljust(20) + str(self.space_max)

class VerySmartSpaceCollector(SpaceCollector):
    """Takes memory reuse into account, i.e. that cons nil X is only one space,
    as long as X is in memory"""
