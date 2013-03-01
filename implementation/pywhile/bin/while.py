#!/usr/bin/env python
import pywhile
from pywhile import cons, nil
import argparse

if __name__ == '__main__':
    argument = argparse.ArgumentParser('while')
    argument.add_argument('files', metavar='FILES', nargs='+',
            help='Files to read functions from')
    argument.add_argument('-B', '--bool', dest = 'output', action='store_const',
            const = pywhile.bool, default = pywhile.identity,
            help = 'The output is a boolean and printed accordingly')
    argument.add_argument('-I', '--int', dest = 'output',  action='store_const',
            const = pywhile.integer, default = pywhile.identity,
            help = 'The output is an integer and printed accordingly')
    argument.add_argument('-L', '--list', dest = 'output', action='store_const',
            const = pywhile.list, default = pywhile.identity,
            help = 'The output is a list and printed accordingly')
    argument.add_argument('-i', '--input', default = 'nil', help = 'The main function gets this expression as argument, default to nil')
    argument.add_argument('--trace',  action="append_const", dest = 'collector',
                          const = pywhile.TraceCollector(), help = 'Prints the complete trace of the program in the end.')
    argument.add_argument('--space',  action="append_const", dest = 'collector',
                          const = pywhile.SpaceCollector(), help = 'Reports the space usage of the program.')
    argument.add_argument('--time',  action="append_const", dest = 'collector',
                          const = pywhile.TimeCollector(),
            help = 'Reports the time usage of the program.')
    args = argument.parse_args()
    progs = []
    for f in args.files:
        for p in pywhile.parseFile(f):
            progs.append(p)
    collector = reduce(lambda x,y: x+y, args.collector, pywhile.NullCollector())
    context = pywhile.Context(progs, collector = collector)
    input = context.executeExpression(pywhile.parseExpression(args.input)[0])
    last_name = progs[-1].name
    ret = context.executeProgram(last_name, input)
    print args.output(ret)
    print
    print context.collector.report()
