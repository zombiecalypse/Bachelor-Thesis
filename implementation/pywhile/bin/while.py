#!/usr/bin/env python
import pywhile
from pywhile import cons, nil
import argparse
import logging

logger = logging.getLogger('Main')

if __name__ == '__main__':
    argument = argparse.ArgumentParser('while')
    argument.add_argument('files', metavar='FILES', nargs='+',
            help='Files to read functions from')
    argument.add_argument('-V', '--version', action = 'version', version = pywhile.version)
    argument.add_argument('-v', '--verbose', action = 'store_true')
    argument.add_argument('-B', '--bool', dest = 'output', action='store_const',
            const = pywhile.bool, default = pywhile.identity,
            help = 'The output is a boolean and printed accordingly')
    argument.add_argument('-I', '--int', dest = 'output',  action='store_const',
            const = pywhile.integer, default = pywhile.identity,
            help = 'The output is an integer and printed accordingly')
    argument.add_argument('-L', '--list', dest = 'output', action='store_const',
            const = pywhile.list, default = pywhile.identity,
            help = 'The output is a list and printed accordingly')
    argument.add_argument('--lisp', dest = 'output', action='store_const',
            const = pywhile.lispify, default = pywhile.identity,
            help = 'The output is a list and printed as if it was LISP code')
    argument.add_argument('-i', '--input', default = 'nil', help = 'The main function gets this expression as argument, default to nil')
    argument.add_argument('--trace',  action="append_const", dest = 'collector',
                          const = pywhile.TraceCollector(), help = 'Prints the complete trace of the program in the end.')
    argument.add_argument('--space',  action="append_const", dest = 'collector',
                          const = pywhile.SpaceCollector(), help = 'Reports the space usage of the program.')
    argument.add_argument('--time',  action="append_const", dest = 'collector',
                          const = pywhile.TimeCollector(),
            help = 'Reports the time usage of the program.')
    args = argument.parse_args()

    if args.verbose:
        logging.basicConfig(level = logging.DEBUG)
    else:
        logging.basicConfig()
    progs = []
    for f in args.files:
        logger.debug("parsing %s", f)
        for p in pywhile.parseFile(f):
            progs.append(p)
    for p in progs:
        logger.debug("Procedure: %s", p.name)
    logger.debug("Collectors in place: %s",args.collector)
    collector = reduce(lambda x,y: x+y, args.collector or [], pywhile.NullCollector())
    context = pywhile.Context(progs, collector = collector)
    input = context.executeExpression(pywhile.parseExpression(args.input)[0])
    last_name = progs[-1].name
    ret = context.executeProgram(last_name, input)
    print args.output(ret)
    if args.collector:
        print
        print context.collector.report()
