import lib
from lib import cons, nil
import argparse

if __name__ == '__main__':
    argument = argparse.ArgumentParser('while')
    argument.add_argument('files', metavar='FILES', nargs='+',
            help='Files to read functions from')
    argument.add_argument('-B', '--bool', dest = 'output', action='store_const',
            const = lib.bool, default = lib.identity,
            help = 'The output is a boolean and printed accordingly')
    argument.add_argument('-I', '--int', dest = 'output',  action='store_const',
            const = lib.integer, default = lib.identity,
            help = 'The output is an integer and printed accordingly')
    argument.add_argument('-L', '--list', dest = 'output', action='store_const',
            const = lib.list, default = lib.identity,
            help = 'The output is a list and printed accordingly')
    argument.add_argument('-i', '--input', default = 'nil', 
            help = 'The main function gets this expression as argument, default to nil')
    args = argument.parse_args()
    progs = []
    for f in args.files:
        for p in lib.parseFile(f):
            progs.append(p)
    context = lib.Context(progs)
    input = context.executeExpression(lib.parseExpression(args.input)[0])
    last_name = progs[-1].name
    print args.output(context.executeProgram(last_name, input))
