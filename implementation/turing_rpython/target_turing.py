import os, py, sys

import turing_lang, turing

def entry_point(argv = None):
    try:
        filename, input = argv[1], argv[2:]
        contents = ""
        f = os.open(filename, os.O_RDONLY, 0777)
        while True:
            read = os.read(f, 4096)
            if len(read) == 0:
                break
            contents += read
        os.close(f)
        transitions, endstates = turing_lang.build_information(contents)
        print transitions, endstates
        tm = turing.TuringMachine(transitions, endstates)
        tm.run_on(input)
        print tm.state

    except IndexError:
        print "Must give a file to execute and input"
    except BaseException, e:
        print "Weird things happening: %s" % e
    finally:
        return 0

def target(*args):
    return entry_point, None
