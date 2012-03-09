L, N, R = ('L','N','R')
try:
    import logging

    logger = logging.getLogger('Turing')
except ImportError:
    class Logger:
        def info(*args):
            pass
    logger = Logger()
from turing_lang import Command
class Cell:
    def __init__(self, value = "#"):
        self.value = value

    @classmethod
    def create(cls, v):
        if isinstance(v, cls):
            return cls(v.value)
        else:
            return cls(v)

class TuringMachine:
    def __init__(self, state_transition, end_states = [0],  start = []):
        self.state = 'start'
        self.transitions = state_transition
        self.r_tape = [Cell.create(k) for k in  start]
        self.l_tape = []
        self.end_states = end_states
        self.running = True

    def run_on(self, l):
        self.r_tape = [Cell(k) for k in l]
        self.l_tape = []
        while self.running:
            self.step()

    def transition(self, val, state):
        for e,f in self.transitions:
            if e.value == val and e.state == state:
                return Command(f.value, f.state, f.movement)
        return Command(val, state, N)

    def read(self, cell):
        e = self.transition(cell.value, self.state)
        self.state = e.state
        cell.value = e.value
        return e.movement

    def load_cell_r(self):
        if not self.r_tape:
            cell = Cell()
            self.r_tape.append(cell)
        return self.r_tape[-1]

    def load_cell_l(self):
        if not self.l_tape:
            cell = Cell()
            self.l_tape.append(cell)
        return self.l_tape[-1]

    def move(self, movement):
        if movement == L:
            cell = self.load_cell_l()
            self.l_tape.pop()
            self.r_tape.append(cell)
            return
        elif movement == N:
            return
        elif movement == R:
            cell = self.load_cell_r()
            self.r_tape.pop()
            self.l_tape.append(cell)
            return

    def step(self):
        if not self.running: return

        current_cell = self.load_cell_r()
        old_state, cell_value = self.state, current_cell.value
        movement = self.read(current_cell)
        self.move(movement)
        if self.state in self.end_states:
            self.running = False

if __name__ == '__main__':
    logging.basicConfig()
    d = {
            (1 , 1) : (1, 0, R),
            (1 , 0) : (1, 1, R),
            (0 , 1) : (0, 1, R),
            (0 , 0) : (0, 0, R),
            (-1, 0) : (-1, 2, N),
            (-1, 1) : (-1, 3, N),
            }
    parity_ones = TuringMachine(d, ([2, 3]))
    input = [int(bool(int(e))) for e in raw_input("Parity 1s:\t")]
    parity_ones.run_on(input)
    print parity_ones.state
