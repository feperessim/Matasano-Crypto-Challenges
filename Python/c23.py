from c21 import MT19937


class Untemper():
    def __init__(self):
        self.n = 624
        self.u = 11
        self.d = 0xFFFFFFFF
        self.s = 7
        self.b = 0x9D2C5680
        self.t = 15
        self.c = 0xEFC60000
        self.l = 18

    def untemper(self, x):
        '''
        Reverse of the function
        extract_number of MT19937.
        args:
            x - (int)
        returns
            a list with the truncated lists
        '''
        y = x ^ (x >> self.l)
        y = y ^ ((y << self.t) & self.c)
        tmp = y ^ ((y << self.s) & self.b)
        tmp = y ^ ((tmp << self.s) & self.b)
        tmp = y ^ ((tmp << self.s) & self.b)
        y = y ^ ((tmp << self.s) & self.b)
        tmp = y ^ (y >> self.u)
        y = y ^ (tmp >> self.u)
        return y


if __name__ == '__main__':
    n = 624
    seed = 47
    random = MT19937(seed=seed)
    untemper = Untemper()
    random_numbers = [random.extract_number() for _ in range(n)]
    state_array = [untemper.untemper(x) for x in random_numbers]
    random_clone = MT19937(seed=seed)
    random_clone.MT = state_array
    random_clone.index = 0
    random_numbers_clone = [random_clone.extract_number() for _ in range(n)]
    assert random_numbers == random_numbers_clone
