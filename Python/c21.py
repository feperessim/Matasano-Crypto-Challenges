class MT19937():
    '''
    This class implement the Mersenne Twister PRNG
    from Wikipedia's pseudocode.
    '''
    def __init__(self, seed=None, sixty_four_bits=False):
        if seed is None:
            self.seed = 5489
        else:
            self.seed = seed
        if not sixty_four_bits:
            self.w = 32
            self.n = 624
            self.m = 397
            self.r = 31
            self.a = 0x9908B0DF
            self.u = 11
            self.d = 0xFFFFFFFF
            self.s = 7
            self.b = 0x9D2C5680
            self.t = 15
            self.c = 0xEFC60000
            self.l = 18
            self.f = 1812433253
        else:
            self.w = 64
            self.n = 312
            self.m = 156
            self.r = 31
            self.a = 0xB5026F5AA96619E9
            self.u = 29
            self.d = 0x5555555555555555
            self.s = 17
            self.b = 0x71D67FFFEDA60000
            self.t = 37
            self.c = 0xFFF7EEE000000000
            self.l = 43
            self.f = 6364136223846793005

        self.MT = [0] * self.n
        self.index = self.n + 1
        self.lower_mask = (1 << self.r) - 1
        self.upper_mask = ((1 << self.w) - 1) - self.lower_mask
        self.seed_mt()

        
    def seed_mt(self):
        self.MT[0] = self.seed

        for i in range(1, self.n):
            self.MT[i] = ((1 << self.w) - 1) & (self.f * (self.MT[i-1] ^ (self.MT[i-1] >> (self.w-2))) + i)
            

    def extract_number(self):
        if self.index >= self.n:
            self.twist()
        y = self.MT[self.index]
        y = y ^ ((y >> self.u) & self.d)
        y = y ^ ((y << self.s) & self.b)
        y = y ^ ((y << self.t) & self.c)
        y = y ^ (y >> self.l)
        self.index += 1
        return ((1 << self.w) - 1) & y
    

    def twist(self):
        for i in range(self.n):
            x = (self.MT[i] & self.upper_mask) + (self.MT[(i+1) % self.n] & self.lower_mask)
            xA = x >> 1
            if x % 2 != 0:
                xA ^= self.a
            self.MT[i] = self.MT[(i + self.m) % self.n] ^ xA
        self.index = 0


def main():
    random = MT19937(seed=47)
    for i in range(10):
        print(random.extract_number())
    print()
    random = MT19937(seed=47, sixty_four_bits=True)
    for i in range(10):
        print(random.extract_number())


if __name__ == '__main__':
    main()
