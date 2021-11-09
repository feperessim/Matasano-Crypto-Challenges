import time
import random

from c21 import MT19937


def rnd_MT19937_unix_timestamp_seed():
    seconds = random.randint(40, 1000)
    time.sleep(seconds)
    seed = int(time.time())
    rdn = MT19937(seed)
    seconds = random.randint(40, 1000)
    time.sleep(seconds)
    random_number = rdn.extract_number()
    return random_number, seed


if __name__ == '__main__':
    random_number, real_seed = rnd_MT19937_unix_timestamp_seed()
    print("Generated random number:", random_number)
    print("Real seed", real_seed)
    now = int(time.time())
    recovered_seed = -1

    while now >= 0:
        seed = now
        if random_number == MT19937(seed).extract_number():
            print('Recovered seed', seed)
            break
        now -= 1

    if recovered_seed == -1:
        print('Could not recover the seed.')
