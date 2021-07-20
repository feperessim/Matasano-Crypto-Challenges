from c1 import encode_hex


def byte_list_xor_int(byte_list, key):
    '''
    Xor a list of integers against a single int
    args:
        byte_list - (list of 8 bit integers)
        key - (8 bit integer)
    returns:
        (list of 8 bit integers)
    '''
    return [e ^ key for e in byte_list]


def brute_force_xor(byte_list):
    '''
    Brute force xor.
    Finds the original key that
    the given string was xored.
    Tests all 256 chars
    args:
        byte_list - (list of 8 bit integers)
    returns:
        (key 8 bit integer)
    '''
    # The frequency of the letters of the alphabet in English
    freqs = [0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
             0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
             0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
             0.00978, 0.02360, 0.00150, 0.01974, 0.00074, 0.13591]
    letters = [chr(ch) for ch in range(ord('a'), ord('z') + 1)] + [" "]
    en_freq_map = {letter: freq for letter, freq in zip(letters, freqs)}
    key_score_map = {}

    for key in range(256):
        xored = byte_list_xor_int(byte_list, key)
        xored = ''.join([chr(e) for e in xored]).lower()
        sum_of_freqs = sum([en_freq_map[e] for e in xored if e in letters])
        key_score_map[key] = sum_of_freqs

    max_key_score = max(key_score_map, key=key_score_map.get)
    return max_key_score, key_score_map[max_key_score]


# encstr = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
# byte_list = encode_hex(encstr)
# key, score = brute_force_xor(byte_list)

# s = ''
# for e in byte_list_xor_int(byte_list, key):
#     s += chr(e)

# print("Best Key:\n", key)
# print("Best Score:\n", score)
# print("Message:\n", s)


