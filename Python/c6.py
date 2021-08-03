from c3 import brute_force_xor
from c5 import repeating_xor


def decode_base64(list_str_b64):
    '''
    Decode strings in list from base 64
    to a list of 8bit integers.
    args:
        list_str_b64 - (list of strings)
    returns:
        decoded_list - (list of lists of 8 bit integers)
    '''
    b64_map = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    decoded = []

    for b64_str in list_str_b64:
        indexes = [b64_map.index(ch) if ch != '=' else 0 for ch in b64_str]
        for i in range(0, len(indexes), 4):
            decoded.append(((indexes[i] & 0x3f) << 0x02) | ((indexes[i+1] & 0x30) >> 0x04))
            decoded.append(((indexes[i+1] & 0x0f) << 0x04) | ((indexes[i+2] & 0x3c) >> 0x02))
            decoded.append(((indexes[i+2] & 0x03) << 0x06) | indexes[i+3])

    if list_str_b64[-1][-1] == '=':
        return decoded[:-1]
    return decoded


def byte_lists_xor(byte_list1, byte_list2):
    '''
    Xorwise two lists of integers of same length.
    args:
        byte_list1 - (list of 8 bit integers)
        byte_list2 - (list of 8 bit integers)
    returns:
        (list of 8 bit integers)
    '''
    return [byte_list1[i] ^ byte_list2[i] for i in range(len(byte_list1))]


def hamming_distance(byte_list1, byte_list2):
    '''
    Compute the Hamming Distance between two
    lists of integers of same length  against
    each other
    args:
        byte_list1 - (list of 8 bit integers)
        byte_list2 - (list of 8 bit integers)
    returns:
        (int)
    '''
    xored = byte_lists_xor(byte_list1, byte_list2)
    return sum([count_bits(e) for e in xored])


def count_bits(val):
    '''
    Counts the number of bits on
    args:
        val - (int)
    returns:
        count (int)
    '''
    count = 0
    while val != 0:
        count += val & 0x01
        val = val >> 0x01
    return count


def break_into_chunks(byte_list, keysize):
    '''
    Break a list of bytes into chunks
    of length keysize
    args:
        byte_list - (list of 8 bit integers)
        keysize - (int)
    returns:
        count (int)
    '''
    length = len(byte_list)
    max_index = length // keysize
    rem = max_index % 2  # it allows to have even pairs of lists
    chunks = [byte_list[i:i+keysize] for i in range(0, length, keysize)]
    return chunks[:max_index - rem]


def avg_norm_hamming_distance(chunks, keysize):
    '''
    Compute the Hamming Distance between every
    two consecutives chunks, normalize them,
    and compute the average among the distances.
    args:
        chunks - (list of lists of 8 bit integers)
        keysize - (int)
    returns:
        (int)
    '''
    distances = [hamming_distance(chunks[i], chunks[i+1])/keysize
                 for i in range(0, len(chunks), 2)]
    return sum(distances)/len(distances)


def find_best_keysize(byte_list):
    '''
    Finds the keysize with best average normalizaed
    hamming distance.
    args:
        byte_list - (list of lists of 8 bit integers)
    returns:
        (int)
    '''
    keysize_dist_map = {}
    
    for keysize in range(2, 41):
        chunks = break_into_chunks(byte_list, keysize)
        keysize_dist_map[keysize] = avg_norm_hamming_distance(chunks, keysize)

    keysize_max_score = min(keysize_dist_map, key=keysize_dist_map.get)
    return keysize_max_score


def transpose_blocks(chunks, keysize):
    '''
    Transpose a square byte_list of size
    keysize^2.
    args:
        chunks - (list of lists of 8 bit integers)
        keysize - (int)
    returns:
         chunks_transposed - (list of lists of 8 bit integers)
    '''
    chunks_transposed = []
    for i in range(keysize):
        transposed = []
        for byte_list in chunks:
            transposed.append(byte_list[i])
        chunks_transposed.append(transposed)
    return chunks_transposed


def brute_force_xor_blocks(encrypted_lines):
    '''
    Brute force repeating-key-xor.
    Finds the original key that
    the given string was xored.
    Returns the original key used
    to encrypt it.
    args:
        encrypted_lines - (list of strings)
    returns:
        (list of int)
    '''
    decoded_encrypted_lines = decode_base64(encrypted_lines)
    keysize = find_best_keysize(decoded_encrypted_lines)
    transposed_chunks = break_into_chunks(decoded_encrypted_lines, keysize)
    transposed_chunks = transpose_blocks(transposed_chunks, keysize)
    key = []
    for chunk in transposed_chunks:
        key.append(brute_force_xor(chunk)[0])
    return key

# Tests
# for i in range(2, 41):
#     chunks = break_into_chunks(decoded_encrypted_lines, i)
#     print(avg_norm_hamming_distance(chunks, i))

# byte_list1 = [ord(ch) for ch in "this is a test"]
# byte_list2 = [ord(ch) for ch in "wokka wokka!!!"]

# dist = hamming_distance(byte_list1, byte_list2)
# print(dist)
# assert dist == 37

# with open('../text_files/6.txt', 'r') as f:
#     encrypted_lines = f.read().split()

# key = brute_force_xor_blocks(encrypted_lines)
# decoded_encrypted_lines = decode_base64(encrypted_lines)
# decrypted = repeating_xor(decoded_encrypted_lines, key)

# print('Key: ', ''.join([chr(ch) for ch in key]))
# print('Decrypted text')
# print(''.join([chr(ch) for ch in decrypted]))
