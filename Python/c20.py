from c3 import brute_force_xor
from c6 import decode_base64, transpose_blocks, byte_lists_xor
from c11 import gen_rand_aes_key
from c17 import ord_to_ch
from c18 import AES_ctr_encrypt


def truncate_min_length(list_of_lists):
    '''
    Truncates lists of elements in
    a list with the minimum length
    among these lists.
    elements
    args:
        list_of_lists - (list)
    returns
        a list with the truncated lists
    '''
    min_length = min(map(len, list_of_lists))
    return list(map(lambda l: l[:min_length], list_of_lists))


def brute_force_xor_lines(encrypted_lines):
    '''
    Brute force repeating-key-xor,
    on a string encrypted with
    AES in CTR mode using a fixed nonce.
    Finds the original keystream that
    the given string was xored.
    Returns the original keystream xored
    against the plain text.
    args:
        encrypted_lines - (list of byte lists)
    returns:
        (list of int)
    '''
    t_lines = truncate_min_length(encrypted_lines)
    transposed_lines = transpose_blocks(t_lines, len(t_lines[0]))
    keystream = []

    for block in transposed_lines:
        keystream.append(brute_force_xor(block)[0])
    return keystream


with open('../text_files/20.txt', mode='r') as f:
    lines = map(lambda line: decode_base64([line]), f.read().splitlines())
    lines = list(lines)

key = gen_rand_aes_key()
nonce = 0
encrypted_lines = map(lambda line: AES_ctr_encrypt(line, key, nonce), lines)
encrypted_lines = list(encrypted_lines)
keystream = brute_force_xor_lines(encrypted_lines)

print("Keystream: ", keystream, "\n")
print("\n==================")
print("Plain text lines:")
print("==================\n")


for enc in encrypted_lines:
    dec = byte_lists_xor(keystream, enc)
    print(''.join(ord_to_ch(dec)))
