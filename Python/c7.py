from c6 import decode_base64

s_box = [
    0x63 ,0x7c ,0x77 ,0x7b ,0xf2 ,0x6b ,0x6f ,0xc5 ,0x30 ,0x01 ,0x67 ,0x2b ,0xfe ,0xd7 ,0xab ,0x76,
    0xca ,0x82 ,0xc9 ,0x7d ,0xfa ,0x59 ,0x47 ,0xf0 ,0xad ,0xd4 ,0xa2 ,0xaf ,0x9c ,0xa4 ,0x72 ,0xc0,
    0xb7 ,0xfd ,0x93 ,0x26 ,0x36 ,0x3f ,0xf7 ,0xcc ,0x34 ,0xa5 ,0xe5 ,0xf1 ,0x71 ,0xd8 ,0x31 ,0x15,
    0x04 ,0xc7 ,0x23 ,0xc3 ,0x18 ,0x96 ,0x05 ,0x9a ,0x07 ,0x12 ,0x80 ,0xe2 ,0xeb ,0x27 ,0xb2 ,0x75,
    0x09 ,0x83 ,0x2c ,0x1a ,0x1b ,0x6e ,0x5a ,0xa0 ,0x52 ,0x3b ,0xd6 ,0xb3 ,0x29 ,0xe3 ,0x2f ,0x84,
    0x53 ,0xd1 ,0x00 ,0xed ,0x20 ,0xfc ,0xb1 ,0x5b ,0x6a ,0xcb ,0xbe ,0x39 ,0x4a ,0x4c ,0x58 ,0xcf,
    0xd0 ,0xef ,0xaa ,0xfb ,0x43 ,0x4d ,0x33 ,0x85 ,0x45 ,0xf9 ,0x02 ,0x7f ,0x50 ,0x3c ,0x9f ,0xa8,
    0x51 ,0xa3 ,0x40 ,0x8f ,0x92 ,0x9d ,0x38 ,0xf5 ,0xbc ,0xb6 ,0xda ,0x21 ,0x10 ,0xff ,0xf3 ,0xd2,
    0xcd ,0x0c ,0x13 ,0xec ,0x5f ,0x97 ,0x44 ,0x17 ,0xc4 ,0xa7 ,0x7e ,0x3d ,0x64 ,0x5d ,0x19 ,0x73,
    0x60 ,0x81 ,0x4f ,0xdc ,0x22 ,0x2a ,0x90 ,0x88 ,0x46 ,0xee ,0xb8 ,0x14 ,0xde ,0x5e ,0x0b ,0xdb,
    0xe0 ,0x32 ,0x3a ,0x0a ,0x49 ,0x06 ,0x24 ,0x5c ,0xc2 ,0xd3 ,0xac ,0x62 ,0x91 ,0x95 ,0xe4 ,0x79,
    0xe7 ,0xc8 ,0x37 ,0x6d ,0x8d ,0xd5 ,0x4e ,0xa9 ,0x6c ,0x56 ,0xf4 ,0xea ,0x65 ,0x7a ,0xae ,0x08,
    0xba ,0x78 ,0x25 ,0x2e ,0x1c ,0xa6 ,0xb4 ,0xc6 ,0xe8 ,0xdd ,0x74 ,0x1f ,0x4b ,0xbd ,0x8b ,0x8a,
    0x70 ,0x3e ,0xb5 ,0x66 ,0x48 ,0x03 ,0xf6 ,0x0e ,0x61 ,0x35 ,0x57 ,0xb9 ,0x86 ,0xc1 ,0x1d ,0x9e,
    0xe1 ,0xf8 ,0x98 ,0x11 ,0x69 ,0xd9 ,0x8e ,0x94 ,0x9b ,0x1e ,0x87 ,0xe9 ,0xce ,0x55 ,0x28 ,0xdf,
    0x8c ,0xa1 ,0x89 ,0x0d ,0xbf ,0xe6 ,0x42 ,0x68 ,0x41 ,0x99 ,0x2d ,0x0f ,0xb0 ,0x54 ,0xbb ,0x16
]

s_box_inv = [
    0x52, 0x09, 0x6A, 0xD5, 0x30, 0x36, 0xA5, 0x38, 0xBF, 0x40, 0xA3, 0x9E, 0x81, 0xF3, 0xD7, 0xFB,
    0x7C, 0xE3, 0x39, 0x82, 0x9B, 0x2F, 0xFF, 0x87, 0x34, 0x8E, 0x43, 0x44, 0xC4, 0xDE, 0xE9, 0xCB,
    0x54, 0x7B, 0x94, 0x32, 0xA6, 0xC2, 0x23, 0x3D, 0xEE, 0x4C, 0x95, 0x0B, 0x42, 0xFA, 0xC3, 0x4E,
    0x08, 0x2E, 0xA1, 0x66, 0x28, 0xD9, 0x24, 0xB2, 0x76, 0x5B, 0xA2, 0x49, 0x6D, 0x8B, 0xD1, 0x25,
    0x72, 0xF8, 0xF6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xD4, 0xA4, 0x5C, 0xCC, 0x5D, 0x65, 0xB6, 0x92,
    0x6C, 0x70, 0x48, 0x50, 0xFD, 0xED, 0xB9, 0xDA, 0x5E, 0x15, 0x46, 0x57, 0xA7, 0x8D, 0x9D, 0x84,
    0x90, 0xD8, 0xAB, 0x00, 0x8C, 0xBC, 0xD3, 0x0A, 0xF7, 0xE4, 0x58, 0x05, 0xB8, 0xB3, 0x45, 0x06,
    0xD0, 0x2C, 0x1E, 0x8F, 0xCA, 0x3F, 0x0F, 0x02, 0xC1, 0xAF, 0xBD, 0x03, 0x01, 0x13, 0x8A, 0x6B,
    0x3A, 0x91, 0x11, 0x41, 0x4F, 0x67, 0xDC, 0xEA, 0x97, 0xF2, 0xCF, 0xCE, 0xF0, 0xB4, 0xE6, 0x73,
    0x96, 0xAC, 0x74, 0x22, 0xE7, 0xAD, 0x35, 0x85, 0xE2, 0xF9, 0x37, 0xE8, 0x1C, 0x75, 0xDF, 0x6E,
    0x47, 0xF1, 0x1A, 0x71, 0x1D, 0x29, 0xC5, 0x89, 0x6F, 0xB7, 0x62, 0x0E, 0xAA, 0x18, 0xBE, 0x1B,
    0xFC, 0x56, 0x3E, 0x4B, 0xC6, 0xD2, 0x79, 0x20, 0x9A, 0xDB, 0xC0, 0xFE, 0x78, 0xCD, 0x5A, 0xF4,
    0x1F, 0xDD, 0xA8, 0x33, 0x88, 0x07, 0xC7, 0x31, 0xB1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xEC, 0x5F,
    0x60, 0x51, 0x7F, 0xA9, 0x19, 0xB5, 0x4A, 0x0D, 0x2D, 0xE5, 0x7A, 0x9F, 0x93, 0xC9, 0x9C, 0xEF,
    0xA0, 0xE0, 0x3B, 0x4D, 0xAE, 0x2A, 0xF5, 0xB0, 0xC8, 0xEB, 0xBB, 0x3C, 0x83, 0x53, 0x99, 0x61,
    0x17, 0x2B, 0x04, 0x7E, 0xBA, 0x77, 0xD6, 0x26, 0xE1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0C, 0x7D
]

rcon = [0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1B, 0x36]

nb = 4 # nb - The number of columns comprising a state in AES. This is a constant
nk = 4 # nk - The number of columns of the Cipher Key is equal to the key length * 8 divided by 32.
nr = 10 # nr - The number of rounds is nk + 6 e.g 128 bit - 10; 196 - 12 e 256 - 14


def hexstr_to_bytes(hexstr):
    '''
    Convert a string of hexdecimal values
    to a list of integers.
    args:
        hexstr (str)
    returns:
        (list)
    '''
    return [int(hexstr[i:i+2], 16) for i in range(0, len(hexstr), 2)]


def ch_to_ord(string):
    '''
    Converts all characteters in a string
    to a list of integers
    args:
        string (str)
    returns:
        (list)
    '''
    return [ord(ch) for ch in string]


def rot_bytes(word):
    '''
    Performs left circular shift in a word
    E.g rot_bytes([1, 2, 3, 4]) = [2, 3, 4, 1]
    args:
        word (list) : list of 4 bytes
    returns:
        (list)
    '''
    return word[1:] + [word[0]]


def sub_bytes(state):
    '''
    Non-linear  byte  substitution 
    using  a  substitution  table  (S-box)
    args:
        state (list) - 8 bit integer list
    returns:
        (list)
    '''
    return [s_box[b] for b in state]


def sub_bytes_inv(state):
    '''
    Non-linear  byte inverse substitution
    using  a substitution  table  (S-box)
    args:
        state (list) - 8 bit integer list
    returns:
        (list)
    '''
    return [s_box_inv[b] for b in state]


def key_expansion(key):
    '''
    Performs a Key Expansion routine to generate AES key schedule.
    args:
        key (list) : 8 bit integer list
    returns:
        expanded_key (list) : integer list of bytes
    '''
    expanded_key = [0] * nb * (nr + 1) * nb
    temp = [0] * nb
    
    for i in range(len(key)):
        expanded_key[i] = key[i]
        
    for i in range(nk, nb * (nr + 1)):
        temp = expanded_key[(i-1)*nb:(i-1)*nb + nb]
        if i % nk == 0:
            temp = sub_bytes(rot_bytes(temp))
            temp[0] = temp[0] ^ rcon[i // nk]
        elif i % nk == 4:
            temp = sub_bytes(temp)
        j = i * nb
        k = (i - nk) * nb
        expanded_key[j + 0] = expanded_key[k + 0] ^ temp[0]
        expanded_key[j + 1] = expanded_key[k + 1] ^ temp[1]
        expanded_key[j + 2] = expanded_key[k + 2] ^ temp[2]
        expanded_key[j + 3] = expanded_key[k + 3] ^ temp[3]
    return expanded_key


def add_round_key(state, word):
    '''
    Add round key transformation -
    XORs each column of the State
    with a word from the key schedule.
    args:
        state (list) : 8 bit integer list
        word (list) : 8 bit integer list
    returns:
        (list)
    '''
    for i in range(len(word)):
        state[i] = state[i] ^ word[i]
    return state


def add_round_key_inv(state, word):
    '''
    Inverse add round key transformation -
    XORs each column of the State
    with a word from the key schedule.
    args:
        state (list) : 8 bit integer list
        word (list) : 8 bit integer list
    returns:
        (list)
    '''
    return add_round_key(state, word)


def shift_rows(state):
    '''
    Rows of the State are cyclically
    shifted over different offsets.
    args:
        state (list) - 8 bit integer list
    returns:
        (list)
    '''
    for i in range(0, nb):
        state[i::nb] = state[i::nb][i:] + state[i::nb][:i]
    return state


def shift_rows_inv(state):
    '''
    Rows of the State are cyclically
    inversely shifted over different offsets.
    args:
        state (list) - 8 bit integer list
    returns:
        (list)
    '''
    for i in range(0, nb):
        state[i::nb] = state[i::nb][-i:] + state[i::nb][:-i]
    return state


def mix_columns(state):
    '''
    The mix columns transformation operates
    on the state column-by-column, treating each
    column as a four-term polynomial over GF(2^8).
    The columns of state at multiplied by the
    polinomial {03}x^3 + {01}x^2 + {01}x + {02}
    modulo x^4 + 1.
    args:
        state (list) - 8 bit integer list
    returns:
        (list)
    '''
    for i in range(nb):
        a = state[i * nb + 0]
        b = state[i * nb + 1]
        c = state[i * nb + 2]
        d = state[i * nb + 3]
        
        state[i * nb + 0] = gf2_mul(0x02, a) ^ gf2_mul(0x03, b) ^ c ^ d
        state[i * nb + 1] = a ^ gf2_mul(0x02, b) ^ gf2_mul(0x03, c) ^ d
        state[i * nb + 2] = a ^ b ^ gf2_mul(0x02, c) ^ gf2_mul(0x03, d)
        state[i * nb + 3] = gf2_mul(0x03, a) ^ b ^ c ^ gf2_mul(0x02, d)
    return state


def mix_columns_inv(state):
    '''
    The inverse of mix columns transformation
    operates on the state column-by-column,
    treating  each column as a four-term  polynomial over GF(2^8).
    The columns of state at multiplied by the
    polinomial {0b}x^3 + {0d}x^2 + {09}x + {0e}
    modulo x^4 + 1.
    args:
        state (list) - 8 bit integer list
    returns:
        (list)
    '''
    for i in range(nb):
        a = state[i * nb + 0]
        b = state[i * nb + 1]
        c = state[i * nb + 2]
        d = state[i * nb + 3]
        
        state[i * nb + 0] = gf2_mul(0x0E, a) ^ gf2_mul(0x0B, b) ^ gf2_mul(0x0D, c) ^ gf2_mul(0x09, d)
        state[i * nb + 1] = gf2_mul(0x09, a) ^ gf2_mul(0x0E, b) ^ gf2_mul(0x0B, c) ^ gf2_mul(0x0D, d)
        state[i * nb + 2] = gf2_mul(0x0D, a) ^ gf2_mul(0x09, b) ^ gf2_mul(0x0E, c) ^ gf2_mul(0x0B, d)
        state[i * nb + 3] = gf2_mul(0x0B, a) ^ gf2_mul(0x0D, b) ^ gf2_mul(0x09, c) ^ gf2_mul(0x0E, d)
    return state


def gf2_add(a, b):
    '''
    Sum of two numbers in GF(2^8)
    args:
        a (int) : 8 bit integer
        b (int) : 8 bit integer
    returns:
        (int) : 8 bit integer
    '''
    return a ^ b


def gf2_mul(a, b):
    '''
    Multiplication of two numbers in GF(2^8)
    using Peasant Multiplication algorithm
    Reference: https://en.wikipedia.org/wiki/Finite_field_arithmetic
    args:
        a (int) : 8 bit integer
        b (int) : 8 bit integer
    returns:
        (int) : 8 bit integer
    '''
    p = 0
    irreductible = 0x11B
    MASK = 0xFF # necessary to work as it were 8bit int
    
    while (a and b):
        if b & 0x01:
            p = p ^ a
        if a & 0x80:
            a = MASK & ((a << 0x01) ^ irreductible)
        else:
            a = MASK & (a << 0x01)
        b = b >> 0x01
    return p


def AES_encrypt(plain_text, key):
    '''
    Implementation of AES-128bit encrypt
    args:
        plain_text (list) - sequence of bytes
        key (list) - sequence of bytes
    returns:
        state (list) - encrypted plain text
    '''
    state = plain_text
    expanded_key = key_expansion(key)
    add_round_key(state, key)
    
    for i in range(0, nr-1):
        state = sub_bytes(state)
        state = shift_rows(state)
        state = mix_columns(state)
        state = add_round_key(state, expanded_key[(i + 1) * nb**2 : (i + 1) * nb**2 + nb**2])
    state = sub_bytes(state);
    state = shift_rows(state);
    state = add_round_key(state, expanded_key[nr * nb**2 : nr * nb**2 + nb**2]);
    return state


def AES_decrypt(encrypted_text, key):
    '''
    Implementation of AES-128bit decrypt
    args:
        encrypted_text (list) - sequence of bytes
        key (list) - sequence of bytes
    returns:
        state (list) - decrypted encrypted_text
    '''
    state = encrypted_text
    expanded_key = key_expansion(key)

    state = add_round_key_inv(state, expanded_key[nr * nb**2 : nr * nb**2 + nb**2])
    state = shift_rows_inv(state)
    state = sub_bytes_inv(state)

    for i in range(nr-1, 0, -1):
        state = add_round_key_inv(state, expanded_key[i * nb**2 : i * nb**2 + nb**2])
        state = mix_columns_inv(state)
        state = shift_rows_inv(state)
        state = sub_bytes_inv(state)
    add_round_key_inv(state, key)
    return state


def AES_ecb_encrypt(plain_text, key):
    '''
    Encrypt a plain text with AES 128
    in ECB mode.
    Args:
       plain_text (list) : sequence of bytes
       key (list) : sequence of bytes
    returns:
       encrypted (list) : sequence of bytes
    '''
    encrypted = []
    
    for i in range(0, len(plain_text), nb**2):
        encrypted.extend(AES_encrypt(plain_text[i:i+nb**2], key))
    return encrypted


def AES_ecb_decrypt(encrypted_text, key):
    '''
    Decrypt an encrypted text with AES 128
    in ECB mode.
    Args:
       encrypted_text (list) : sequence of bytes
       key (list) : sequence of bytes
    returns:
       decrypted (list) : sequence of bytes
    '''
    decrypted = []
    
    for i in range(0, len(encrypted_text), nb**2):
        decrypted.extend(AES_decrypt(encrypted_text[i:i+nb**2], key))
    return decrypted


# with open('../text_files/7.txt', mode='r') as f:
#     encrypted_text = decode_base64(f.read().split())

# key = ch_to_ord("YELLOW SUBMARINE")
# decrypted_text = AES_ecb_decrypt(encrypted_text, key)

# for ch in decrypted_text:
#     print(chr(ch), end='')
# print()

# import base64

# with open('../text_files/7.txt', mode='r') as f:
#     encrypted_text = list(base64.b64decode(f.read()))

# key = ch_to_ord("YELLOW SUBMARINE")
# decrypted_text = AES_ecb_decrypt(encrypted_text, key)

# for ch in decrypted_text:
#     print(chr(ch), end='')
# print()


