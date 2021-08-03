from c6 import decode_base64
# from c7 import AES_ecb_encrypt
from c9 import pkcs7_padding
from c10 import pkcs7_unpadding
from c11 import gen_rand_aes_key, detect_block_cipher_mode
from Crypto.Cipher import AES

# My AES implementation makes this code too slow
# then I used pycryptodome lib to do the job
# now it runs fast
def AES_ecb_encrypt(plain_text, key):
    cipher = AES.new(bytes(key), AES.MODE_ECB)
    return list(cipher.encrypt(bytes(plain_text)))

def encryption_oracle_ecb(plain_text, key):
    '''
    Encrypts data under an unknown key
    Args:
        plain_text (list) : sequence of bytes
        key (list) : sequence of bytes
    returns:
        encrypted (list) : sequence of bytes,
    '''
    secret = ('Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg'
              'aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq'
              'dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg'
              'YnkK')
    secret = decode_base64([secret])
    plain_text = plain_text + secret
    plain_text = pkcs7_padding(plain_text, 16)
    encrypted = AES_ecb_encrypt(plain_text, key)
    return encrypted


def find_block_size():
    '''
    Figure out the blocksize of encrytion
    function is using.
    Args:
        None
    returns:
        blocksize (int)
    '''
    plain_text = []
    key = gen_rand_aes_key()
    length_encrypted = len(encryption_oracle_ecb(plain_text, key))
    blocksize = 0
    
    for _ in range(1, 65):
        plain_text += [0]
        encrypted = encryption_oracle_ecb(plain_text, key)
        if length_encrypted != len(encrypted):
            blocksize = len(encrypted) - length_encrypted
            break
    return blocksize


def byte_at_a_time_ecb_simple():
    '''
    Break encrypted text in AES ecb mode
    Args:
        None
    returns:
        (list of 8 bit integers)
    '''
    blocksize = find_block_size()
    key = gen_rand_aes_key()
    recovered = []
    n_blocks = len(encryption_oracle_ecb([], key)) // blocksize
    length = len(encryption_oracle_ecb([], key))

    for i in range(n_blocks):
        start = i*blocksize
        end = start + blocksize
        for k in range(1, blocksize+1):
            input_block = [ord('A')]*(blocksize - k)
            encrypted = encryption_oracle_ecb(input_block, key)[start:end]
            composed_input = input_block + recovered + [0]
            for j in range(256):
                composed_input[-1] = j
                encrypted_composed = encryption_oracle_ecb(composed_input, key)
                if encrypted_composed[start:end] == encrypted:
                    recovered.append(j)
                    break
    return pkcs7_unpadding(recovered, blocksize)


# Detecting cipher mode:
tmp = encryption_oracle_ecb(list(map(ord, ['A']*32)), gen_rand_aes_key())
assert detect_block_cipher_mode(tmp) == 1
recovered = byte_at_a_time_ecb_simple()
decrypted = ''.join(list(map(chr, recovered)))
print(decrypted)

