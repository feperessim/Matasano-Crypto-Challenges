import random
from c6 import decode_base64
from c9 import pkcs7_padding
from c10 import pkcs7_unpadding
from c11 import gen_rand_aes_key, detect_block_cipher_mode
from c12 import AES_ecb_encrypt, find_blocksize


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
    plain_text = random_prefix + plain_text + secret
    plain_text = pkcs7_padding(plain_text, 16)
    encrypted = AES_ecb_encrypt(plain_text, key)
    return encrypted


def byte_at_a_time_ecb_simple():
    '''
    Break encrypted text in AES ecb mode
    Args:
        None
    returns:
        (list of 8 bit integers)
    '''
    blocksize = find_blocksize()
    prefix_size = find_prefix_size(blocksize)
    prefix_complete_block = [ord('A')]*(blocksize-prefix_size)
    key = gen_rand_aes_key()
    recovered = []
    n_blocks = len(encryption_oracle_ecb([], key)) // blocksize

    for i in range(n_blocks):
        start = i*blocksize + blocksize
        end = start + blocksize
        for k in range(1, blocksize+1):
            input_block = prefix_complete_block + [ord('A')]*(blocksize - k)
            encrypted = encryption_oracle_ecb(input_block, key)[start:end]
            composed_input = input_block + recovered + [0]
            for j in range(256):
                composed_input[-1] = j
                encrypted_composed = encryption_oracle_ecb(composed_input, key)
                if encrypted_composed[start:end] == encrypted:
                    recovered.append(j)
                    break
    return pkcs7_unpadding(recovered, blocksize)


def find_prefix_size(blocksize):
    '''
    Detects the size prefix of encryptation oracle
    and returns how much it was padded
    Args:
        encrypted (list) : sequence of bytes
    returns:
        mode (int) - blocksize - size of prefix
    '''
    key = gen_rand_aes_key()
    previous_enc = encryption_oracle_ecb([], key)
    count = 0

    for i in range(1, 17):
        plain_text = [ord('A')]*i
        encrypted = encryption_oracle_ecb(plain_text, key)[:blocksize]
        if encrypted[:blocksize] != previous_enc[:blocksize]:
            count += 1
            previous_enc = encrypted
        else:
            break
    return blocksize - count


# tests
random_prefix = gen_rand_aes_key()[:random.randint(1, 16)]
tmp = encryption_oracle_ecb(list(map(ord, ['A']*64)), gen_rand_aes_key())
assert detect_block_cipher_mode(tmp) == 1
recovered = byte_at_a_time_ecb_simple()
decrypted = ''.join(list(map(chr, recovered)))
print(decrypted)

