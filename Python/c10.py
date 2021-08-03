from c9 import pkcs7_padding
from c6 import byte_lists_xor, decode_base64
from c7 import AES_ecb_encrypt, AES_ecb_decrypt, ch_to_ord


def AES_cbc_encrypt(plain_text, key, iv):
    '''
    Encrypt a plain text with AES 128
    in Cipher Block Chaining (CBC) mode.
    Args:
       plain_text (list) : sequence of bytes
       key (list) : sequence of bytes
       iv (list) : sequence of bytes
    returns:
       encrypted (list) : sequence of bytes
    '''
    encrypted = []
    blocksize = 16
    plain_text = pkcs7_padding(plain_text, blocksize)

    for i in range(0, len(plain_text), blocksize):
        to_encrypt = byte_lists_xor(plain_text[i:i+blocksize], iv)
        iv = AES_ecb_encrypt(to_encrypt, key)
        encrypted.extend(iv)
    return encrypted


def AES_cbc_decrypt(encrypted_text, key, iv):
    '''
    Decrypt an encrypted text with AES 128
    in Cipher Block Chaining (CBC) mode.
    Args:
       encrypted_text (list) : sequence of bytes
       key (list) : sequence of bytes
       iv (list) : sequence of bytes
    returns:
       encrypted (list) : sequence of bytes
    '''
    decrypted = []
    blocksize = 16
        
    for i in range(0, len(encrypted_text), blocksize):
        to_decrypt = encrypted_text[i:i+blocksize]
        decrypted_xor = byte_lists_xor(AES_ecb_decrypt(to_decrypt, key), iv)
        iv = to_decrypt
        decrypted.extend(decrypted_xor)
    return decrypted


def pkcs7_unpadding(byte_list, blocksize):
    '''
    Unpad a sequence of 8 bit integers
    with pkcs#7 method according to
    rfc5652 and rfc2315.
    pad = k - (l % k), where
    k is the block size and l
    is the length of the text.
    args:
        byte_list - (list of 8 bit integers)
        blocksize - (int)
    returns:
        (list of 8 bit integers)
    '''
    pad = byte_list[-1]
    for n in byte_list[-pad:]:
        if pad != n:
            return byte_list
    return byte_list[:-pad]


# import os
# key = ch_to_ord("YELLOW SUBMARINE")
# plain_text = ch_to_ord("Hello World")
# iv = os.urandom(16)
# encrypted_text = AES_cbc_encrypt(plain_text, key, iv)
# print(encrypted_text)
# decrypted_text = AES_cbc_decrypt(encrypted_text, key, iv)
# print(pkcs7_unpadding(decrypted_text, 16))


# with open('../text_files/10.txt', mode='r') as f:
#     encrypted_text = decode_base64(f.read().split())

# key = ch_to_ord("YELLOW SUBMARINE")
# iv = [0]*16
# decrypted_text = AES_cbc_decrypt(encrypted_text, key, iv)

# for ch in pkcs7_unpadding(decrypted_text, 16):
#     print(chr(ch), end='')
# print()

