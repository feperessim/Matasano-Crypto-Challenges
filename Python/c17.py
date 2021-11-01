import random

from c6 import byte_lists_xor, decode_base64
from c7 import ch_to_ord
from c10 import AES_cbc_encrypt, AES_cbc_decrypt
from c11 import gen_rand_aes_key
from c15 import pkcs7_unpadding


strings = ["MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc=",
           "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic=",
           "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw==",
           "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg==",
           "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl",
           "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA==",
           "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw==",
           "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8=",
           "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g=",
           "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"]

key = gen_rand_aes_key()


def cbc_padding_oracle_encrypt():
    '''
    Encrypts a random string with AES in CBC mode
    Args:
    returns:
        encrypted (list) : sequence of bytes
        iv (list) : sequence of bytes
    '''
    random_index = random.randint(0, len(strings) - 1)
    iv = gen_rand_aes_key()
    plain_text = ch_to_ord(strings[random_index])
    encrypted = AES_cbc_encrypt(plain_text, key, iv)

    return (encrypted, iv)


def cbc_padding_oracle_decrypt(encrypted, iv):
    '''
    Decrypt an encrypted text with AES 128
    in Cipher Block Chaining (CBC) mode.
    Args:
       encrypted (list) : sequence of bytes
       iv (list) : sequence of bytes
    returns:
       decrypted (list) : sequence of bytes
    '''
    blocksize = 16
    decrypted = AES_cbc_decrypt(encrypted, key, iv)

    try:
        pkcs7_unpadding(decrypted, blocksize)
        return True
    except Exception:
        return False


def ord_to_ch(byte_list):
    '''
    Converts all bytes in a list
    to a string of chars
    args:
        byte_list : (list)
    returns:
        (str)
    '''
    return ''.join(map(lambda x: chr(x), byte_list))


def cbc_padding_attack():
    '''
    Perfoms the CBC padding attack
    on a randomly select string from
    strings list.
    return the random selected text
    decrypted.
    args:
    returns:
        plain_text : (str)
    '''
    encrypted, iv = cbc_padding_oracle_encrypt()
    blocksize = 16
    i_state = [0] * blocksize
    plain_text = []
    block = gen_rand_aes_key()

    for block_index in range(0, len(encrypted), blocksize):
        padding = 0x01
        next_block = encrypted[block_index: block_index + blocksize]
        block = block[:blocksize] + next_block
        for i in range(blocksize-1, -1, -1):
            for byte in range(256):
                block[i] = byte
                valid_padding = cbc_padding_oracle_decrypt(block, iv)
                if valid_padding:
                    i_state[i] = byte ^ padding
                    padding += 1
                    break
            for k in range(1, padding):
                block[blocksize-k] = i_state[blocksize-k] ^ padding
        plain_text.extend(byte_lists_xor(i_state, iv))
        iv = encrypted[block_index: block_index+blocksize]
    plain_text = pkcs7_unpadding(plain_text, blocksize)
    plain_text = ord_to_ch(decode_base64([ord_to_ch(plain_text)]))

    return plain_text
