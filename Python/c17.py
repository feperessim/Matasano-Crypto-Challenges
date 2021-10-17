import random

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
           "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g="
           "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"]

key = gen_rand_aes_key()


def CBC_padding_oracle_encrypt(strings):
    '''
    Encrypts a random string with AES in CBC mode
    Args:
        plain_text (list) : sequence of bytes
    returns:
        encrypted (list) : sequence of bytes
        iv (list) : sequence of bytes
    '''
    random_index = random.randint(0, 9)
    iv = gen_rand_aes_key()
    plain_text = ch_to_ord(strings[random_index])
    encrypted = AES_cbc_encrypt(plain_text, key, iv)

    return (encrypted, iv)


def CBC_padding_oracle_decrypt(encrypted_text, iv):
    '''
    Decrypt an encrypted text with AES 128
    in Cipher Block Chaining (CBC) mode.
    Args:
       encrypted_text (list) : sequence of bytes
       iv (list) : sequence of bytes
    returns:
       decrypted (list) : sequence of bytes
    '''
    blocksize = 16
    decrypted = AES_cbc_decrypt(encrypted_text, key, iv)
    unpadded = pkcs7_unpadding(decrypted, blocksize)

    return unpadded

