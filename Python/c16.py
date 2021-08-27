from c10 import AES_cbc_encrypt, AES_cbc_decrypt
from c11 import gen_rand_aes_key
from c15 import pkcs7_unpadding


def encrypt(plain_text, key, iv):
    '''
    Encrypts data with AES in CBC mode
    Args:
        plain_text (list) : sequence of bytes
        key (list) : sequence of bytes
        iv (list) : sequence of bytes
    returns:
        encrypted (list) : sequence of bytes
    '''
    prefix = "comment1=cooking%20MCs;userdata="
    suffix = ";comment2=%20like%20a%20pound%20of%20bacon"
    plain_text = plain_text.translate({ord(";"): None, ord("="): None})
    plain_text = prefix + plain_text + suffix
    plain_text = [ord(char) for char in plain_text]
    encrypted = AES_cbc_encrypt(plain_text, key, iv)

    return encrypted


def decrypt(encrypted, key, iv):
    '''
    Decrypts data with AES in CBC mode
    Args:
        encrypted (list) : sequence of bytes
        key (list) : sequence of bytes
        iv (list) : sequence of bytes
    returns:
        (bool) : True or False
     '''
    blocksize = 16
    decrypted = AES_cbc_decrypt(encrypted, key, iv)
    decrypted = pkcs7_unpadding(decrypted, blocksize)
    decrypted = ''.join([chr(byte) for byte in decrypted])
    return ";admin=true;" in decrypted


def bit_flip_attack():
    '''
    Performs CBC bitflipping attack
    Args:
    returns:
        (bool) : True or False
    '''
    plain_text = 'AadminAtrue'
    key = gen_rand_aes_key()
    iv = gen_rand_aes_key()
    encrypted = encrypt(plain_text, key, iv)
    encrypted[16] = encrypted[16] ^ ord('A') ^ ord(';')
    encrypted[22] = encrypted[22] ^ ord('A') ^ ord('=')
    return decrypt(encrypted, key, iv)
