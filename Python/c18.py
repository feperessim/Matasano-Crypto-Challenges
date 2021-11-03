from c6 import decode_base64
from c7 import AES_ecb_encrypt, ch_to_ord


def AES_ctr_encrypt(plain_text, key, nonce):
    '''
    Encrypt a plain text with AES 128
    in CTR - The  Counter mode.
    Args:
       plain_text (list) : sequence of bytes
       key (list) : sequence of bytes
       nonce (int) : integer
    returns:
       encrypted (list) : sequence of bytes
    '''
    blocksize = 16
    counter = 0
    encrypted = []
    nonce_bytes = nonce.to_bytes(length=8, byteorder='little')
    
    for i in range(0, len(plain_text), blocksize):
        block = nonce_bytes + counter.to_bytes(length=8, byteorder='little')
        enc = AES_ecb_encrypt(list(block), key)
        enc_block = plain_text[i:i+blocksize]
        enc = [enc[j] ^ enc_block[j] for j in range(len(enc_block))]
        encrypted.extend(enc)
        counter += 1
    return encrypted


def AES_ctr_decrypt(encrypted_text, key, nonce):
    '''
    Decrypt an encrypted text with AES 128
    in in CTR - The  Counter mode.
    Args:
       encrypted_text (list) : sequence of bytes
       key (list) : sequence of bytes
       iv (list) : sequence of bytes
    returns:
       decrypted (list) : sequence of bytes
    '''
    return AES_ctr_encrypt(encrypted_text, key, nonce)


# string = "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="
# encrypted_text = ch_to_ord(string)
# encrypted_text = decode_base64([string])
# key = ch_to_ord("YELLOW SUBMARINE")
# nonce = 0
# decrypted = AES_ctr_decrypt(encrypted_text, key, nonce)
# decrypted = ''.join(map(lambda x: chr(x), decrypted))
# print(decrypted)
