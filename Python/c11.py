import random

from collections import Counter
from c6 import break_into_chunks
from c7 import AES_ecb_encrypt
from c9 import pkcs7_padding
from c10 import AES_cbc_encrypt


def gen_rand_aes_key():
    '''
    Generates a random AES key;
    that's just 16 random bytes.
    Args:
        None
    returns:
        (list) : sequence of bytes
    '''
    return [random.randint(0, 255) for _ in range(16)]


def encryption_oracle(plain_text):
    '''
    Encrypts data under an unknown key
    Args:
        plain_text (list) : sequence of bytes
    returns:
        encrypted (list) : sequence of bytes,
        y (int) : mode used 1 - ecb; 0 - cbc
    '''
    key = gen_rand_aes_key()
    n = random.randint(5, 10)
    m = random.randint(5, 10)
    left_bytes = [random.randint(0, 255) for _ in range(n)]
    right_bytes = [random.randint(0, 255) for _ in range(m)]
    plain_text = left_bytes + plain_text + right_bytes

    if random.randint(0, 1) == 1:
        plain_text = pkcs7_padding(plain_text, 16)
        encrypted = AES_ecb_encrypt(plain_text, key)
        y = 1
    else:
        iv = gen_rand_aes_key()
        encrypted = AES_cbc_encrypt(plain_text, key, iv)
        y = 0
    return encrypted, y


def detect_block_cipher_mode(encrypted):
    '''
    Detects the cipher mode (ecb or cbc) of AES
    Args:
        encrypted (list) : sequence of bytes
    returns:
        mode (int) - 0 for cbc or 1 for ecb
    '''
    chunks = break_into_chunks(encrypted, 16)
    return int(len(chunks) - len(set(map(str, chunks))) > 0)


# def tests():
#     y_true = []
#     y_pred = []
#     for _ in range(1000):
#         plain_text = 37*[ord('a')]
#         #for _ in range(random.randint(100,200)):
#         # plain_text += gen_rand_aes_key()
#         encrypted, y = encryption_oracle(plain_text)
#         yhat = detect_block_cipher_mode(encrypted)
#         y_true.append(y)
#         y_pred.append(yhat)
#     acc = 0
#     total = len(y_true)
#     counter = Counter({'tn':0, 'fp':0, 'fn':0, 'tp':0})
#     for i in range(total):
#         acc += int(y_true[i] == y_pred[i])
        
#     print('Accuraccy: ', str(acc/total * 100) + '%')
#     return y_true, y_pred

# y_true, y_pred = tests()

