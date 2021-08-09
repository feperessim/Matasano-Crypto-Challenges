from c7 import AES_ecb_encrypt, AES_ecb_decrypt
from c9 import pkcs7_padding
from c10 import pkcs7_unpadding
from c11 import gen_rand_aes_key


def kv_parser(cookie):
    '''
    Parse a structured cookie
    Args:
        cookie (str) : string
    returns:
        parsed (dict) : dictionary
    '''
    parsed = dict(map(lambda pair: pair.split("="), cookie.split("&")))
    return parsed


def profile_for(email):
    '''
    Parse e-mail and create a cookie
    Args:
        email (str) : string
    returns:
        encoded (str) : dictionary
    '''
    email = email.replace('&', '').replace('=', '')
    encoded = 'email=' + email + '&uid=10&role=user'
    return encoded


def encrypt_user_profile(user_profile, key):
    '''
    Encrypt a user profile given the user e-mail
    Args:
        user_profile (str) : string
        key (list) : sequence of bytes
    returns:
        encrypted (list) : sequence of bytes
    '''
    user_profile_bytes = pkcs7_padding([ord(char) for char in user_profile], 16)
    encrypted = AES_ecb_encrypt(user_profile_bytes, key)
    return encrypted


def decrypt_user_profile(encrypted, key):
    '''
    Decrypts a user profile and parse it
    Args:
        encrypted (list) : sequence of bytes
        key (list) : sequence of bytes
    returns:
        parsed (dict) : dictionary
    '''
    decrypted = pkcs7_unpadding(AES_ecb_decrypt(encrypted, key), 16)
    parsed = kv_parser(''.join([chr(byte) for byte in decrypted]))
    return parsed


def profile_for_oracle(email, key):
    '''
    Parse e-mail, create a cookie
    and encrypt it.
    Args:
        email (str) : string
    returns:
        encoded (str) : dictionary
    '''
    user_profile = profile_for(email)
    encrypted = encrypt_user_profile(user_profile, key)
    return encrypted


def ecb_cut_and_paste_attack():
    email = 'oo@bar.comaaa'
    email_to_trick = 'oo@bar.com' + 'admin' + '\x0b' * 11
    key = gen_rand_aes_key()
    encrypted = profile_for_oracle(email, key)
    encrypted_admin = profile_for_oracle(email_to_trick, key)
    encrypted_trick = encrypted[0:32] + encrypted_admin[16:32]

    return decrypt_user_profile(encrypted_trick, key)

# tests
# cookie = 'foo=bar&baz=qux&zap=zazzle'
# cookie_parsed = kv_parser(cookie)
# # user_profile = profile_for(email)
# # encrypted = encrypt_user_profile(user_profile, key)
# # decrypted_parsed = decrypt_user_profile(encrypted, key)
# # x = profile_for('foo@bar.comaa')
# x = profile_for('oo@bar.comaaa')
# x = profile_for('oo@bar.com' + 'admin' + '\x0b' * 11 )
# a = [x[i*16:i*16+16] for i in range(len(x))]
# for i in range(0, len(x), 16):
#     print(x[i:i+16])

# print(ecb_cut_and_paste_attack())
