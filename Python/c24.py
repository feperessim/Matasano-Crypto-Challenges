import random
import time

from c21 import MT19937


MAX_SEED = 2**16 - 1


def keystream_generator(seed, length):
    prng = MT19937(seed=seed)
    return [prng.extract_number() for _ in range(length)]


def stream_cipher_mt19937_enc(keystream, plain_text):
    return [keystream[i] ^ plain_text[i] for i in range(len(plain_text))]


def stream_cipher_mt19937_dec(keystream, encrypted_text):
    return stream_cipher_mt19937_enc(keystream, encrypted_text)


def brute_force_stream_cipher_mt19937(plain_text, encrypted_text):
    encrypted_text_len = len(encrypted_text)
    
    for seed in range(MAX_SEED):
        keystream = keystream_generator(seed, encrypted_text_len)
        decrypted_text = stream_cipher_mt19937_dec(keystream, encrypted_text)
        if plain_text == decrypted_text:
            return seed
    return None


def gen_token(length=16):
    return keystream_generator(int(time.time()) & MAX_SEED, length)


def is_generated_with_mt19937(token):
    length = len(token)

    for seed in range(MAX_SEED):
        if keystream_generator(seed, length) == token:
            return True
    return False


def main():
    plain_text = [random.randint(0, 255) for _ in range(10)] + 14 * [ord('A')]
    keystream = keystream_generator(seed=47, length=len(plain_text))
    encrypted_text = stream_cipher_mt19937_enc(keystream, plain_text)
    seed = brute_force_stream_cipher_mt19937(plain_text, encrypted_text)
    token = gen_token()
    is_mt19937 = is_generated_with_mt19937(token)

    print('Recovered seed: ', seed)
    print('Was token generated using mt19937: ', is_mt19937)


if __name__ == '__main__':
    main()
