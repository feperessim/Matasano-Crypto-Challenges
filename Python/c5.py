from c1 import encode_hex


def repeating_xor(message, key):
    '''
    Performs repeating xor in a given string
    with a given key.
    args:
        message - (list of 8 bit int)
        key - (list of 8 bit int)
    returns:
        (list of 8 bit integers)
    '''
    xored_msg = []
    keylen = len(key)

    for i, val in enumerate(message):
        xored_msg.append(val ^ key[i % keylen])
    return xored_msg


message = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
key = "ICE"
encrypted = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
encrypted = encode_hex(encrypted)
message = [ord(ch) for ch in message]
key = [ord(ch) for ch in key]
result = repeating_xor(message, key)

assert encrypted == result
