def pkcs7_padding(byte_list, blocksize):
    '''
    Pads a sequence of 8 bit integers
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
    pad = blocksize - len(byte_list) % blocksize
    return byte_list + [pad] * pad


# text = "YELLOW SUBMARINE"
# blocksize = 20
# byte_list = [ord(ch) for ch in text]
# padded_message = pkcs7_padding(byte_list, blocksize)
# print(bytes(padded_message))


# import sys
# sys.path.append("/home/felipe/Dropbox/Matasano Crypto Challenge/Set1/Python")
# from c1 import encode_hex
