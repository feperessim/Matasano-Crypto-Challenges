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
    length_message = len(byte_list)
    
    if length_message < pad or length_message % blocksize != 0:
        raise Exception("Bad padding")
    for n in byte_list[-pad:]:
        if pad != n:
            raise Exception("Bad padding")
    return byte_list[:-pad]


# tests
blocksize = 16
text = "ICE ICE BABY\x04\x04\x04\x04"
byte_list = [ord(ch) for ch in text]
print(pkcs7_unpadding(byte_list, blocksize))

text = "ICE ICE BABY\x05\x05\x05\x05"
byte_list = [ord(ch) for ch in text]
print(pkcs7_unpadding(byte_list, blocksize))

text = "ICE ICE BABY\x05\x05\x05\x4D"
byte_list = [ord(ch) for ch in text]
print(pkcs7_unpadding(byte_list, blocksize))

text = "ICE ICE BABY"
byte_list = [ord(ch) for ch in text]
print(pkcs7_unpadding(byte_list, blocksize))
