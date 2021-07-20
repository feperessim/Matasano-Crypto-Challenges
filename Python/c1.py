def encode_hex(hexstr):
    '''
    Convert a string of hexdecimal values
    to a list of integers.
    args:
        hexstr - (str)
    returns:
        (list)
    '''
    return [int(hexstr[i:i+2], 16) for i in range(0, len(hexstr), 2)]


def encode_base64(hex_byte_list):
    '''
    Convert a list of raw bytes (hexdecimal values)
    to base 64 string.
    args:
        hexstr - (list of 8 bit integers)
    returns:
        b64_str - (str)
    '''
    b64_map = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    padding = len(hex_byte_list) % 3
    hex_byte_list = hex_byte_list + [0 for _ in range(3 - padding) if padding > 0]
    b64_str = ''
    
    for i in range(0, len(hex_byte_list), 3):
        b64_str += b64_map[hex_byte_list[i] >> 0x02]
        b64_str += b64_map[((hex_byte_list[i] & 0x03) << 0x04) | (hex_byte_list[i+1] >> 0x04)]
        b64_str += b64_map[((hex_byte_list[i+1] & 0x0f) << 0x02) | (hex_byte_list[i+2] >> 0x06)]
        b64_str += b64_map[hex_byte_list[i+2] & 0x3f]

    for _ in range(padding):
        b64_str += '='

    return b64_str


hex_string = '49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d'

b64_string = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

assert encode_base64(encode_hex(hex_string)) == b64_string

# print(encode_hex(hex_string))
# print(encode_base64(encode_hex(hex_string)))



