from c1 import encode_hex

def byte_list_xor(byte_list1, byte_list2):
    '''
    Xor two lists of integers against each other
    args:
        byte_list1 - (list of 8 bit integers)
        byte_list2 - (list of 8 bit integers)
    returns:
        (list of 8 bit integers)
    '''
    return [byte_list1[i] ^ byte_list2[i]
            for i in range(len(byte_list1))]


byte_list1 = encode_hex("1c0111001f010100061a024b53535009181c")
byte_list2 = encode_hex("686974207468652062756c6c277320657965")
result = encode_hex("746865206b696420646f6e277420706c6179")
xored_hex_str = byte_list_xor(byte_list1, byte_list2)

assert result == xored_hex_str
