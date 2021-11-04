import random

from c6 import decode_base64
from c11 import gen_rand_aes_key
from c17 import ord_to_ch
from c18 import AES_ctr_encrypt


with open('../text_files/19.txt', mode='r') as f:
    lines = map(lambda line: decode_base64([line]), f.read().splitlines())
    lines = list(lines)

key = gen_rand_aes_key()
nonce = 0
encrypted_lines = map(lambda line: AES_ctr_encrypt(line, key, nonce), lines)
encrypted_lines = list(encrypted_lines)

print("\n==================")
print("Plain text lines:")
print("==================\n")
for plain_text in list(map(ord_to_ch, lines)):
    print(plain_text)

print("\n==================")
print("Encrypted lines:")
print("==================\n")
for plain_text in list(map(ord_to_ch, encrypted_lines)):
    print(plain_text)
print()


while True:
    user_input = input("Input -1 to exit or 1 to starting Guessing: ")
    if user_input == "-1":
        break
    elif user_input != "1":
        print("Wrong input, try again")
    else:
        print("Choosing a random encrypted String:")
        index = random.randint(0, len(encrypted_lines) - 1)
        random_enc_str = encrypted_lines[index]
        keystream = []
        print(''.join(ord_to_ch(random_enc_str)))
        print()
        random_enc_str_copy = random_enc_str.copy()
        string_length = len(random_enc_str)
        index = 0
       
        while True:
            if index >= string_length:
                print("You reached the end of the string")
                break
            print("Choose an interger in [0-255] interval to guess, -1 to exit", end="")
            print(" or -2 to show the current guessed keystream\n")
            user_input = input()
            if user_input == "-1":
                break
            elif user_input == "-2":
                print("Current guessed keystream")
                print(keystream)
                print()
            elif user_input.isdigit():
                n = int(user_input)
                if n < 0 or n > 255:
                    print("Error: Value out of [0-255]")
                else:
                    random_enc_str_copy[index] = random_enc_str[index] ^ n
                    print("Index:", index)
                    print("before:", ''.join(ord_to_ch(random_enc_str)))
                    print("after: ", ''.join(ord_to_ch(random_enc_str_copy)))
                    print()
                    while True:
                        user_input = input("Input -1 to try again or -2 keep the guess: ")
                        if user_input == "-1":
                            keystream.append(n)
                            index += 1
                            break
                        elif user_input == "-2":
                            break
                        else:
                            print("Wrong input, try again")
            else:
                 print("Wrong input, try again")

