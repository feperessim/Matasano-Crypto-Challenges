from c1 import encode_hex
from c3 import byte_list_xor_int, brute_force_xor

with open('../text_files/4.txt', 'r') as f:
    lines = f.read().split()

scores = {}
keys = {}

for index, line in enumerate(lines):
    key, score = brute_force_xor(encode_hex(line))
    keys[index] = key
    scores[index] = score

index_max_score = max(scores, key=scores.get)
message = byte_list_xor_int(encode_hex(lines[index_max_score]), keys[index_max_score])
message = ''.join(chr(e) for e in message)

print("Best Key:\n", keys[index_max_score])
print("Best Score:\n", scores[index_max_score])
print("String:\n", lines[index_max_score])
print("Message:\n", message)
    
