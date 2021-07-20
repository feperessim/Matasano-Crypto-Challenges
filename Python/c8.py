from c6 import break_into_chunks

with open('../text_files/8.txt', 'r') as f:
    encrypted_lines = f.read().split()

repetitions = {}
for i, line in enumerate(encrypted_lines):
    chunks = break_into_chunks(encrypted_lines[i], 16)
    repetitions[i] = len(set(chunks))

index = min(repetitions, key=repetitions.get)

print('Index:', index)
print("Index and String encrypted with AES in ECB mode:\n")
print(encrypted_lines[index])
