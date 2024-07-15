# Does Pysa analyze this program correctly? No, with the right options.

def dict_create(n):
    i = 0
    ls = {}
    while i<n:
        ls[i] = int(input())
        i = i + 1
    return ls

ls = list_create(4)
ls[3] = 0

print(ls[3])

