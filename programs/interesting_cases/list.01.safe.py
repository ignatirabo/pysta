# Does Pysa analyze this program correctly? No, with the right options.

def foo():
    return [int(input()), int(input()),int(input()), 3]

ls = foo()

print(ls[3])

