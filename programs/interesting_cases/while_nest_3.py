# Three nested loops inside function.
# Function int is left unspecified which works as the identity in the current implementation.
def f(z,w):
    i = 0
    x = 0
    while (i < z):
        j = 0
        while (j < w):
            y = int(input())
            while (y < 5):
                x = x + 1
                y = y + 1
            j = j + 1
        i = i + 1
    x = int(input()) + x
    return x

w = 1
z = 2
x = f(z,w)
print(x)

