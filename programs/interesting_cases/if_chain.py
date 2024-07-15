# Chain of if statements: each loop is an if statement.
def f(i):
    y = 0
    j = 0
    while (j < i):
        x = input()
        if (x > 5):
            y = y + x
        j = j + 1
    return y

x = f(3)
print(x)
