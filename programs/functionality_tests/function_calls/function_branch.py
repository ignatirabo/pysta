def branching(x):
    if x > 0:
        return 1
    else:
        return 0

x = int(input())
y = branching(x) + 10
print(y)
