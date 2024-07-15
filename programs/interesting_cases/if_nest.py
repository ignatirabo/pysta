# Simple nested if
i = int(input())
j = int(input())
if (i < 0):
    if (j > 0):
        x = 0
    else:
        x = input()
else:
    x = 1
print(x)
