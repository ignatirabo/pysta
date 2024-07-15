# Simple loop.
# It's potentially interesting simulating that we start the analysis from the value of `x` since it's the sinking value.
# Then the value of `i` is unknown and generates imprecision.
i = input()
x = input()
while (i < 4):
    if (i > 2):
        print(x)
    i = i + 1
