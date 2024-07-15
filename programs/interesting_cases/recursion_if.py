# In this example function f runs for 10 times trying to find an input that is positive.
# Positive input means that the return value is tainted.
# If x reaches 0, it returns 0. Implies no taint.
def f(x):
    y = input()
    if y > 0:
        return y
    elif x > 0:
        return f(x-1)
    else:
        0

x = f(10)
print(x)
