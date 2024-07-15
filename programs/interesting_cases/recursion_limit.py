# Showing deep recursion.
# First argument decides how deep to go.
def f(j,x):
    if j <= 0:
        return x
    x = int(input()) + x
    return f(j-1,x)

x = f(2000,0)
print(x)
