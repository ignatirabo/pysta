# Chained loops inside function.
# The function returns the max between z and w.
# There the value of m1 and m2 should be the same if the analyzer is well implemented.
def max(z,w):
    i = 0
    while (i < z):
        x = x + 1
        i = i + 1
    while (i < w):
        x = x + 1
        i = i + 1
    return i

w = 0
z = 2
m1 = max(z,w)
m2 = max(z,w)
times2max = m1 + m2 
print(m1)

