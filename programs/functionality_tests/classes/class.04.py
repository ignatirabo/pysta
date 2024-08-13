# Define a class
class MyClass:
    a = 1
    b = 2
    c = 3
    h = 10
    def g(self):
        return self.a

# Extra instruction to check if we reach this point
x = 1
y = MyClass()
x = 2
y.a = 20
x = 3
z = y.g()
x = 4
