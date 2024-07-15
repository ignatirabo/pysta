# This program is tagged as issue in Pysa since it is returning the object `ob` and collapsing it.

class MyClass:
    a = 1
    b = 1
    c = 1
    h = 10

def foo():
    ob = MyClass()
    ob.a = ob.a + int(input())
    ob.b = ob.b + int(input())
    ob.c = ob.c + int(input())
    return ob

ob = foo()
print(ob.h)

