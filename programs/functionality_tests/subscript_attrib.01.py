class MyClass:
    d = { 'a' : 1 , 'b' : 2 }

x = MyClass()
d = x.d
a = d['a']
b = x.d['a']
f = { 'a' : 1 , 'b' : 2 }
c = f['b']
print(a)
