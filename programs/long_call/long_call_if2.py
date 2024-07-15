
def up2(x):
    if x > 0:
        return input()
    else:
        return input()
            
def up1(x):
    if x > 0:
        return up2(input())
    else:
        return up2(input())
            
def up0(x):
    if x > 0:
        return up1(input())
    else:
        return up1(input())
            
def down0(x):
    down1(x)
            
def down1(x):
    down2(x)
            
def down2(x):
    print(x)
            
x = up0(input())
down0(x)
