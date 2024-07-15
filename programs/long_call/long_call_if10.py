
def up10(x):
    if x > 0:
        return input()
    else:
        return input()
            
def up9(x):
    if x > 0:
        return up10(input())
    else:
        return up10(input())
            
def up8(x):
    if x > 0:
        return up9(input())
    else:
        return up9(input())
            
def up7(x):
    if x > 0:
        return up8(input())
    else:
        return up8(input())
            
def up6(x):
    if x > 0:
        return up7(input())
    else:
        return up7(input())
            
def up5(x):
    if x > 0:
        return up6(input())
    else:
        return up6(input())
            
def up4(x):
    if x > 0:
        return up5(input())
    else:
        return up5(input())
            
def up3(x):
    if x > 0:
        return up4(input())
    else:
        return up4(input())
            
def up2(x):
    if x > 0:
        return up3(input())
    else:
        return up3(input())
            
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
    down3(x)
            
def down3(x):
    down4(x)
            
def down4(x):
    down5(x)
            
def down5(x):
    down6(x)
            
def down6(x):
    down7(x)
            
def down7(x):
    down8(x)
            
def down8(x):
    down9(x)
            
def down9(x):
    down10(x)
            
def down10(x):
    print(x)
            
x = up0(input())
down0(x)
