
def up10():
    return input()
            
def up9():
    return up10()
            
def up8():
    return up9()
            
def up7():
    return up8()
            
def up6():
    return up7()
            
def up5():
    return up6()
            
def up4():
    return up5()
            
def up3():
    return up4()
            
def up2():
    return up3()
            
def up1():
    return up2()
            
def up0():
    return up1()
            
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

x = up0()
down0(x)
