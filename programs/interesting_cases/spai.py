def update_nth(ls,i):
    if i < len(ls):
        ls[i] = int(input())
    return ls

ls = [0,1,2,3,4]
update_nth(ls,0)
print(ls[1])

