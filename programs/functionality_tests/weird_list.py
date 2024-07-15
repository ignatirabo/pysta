def handler(event):
    x : list[int] = event['list']
    ls = event['list']
    ls[1] = 3
    x[1] = list[1]
    if x[1] == 2:
        print(x[1])
    else:
        print("Error")
