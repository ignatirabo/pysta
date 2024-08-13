def handler(event):
    x : list[int] = event['list']
    ls = event['list']
    ls[0] = 3
    x[0] = ls[0]
    if x[0] == 2:
        print(x[0])
    else:
        print("Error")

event = input()
handler(event)
