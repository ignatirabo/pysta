def abs(x):
    if x < 0:
        return x * (-1)
    else:
        return x

def operate_on_twos(request):
    operator = request["operator"]
    range = request["range"] + 1

    result = 0
    if range==0 :
        s = concat(operator,2,2)
        result = eval(s)

    return result

request = { "operator" : input() , "range" : int(input()) }
operate_on_twos(request)

