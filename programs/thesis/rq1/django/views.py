# from django.http import HttpRequest, HttpResponse

def operate_on_twos(request: django.http.HttpRequest) -> django.http.HttpResponse:
    operator = request.GET["operator"]
    r = abs(request.GET["range"]) + 1

    if range == 0:
        result = 0
    else:
        result = eval(f"(2 {operator} 2) * {r}")

    return result
