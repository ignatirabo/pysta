# Program generates violation from GET to eval.

def operate_on_twos(request: django.http.HttpRequest):
    GET = request.GET
    operator = GET["operator"]
    result = eval(f"2 {operator} 2")  # noqa: P204
    
    return result
