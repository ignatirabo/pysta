# Program generates violation from GET to eval.
# Start analysis in line 4.

def operate_on_twos(request: django.http.HttpRequest):
	operator = request.GET["operator"]
	result = eval(f"2 {operator} 2")  # noqa: P204
    
	return result
