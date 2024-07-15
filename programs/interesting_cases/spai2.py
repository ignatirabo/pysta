# Program does not generates violation from GET to eval.
# This program will never be able to enter the true body of the if statement.
# Start analysis in line 12.

# We redefine built-in functions (limitation of tool).
def abs(x):
    if x < 0:
        return x * (-1)
    else:
        return x

def operate_on_twos(request: django.http.HttpRequest):
	operator = request.GET["operator"]
	range = abs(request.GET["range"]) + 1 
        
	if range==0: 
		result = eval(f"2 {operator} 2")  # noqa: P204
	else: 
		result=0

	return result
