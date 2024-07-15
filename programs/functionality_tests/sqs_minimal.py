# def lambda_handler(event, context):
#     url = "whatever"
#     data_json = event
#     req = urllib.request.Request(url, data_json, headers={'content-type': 'application/json'})
#     return "Message sent to EC2 server successfully!"

# event = input()
# lambda_handler(event, "something")
url = "whatever"
data_json = input()
req = urllib.request.Request(url, data_json, {'content-type': 'application/json'})
x = "This line should not execute"
