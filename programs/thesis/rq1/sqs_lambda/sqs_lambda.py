# We believe urllib.request.urlopen is the sink.
# Source is obviously event.
# According to scenario "The lambda function does not verify the received message."

import json
import os
import urllib.request


def lambda_handler(event, context):
    print("event : ", event)
    # url = os.environ['web_url'] # THIS LINE IS PROBLEMATIC BECAUSE THERE IS NO REFERENCE TO OS
    url = "whatever"
    print("url : ", url)

    temp : list[int] = event["Records"]
    temp = temp[0]
    charge_cash = temp["body"]
    charge_cash = json.loads(charge_cash)

    # charge_cash["charge_amount"] is an int and int > 0 => true
    # if charge_cash["charge_amount"]:
    if charge_cash["charge_amount"] >= 0:
    #    try:
        charge_cash['auth'] = "auth"
        # charge_cash['auth'] = os.environ['auth'] # THIS LINE IS PROBLEMATIC BECAUSE THERE IS NO REFERENCE TO OS
        charge_cash['sqs_request'] = pop(charge_cash,'charge_amount')
        # charge_cash['sqs_request'] = charge_cash.pop('charge_amount') # THIS LINE IS PROBLEMATIC BECAUSE POP IS A METHOD OF DICT
        data_json = json.dumps(charge_cash) # I SPLITTED THIS LINE IN TWO.
        data_json = encode(data_json, 'utf-8')
        # data_json = data_json.encode('utf-8') # SAME HERE.
        req = urllib.request.Request(url, data_json, headers={'content-type': 'application/json'})
        res = urllib.request.urlopen(req)
        return "Message sent to EC2 server successfully!"

    #     # except Exception as e:
    #     #     return "Error sending request to EC2 server"
    else:
        return "another request"

event = input()
lambda_handler(event, "something")
