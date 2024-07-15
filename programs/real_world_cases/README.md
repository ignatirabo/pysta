In this directory there are examples based on real-life scenarios:
- `cpython_ex0.py`
- `vulnerable_lambda.py`
- `sqs_lambda.py`

### CPython - Working example

In [CVE-2018-1000802](https://www.cvedetails.com/cve/CVE-2018-1000802/) a vulneravility was found in [CPython](https://github.com/python/cpython) 2.7. CPython is the official implementation of the Python interpreter.

In the following [commit](https://github.com/python/cpython/pull/8985/commits/add531a1e55b0a739b0f42582f1c9747e5649ace), the issue was closed. The solution is to replace `spawn` and the exception `DistutilsExecError`, for `subprocess.check_call(cmd)` and `subprocess.CalledProcessError`.

Old code:
```Python
def _call_external_zip(base_dir, zip_filename, verbose=False, dry_run=False):
	...
	try:
		spawn(["zip", zipoptions, zip_filename, base_dir], dry_run=dry_run)
	except DistutilsExecError: ...
```

New code:
```Python
def _call_external_zip(base_dir, zip_filename, verbose, dry_run, logger):
	...
	cmd = ["zip", zipoptions, zip_filename, base_dir]
	if logger is not None:
		logger.info(' '.join(cmd))
	if dry_run:
		return
	import subprocess
	try:
		subprocess.check_call(cmd)
	except subprocess.CalledProcessError: ...
```

This CVE generates an issue in Pysa.

### Vulnerable lambda
[Cheatsheet](https://github.com/RhinoSecurityLabs/cloudgoat/blob/master/scenarios/vulnerable_lambda/cheat_sheet.md)

The idea in this scenario is that there is an AWS Lambda that is *vulnerable*. We can do SQL injection with a payload, in order to give administrator access to our user Bilbo.

The source is the payload, the handler input.

The main sink is `iam.client.attach_user_policy`. From what I read, this function is a very important function and should always be treated carefully.
Another sink, the one we made work is `db.query`.

### SQS FLAG Shop
Scenario requires to buy a Flag from the shop.
Here the Python code is a lambda function as in [[Vulnerable lambda]].
Also there is flask code (python)

After looking at lambda:
- When charging the cash, a message is sent to the SQS service.  
- The lambda function does not verify the received message.  
- The message format is `{"charge_amount" : cash}`

