# For some reason I cannot start from h1 to evaluate
def h0(event):
    target_policies: int = event['policy_names']
    b = 0
    if target_policies > 0:
        b = 1
    return b

def h1(event):
    target_policies: int = event['policy_names']
    b = 0
    if target_policies > 0:
        b = 1
    return b

def h2(event):
    target_policies: str = event['policy_names']
    b = 0
    if target_policies > 0:
        b = 1
    return b

def h3(event):
    target_policies: list[int] = event['policy_names']
    b = 0
    if target_policies > 0:
        b = 1
    return b
