# Take argument x from cli arg
import argparse
# Create the parser
parser = argparse.ArgumentParser(description="Generate up functions")
# Add the arguments
parser.add_argument('length', type=int, help='The number of up functions to generate')
# Parse the arguments
args = parser.parse_args()

def generate_function_strings(n, return_value):
    function_strings = []
    # For loop with range starting from n to 0
    # creating all the up functions
    for i in range(n, -1, -1):
        if i != n:
            function_strings.append(f"""
def up{i}():
    return up{i+1}()
            """)
        else:
            function_strings.append(f"""
def up{i}():
    return {return_value}
            """)
    # Now we create down functions
    for i in range(0, n+1):
        if i != n:
            function_strings.append(f"""
def down{i}(x):
    down{i+1}(x)
            """)
        else:
            function_strings.append(f"""
def down{i}(x):
    print(x)
            """)
    # Append call to up0 and down0
    function_strings.append(f"""
x = up0()
down0(x)
""")
    return function_strings


# Generate function strings
length = args.length
function_strings = generate_function_strings(length, "input()")

# Write function strings to file
with open(f'programs/long_call/long_call{length}.py', 'w') as f:
    for function_string in function_strings:
        f.write(function_string)