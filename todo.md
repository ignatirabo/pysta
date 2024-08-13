# Pysta TODOs

We are aiming at making this tool better for the paper submission.

We have a list of aspects we need to take care of.

1. Check all cases that have warnings, and remove warnings.
2. How can we make bigger examples?
    - Multi-file (module) analysis.
    - Proper `import` handling with references in the context.

## 2 - Bigger examples

For item two, I think the key ingredient here is to make a script that takes a Python program, and returns a simplified Python program.
This gives us flexibility because we have to assure that:

1. A program that goes trough the script, must be runnable by Pysta.
2. The script must have a well documented set of features that are being accepted, and transformed, and how these statements are transformed.
3. As Pysta grows, the script is extended to keep the semantics closer to the original program.

This translation script should actually be implemented in OCaml, using the same Python AST library as we already use.
Then, the flow is as follows:

1. Take Python program as input.
2. Transform the program to an AST.
3. The script takes the AST and returns a new, "simplified", AST.
4. The new AST is passed to PyreIR (finally transformed into our representation).
5. The final AST is analyzed.
