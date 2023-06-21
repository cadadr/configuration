import sys, os


# This will print tuples in python 2, but who cares about python 2?
print("Interpreter at:", sys.executable)
print("Working directory:", os.getcwd())


def dumpsesh(outfile):
    "Dump session history to OUTFILE."
    import readline
    readline.write_history_file(outfile)
