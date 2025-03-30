import os
import sys

print("hello_world")
assert os.path.exists(sys.argv[0])
for p in sys.argv:
  print(p, flush=True)