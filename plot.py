import matplotlib.pyplot as plt
import json
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('--log')
args = parser.parse_args()

data = json.loads(args.log)
split = len(data)
print(split)
step = 10000

plt.plot(
    list(map(int, data.keys())),
    data.values())
plt.xscale('log')
plt.yscale('log')
plt.grid()
plt.show()
