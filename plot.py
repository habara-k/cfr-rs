import matplotlib.pyplot as plt
import json
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('--log')
args = parser.parse_args()

data = json.loads(args.log)

plt.plot(
    list(map(int, data.keys())),
    data.values())
plt.xscale('log')
plt.yscale('log')
plt.xlabel('step')
plt.ylabel('nashconv')
plt.grid()
plt.show()
