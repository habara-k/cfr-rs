import argparse
import datetime
import json
import os

import matplotlib.pyplot as plt

parser = argparse.ArgumentParser()
parser.add_argument("--log")
args = parser.parse_args()

data = json.loads(args.log)

plt.plot(list(map(int, data.keys())), data.values())
plt.xscale("log")
plt.yscale("log")
plt.xlabel("step")
plt.ylabel("nashconv")
plt.grid()
now = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
os.mkdir("log/{}".format(now))
plt.savefig("log/{}/nash_conv.png".format(now))
