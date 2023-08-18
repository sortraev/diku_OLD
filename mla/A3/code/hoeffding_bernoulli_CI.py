import sys
import numpy as np
import matplotlib.pyplot as plt

confidence = float(sys.argv[1]) if len(sys.argv) > 1 else 0.97

S = np.loadtxt("S.csv")
n = len(S)
S_bar = np.mean(S)

delta = 1 - confidence
dev = np.sqrt(np.log(1 / (delta / 2)) / (2 * n))

upper_bound = S_bar + dev
lower_bound = S_bar - dev

print(f"mu in [{lower_bound.round(3)}, {upper_bound.round(3)}] "
      f"with probability {confidence}.")
