import numpy as np
from a1 import imshow_many

from matplotlib import pyplot as plt, patches as patches
import matplotlib
matplotlib.rc("image", cmap = "gray")

from skimage.io import imread
import skimage.transform as transform
import skimage.draw

from math import ceil

phantom = imread("logan_phantom.png").mean(axis = 2)

theta0 = np.arange(0, 180)
theta1 = np.arange(0, 90)
theta2 = np.arange(90, 180)

radon0 = transform.radon(phantom, theta = theta0, circle = False)
radon1 = transform.radon(phantom, theta = theta1, circle = False)
radon2 = transform.radon(phantom, theta = theta2, circle = False)

phantom_out_a = transform.iradon(radon0, theta = theta0, circle = False)
phantom1      = transform.iradon(radon1, theta = theta1, circle = False)
phantom2      = transform.iradon(radon2, theta = theta2, circle = False)
phantom_out_b = phantom1 + phantom2

phantom_out_a /= np.max(phantom_out_a)
phantom_out_b /= np.max(phantom_out_b)

diff = phantom_out_a - phantom_out_b
mse = np.mean(diff ** 2)
mse_str = "{:.3e}".format(mse)

print(f"mse of reconstruction: {mse}")

imshow_many([phantom1, phantom2, None, phantom_out_b, phantom_out_a, diff],
            [r"Partial reconstruction $A$, using theta = {0, ..., 89}",
             r"Partial reconstruction $B$, using theta = {90, ..., 179}",
             "",
             r"Sum of partial reconstructions: $A + B$",
             "Reference reconstruction using theta = {0, ..., 179}",
             f"Difference between reconstructions (MSE = {mse_str})"
            ],
            png_out = "tex/figures/fbp_linearity.png"
            )




