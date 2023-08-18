#!/usr/bin/env python 
import matplotlib.pyplot as plt
import numpy as np
from skimage.io import imread
from skimage import img_as_ubyte

from skimage.util import random_noise



def task_5_example1(do_savefig = False):
    coins = imread("data/hand.png")
    bc    = np.bincount(coins.ravel(), minlength = 256)
    cdf   = np.cumsum(bc) / bc.sum()
    mask1 = (coins > 50) & (coins < 90)
    mask2 = (coins > 105) & (coins < 120)
    mask3 = (coins > 185) & (coins < 205)
    th1 = mask1
    th2 = mask1 | mask2
    th3 = mask1 | mask2 | mask3

    xs = range(256)

    fig, axes = plt.subplots(2, 3, figsize = (9, 6))
    ax1, ax2, ax3, ax4, ax5, ax6 = axes.ravel()

    ax1.imshow(coins, cmap = "gray")

    ax2b = ax2.twinx()
    ax2.bar(xs, bc)
    ax2b.plot(xs, cdf, color = "red")


    ax3.imshow(th1, cmap = "gray")
    ax4.imshow(th2, cmap = "gray")
    ax5.imshow(th3, cmap = "gray")
    ax6.axis("off")

    ax1.set_title("hand.tiff")
    ax2.set_title("Histogram/CDF")
    ax3.set_title("Thresholding band at 50..90.")
    ax4.set_title("Thresholding bands at\n50..90 and 105..120.")
    ax5.set_title("Thresholding bands at 50..90,\n105..120, and 185..205.")

    fig.tight_layout()
    fig.show()
    if do_savefig:
        fig.savefig("figures/task_5_1_example1.png")


def task_5_example2(do_savefig = False):
    coins  = imread("data/coins.png")
    coins2 = img_as_ubyte(random_noise(coins, mode = "gaussian"))

    bc  = np.bincount(coins.ravel(), minlength = 256)
    bc2 = np.bincount(coins2.ravel(), minlength = 256)
    bc2[0] = bc2[-1] = 0

    cdf  = np.cumsum(bc)  / bc.sum()
    cdf2 = np.cumsum(bc2) / bc2.sum()

    th  = coins  > 95
    th2 = coins2 > 125

    xs = range(256)

    fig, [[ax1, ax2, ax_foo],
          [ax3, ax4, ax_bar]] = plt.subplots(2, 3, figsize = (12, 8))
    ax2b = ax2.twinx()
    ax4b = ax4.twinx()

    ax1.imshow(coins, cmap = "gray")

    ax2.bar(xs, bc)
    ax2b.plot(cdf, color = "red")

    ax3.imshow(coins2, cmap = "gray")
    ax4.bar(xs, bc2)
    ax4b.plot(cdf2, color = "red")

    ax1.set_title("Original img")
    ax2.set_title("Histogram/CDF (original img)")

    ax3.set_title("Noisy img (Gaussian noise)")
    ax4.set_title("Histogram/CDF (noisy img)")

    ax_foo.imshow(th, cmap = "gray")
    ax_foo.set_title("Original img thresholded at 100")
    ax_bar.imshow(th2, cmap = "gray")
    ax_bar.set_title("Noisy img thresholded at 125")

    fig.tight_layout()
    fig.show()
    if do_savefig:
        fig.savefig("figures/task_5_1_example2.png")

