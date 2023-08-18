#!/usr/bin/env -S ipython3 -i
import matplotlib.pyplot as plt
if "science" in plt.style.available: plt.style.use("science")

import numpy as np
from skimage.io import imread
from skimage.color import rgb2gray
from skimage import img_as_ubyte

import task_3
from imshow_many import imshow_many

def threshold(img, th):
    return np.uint8(img > th)

def task_4_1(do_savefig = True):
    img_rgb = imread("test_images/pillsetc.png")
    img     = rgb2gray(img_rgb)
    th = 100
    img_th  = threshold(img, th / 256)

    imshow_many([img_rgb, img, img_th],
                ["original image",
                 "grayscaled image",
                 "thresholded at 100"],
                png_out = "figures/task_4_1.png" if do_savefig else None,
                figsize = (12, 8))

    return img_th

def task_4_2(do_savefig = True):
    img    = rgb2gray(imread("test_images/pillsetc.png"))

    hist = task_3.img_hist(img)
    cdf  = task_3.img_cdf(img)

    # PLOT SETUP
    fig, ax = plt.subplots(1, 1, figsize = (6, 4))
    ax.set_ylabel("Frequency", fontsize = 16)
    ax.set_xlabel("Pixel value", fontsize = 16)
    ax.grid()

    ax2 = ax.twinx()
    ax2.axis("off")

    # PLOTTING
    xs = range(256)
    ax.bar(xs, hist, label = "pillsetc.png histogram")
    ax2.plot(xs, cdf, color = "red", alpha = 0.5)

    plt.show()

    fig.tight_layout()
    if do_savefig:
        fig.savefig("figures/task_4_2.png", bbox_inches = "tight")

if __name__ == "__main__":
    pass
    task_4_1(do_savefig = False)
    #  task_4_2(do_savefig = False)
