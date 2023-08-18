#!/usr/bin/env -S ipython3 -i
import matplotlib.pyplot as plt
if "science" in plt.style.available: plt.style.use("science")

import numpy as np
from skimage.util import img_as_ubyte
from skimage.io import imread


import skimage.exposure
from imshow_many import imshow_many

# task 3.1.
def img_hist(img):
    return np.bincount(img_as_ubyte(img).ravel(), minlength = 256)

def cdf_from_hist(hist):
    return hist.cumsum() / hist.sum()
def img_cdf(img):
    return cdf_from_hist(img_hist(img))

# task 3.2.
def cdf_substitution(img, cdf = None):
    return (cdf if cdf is not None else img_cdf(img))[img]

# task 3.3.
def idf_from_cdf(cdf):
    # ls is either a scalar or a 1D array.
    return lambda ls: (cdf[:, None, None] >= ls).squeeze().argmax(0)

def img_idf(img):
    return idf_from_cdf(img_cdf(img))

def hist_match(img1, img2):
    # matches img1 to img2's cdf.
    return img_idf(img2)(cdf_substitution(img1))


## TASKS

def task_3_1(do_savefig = True):
    pout = img_as_ubyte(imread("test_images/pout.tif"))

    pout_hist = img_hist(pout)
    pout_cdf  = img_cdf(pout)

    # PLOT SETUP
    fig, ax = plt.subplots(1, 1, figsize = (8, 6))
    ax.set_xlabel("Pixel value")
    ax.set_ylabel("Frequency")
    ax.grid()

    # PLOTTING
    xs = range(256)
    ax.bar(xs, pout_hist, alpha = 0.7, label = "pout histogram")

    ax2 = ax.twinx()
    ax2.set_ylabel("CDF")
    ax2.plot(xs, pout_cdf, color = "red", label = "cdf")
    fig.legend()
    fig.show()
    if do_savefig:
        fig.savefig("figures/task_3_1.png")

def task_3_2(do_savefig = True):
    pout = img_as_ubyte(imread("test_images/pout.tif"))
    pout_cdf_reconstruction = cdf_substitution(pout)

    png_out = "figures/task_3_2.png" if do_savefig else None
    imshow_many([pout,
                 pout_cdf_reconstruction,
                ],
                ["pout.tif",
                 "cdf_substitution(pout.tif)",
                ],
                 png_out = png_out,
                 figsize = (6, 6),
               )

def task_3_4(do_savefigs = True):
    pout = imread("test_images/pout.tif")
    trui = imread("test_images/trui.png")

    # HISTOGRAM MATCHING
    match1 = hist_match(pout, trui)
    match2 = skimage.exposure.match_histograms(pout, trui)

    png_out = "figures/task_3_4_matches.png" if do_savefigs else None
    imshow_many([pout, trui, match1, match2],
                ["pout", "trui", "hist_match(pout, trui)",
                 "skimage reference"],
                 figsize = (8, 8),
                 fig_dims = (1, 4),
                 png_out = png_out,
               )

    # HISTOGRAMS
    pout_hist  = img_hist(pout)
    trui_hist  = img_hist(trui)
    match_hist = img_hist(match1)

    pout_cdf   = img_cdf(pout)
    trui_cdf   = img_cdf(trui)
    match_cdf  = img_cdf(match1)

    # PLOT SETUP
    fig, axes = plt.subplots(1, 3, figsize = (12, 4))
    ax1, ax2, ax3 = axes
    for ax in axes:
        ax.set_xlabel("Pixel value", fontsize = 14)
        ax.set_ylabel("Frequency", fontsize = 14)
        ax.grid()

    ax1.set_title("pout.tif histogram", fontsize = 16)
    ax2.set_title("trui.png histogram", fontsize = 16)
    ax3.set_title("hist_match(pout, trui) histogram", fontsize = 16)

    ax4, ax5, ax6 = ax1.twinx(), ax2.twinx(), ax3.twinx()
    for ax in [ax4, ax5, ax6]:
        ax.axis("off")

    # PLOTTING
    xs = range(256)
    ax1.bar(xs, pout_hist)
    ax2.bar(xs, trui_hist)
    ax3.bar(xs, match_hist)

    ax4.plot(xs, pout_cdf,  color = "red", alpha = 0.8)
    ax5.plot(xs, trui_cdf,  color = "red", alpha = 0.8)
    ax6.plot(xs, match_cdf, color = "red", alpha = 0.8)

    fig.tight_layout()
    fig.show()
    if do_savefigs:
        fig.savefig("figures/task_3_4_histograms.png", bbox_inches = "tight")

task_3_4()
