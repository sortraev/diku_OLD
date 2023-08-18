#!/usr/bin/env -S ipython3 -i
import matplotlib.pyplot as plt
if "science" in plt.style.available: plt.style.use("science")

import numpy as np
from skimage.io import imread
from skimage.color import rgb2gray

from scipy.signal import convolve

#  from task_3 import img_hist, img_cdf
import task_4
from imshow_many import imshow_many

FFT_CONVOLVE_EPS = 0.0001
def erode(img, n = 1):
    kern = np.ones((2*n + 1, ) * img.ndim)
    return convolve(img, kern, "same") >= kern.size - FFT_CONVOLVE_EPS

def dilate(img, n = 1):
    kern = np.ones((2*n + 1, ) * img.ndim)
    return convolve(img, kern, "same") > FFT_CONVOLVE_EPS

def task_5_1(do_savefig = True):
    img     = rgb2gray(imread("test_images/pillsetc.png"))
    img_th  = task_4.threshold(img, 100/256)

    img_morph = dilate(erode(img_th))

    png_out = "figures/task_5_1.png" if do_savefig else None
    imshow_many([img_th,
                 img_morph
                ],
                ["thresholded image",
                 "after morphological opening"
                ],
                png_out = png_out,
                figsize = (8, 6)
               )


if __name__ == "__main__":
    task_5_1(False)
