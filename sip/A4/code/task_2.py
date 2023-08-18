#!/usr/bin/env -S ipython3 -i
import numpy as np

from skimage.io import imread, imsave
from skimage import img_as_float
import scipy.signal
from scipy.signal import convolve

from scipy.fft import next_fast_len, fft2, ifft2, fftshift, ifftshift

from imshow_many import imshow_many

def scale_fft(img, sigma):
    n, m = s = np.asarray(img.shape)
    X, Y = ifftshift(np.indices(s), axes = (1, 2))

    # compute sigma for the frequency domain.
    sigma_X, sigma_Y = s / (2 * np.pi * sigma)
    F_gauss = np.exp(-((X - n // 2) ** 2 / (2 * sigma_X ** 2) +
                       (Y - m // 2) ** 2 / (2 * sigma_Y ** 2)))
    return ifft2(fft2(img) * F_gauss).real


def task_2_1():
    #  sigmas = [2, 3, 5]
    sigmas = [0, 1, 2, 4, 8, 16]

    trui = imread("test_images/trui.png")
    tmp = [(scale_fft(trui, sigma), f"scale_fft with $\sigma$ = {sigma}")
           for sigma in sigmas]

    imgs   = [t[0] for t in tmp]
    titles = [t[1] for t in tmp]

    imshow_many(imgs, titles,
                png_out = "../tex/figures/task_2_1.png",
                )
task_2_1()
