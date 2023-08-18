#!/usr/bin/env python 
import numpy as np
import matplotlib.pyplot as plt

from skimage.io import imread
from skimage.util import img_as_float
from skimage.feature import peak_local_max
from scipy.ndimage import gaussian_filter
import os


def harris_response(img, sigma, alpha = 0.05, k = 1):

    img = img_as_float(img)

    Lx  = gaussian_filter(img, sigma, order = [1, 0])
    Ly  = gaussian_filter(img, sigma, order = [0, 1])

    Axx = gaussian_filter(Lx ** 2, k * sigma)
    Axy = gaussian_filter(Lx * Ly, k * sigma)
    Ayy = gaussian_filter(Ly ** 2, k * sigma)

    det_A   = Axx * Ayy - Axy ** 2
    trace_A = Axx + Ayy
    return sigma ** 4 * (det_A - alpha * trace_A ** 2)

def multi_scale_harris_response(img, sigmas, alpha = 0.05, k = 1):
    return np.array([harris_response(img, sigma, alpha, k)
                     for sigma in sigmas])

def multi_scale_corner_detection(img,
                                 sigmas = np.logspace(0, 5, 30, base = 2),
                                 num_peaks = 350,
                                 alpha = 0.05,
                                 k = 1):
    responses = multi_scale_harris_response(img, sigmas, alpha, k)
    coords = peak_local_max(responses, num_peaks = num_peaks).astype(float)
    coords[:, 0] = sigmas[coords[:, 0].astype(int)]
    return coords


def run_multi(img_filepath,
              sigma = None,
              alpha = 0.05,
              k = 1,
              num_peaks = 350,
             ):
    a, b, scale_levels = (0, 5, 30) if sigma is None else sigma

    img_filename = img_filepath.split('/')[-1]
    file_out = img_filename + f"_{alpha=}_{k=}_{a=}_{b=}_{scale_levels=}.npy"
    if os.path.isfile(file_out):
        print(f"loading {file_out}")
        peaks = np.load(file_out)
    else:
        print(f"{file_out} does not exist. (re-)running and saving ..")
        img    = imread(img_filepath)
        sigmas = np.logspace(a, b, scale_levels, base = 2)
        peaks  = multi_scale_corner_detection(img,
                                              sigmas = sigmas,
                                              alpha = alpha,
                                              k = k,
                                              num_peaks = num_peaks,
                                             )
        np.save(file_out, peaks)
    return peaks


def task_4(do_savefig = False):
    alphas = [0.005, 0.01, 0.05, 0.1, 0.5, 1]
    k = 1.5

    all_results = [run_multi("data/modelhouses.png",
                             sigma = (0, 5, 30),
                             k = 1.5,
                             alpha = alpha)
                   for alpha in alphas]

    img = imread("data/modelhouses.png")
    fig, axes = plt.subplots(2, 3, figsize = (12, 8))

    for alpha, peaks, ax in zip(alphas, all_results, axes.flat):
        ax.imshow(img)
        ax.set_title(r"$\alpha$ = " + f"{alpha}", fontsize = 20)
        for sigma, y, x in peaks:
            circle = plt.Circle((x, y),
                                radius = 4 * sigma ** 0.8,
                                #  radius = 2 * sigma,
                                color = "red",
                                fill = False)
            ax.add_artist(circle)

    fig.tight_layout()
    if do_savefig:
        fig.savefig("figures/task_4.png")
