import os.path

import matplotlib.pyplot as plt
plt.set_cmap("viridis")
import numpy as np
import scipy as scipy
from skimage.segmentation import random_walker
import skimage.io

# own modules
from utils import *
from morphing import *
#  from thresholding import *
from ccd import ccd
from volume_viewer import volume_viewer

#  np.random.seed(2)
np.random.seed(49)


if __name__ == "__main__":
    img = skimage.io.imread("data/test.png")
    img_gray = img.mean(2)

    seeds1 = np.array([[34, 65], [50, 119], [18, 164], [65, 206], [90, 234],
                       [90, 278], [92, 322], [99, 350], [34, 347], [31, 294],
                       [25, 237], [21, 167], [58, 258], [19, 104], [12,  53],
                       [61, 312]])


    seeds2 = np.array([[200, 40], [336, 120], [317, 61], [251, 74], [243, 30],
                       [117, 65], [121, 122], [97, 81], [113, 186], [69, 18],
                       [12, 15], [127, 35], [95, 150], [135, 160], [119, 19],
                       [305, 89], [271, 28], [147, 76], [95,  33], [178, 62],
                       [169, 18], [273, 58], [17, 13], [274, 97]])


    seeds3 = np.array([[191, 120], [206, 155], [178, 182], [206, 207],
                       [125, 233], [166, 236], [196, 244], [127, 271], [156, 281],
                       [125, 300], [155, 323], [127, 344], [157, 343], [227, 289],
                       [255, 285], [221, 311], [257, 321], [221, 341], [245, 349],
                       [264, 287]])

    seeds4 = np.array([[243, 130], [278, 137], [310, 145], [337, 176],
                       [302, 184], [263, 174], [238, 161], [248, 204], [288, 215],
                       [334, 216], [331, 258], [296, 244], [251, 231], [292, 268],
                       [332, 294], [326, 336], [282, 324], [191, 344], [190, 310],
                       [186, 283], [189, 287], [186, 276]])



    #  seeds1 = np.random.permutation(seeds1)
    #  seeds2 = np.random.permutation(seeds2)
    #  seeds3 = np.random.permutation(seeds3)
    #  seeds4 = np.random.permutation(seeds4)

    k = 4
    _seeds1 = np.random.permutation(seeds1)[:k]
    _seeds2 = np.random.permutation(seeds2)[:k]
    _seeds3 = np.random.permutation(seeds3)[:k]
    #  _seeds4 = np.random.permutation(seeds4)[:k]

    _seeds3 = np.array([[194, 119], [237, 303], [185, 223], [142, 318]])
    _seeds4 = np.array([[312, 309], [310, 256], [293, 199], [254, 138]])
                       #  [334, 216], [331, 258], [296, 244], [251, 231], [292, 268],
                       #  [332, 294], [326, 336], [282, 324], [191, 344], [190, 310],
                       #  [186, 283], [189, 287], [186, 276]])

    lens = [len(x) for x in [seeds1, seeds1, seeds3, seeds4]]
    min_len, max_len = min(lens), max(lens)

    seeds = np.zeros_like(img_gray)
    seeds[seeds1[:, 1], seeds1[:, 0]] = 1
    seeds[seeds2[:, 1], seeds2[:, 0]] = 2
    seeds[seeds3[:, 1], seeds3[:, 0]] = 3
    seeds[seeds4[:, 1], seeds4[:, 0]] = 4

    _seeds = np.zeros_like(img_gray)
    _seeds[_seeds1[:, 1], _seeds1[:, 0]] = 1
    _seeds[_seeds2[:, 1], _seeds2[:, 0]] = 2
    _seeds[_seeds3[:, 1], _seeds3[:, 0]] = 3
    _seeds[_seeds4[:, 1], _seeds4[:, 0]] = 4

    betas = [1, 4]

    def map_colors(img):
        out = np.zeros(img.shape + (3, ))
        out[img == 1] = [1,   0, 0]
        out[img == 2] = [0,   0, 1]
        out[img == 3] = [0, 0.5, 0]
        out[img == 4] = [1,   1, 0]
        return out
        #  out[:, :, 1] = img == 1

    segs  = [map_colors(random_walker(img_gray,  seeds, beta = b, mode = "cg_mg")) for b in betas]
    _segs = [map_colors(random_walker(img_gray, _seeds, beta = b, mode = "cg_mg")) for b in betas]

    titles = ([f"Original image -- full seeding\n({min_len}..{max_len} seeds per segment)"] +
        [f"RW segmentation\n(full seeding, beta = {b})" for b in betas] +
        [f"Original image -- sparse seeding\n({k} seeds per segment)"] +
        [f"RW segmentation\n(sparse seeding, beta = {b})" for b in betas])

    fig, axes = imshow_many([img, *segs, img, *_segs], titles = titles, do_show = False)


    axes[0].scatter(seeds1[:, 0], seeds1[:, 1], color = "red",    marker = 'o', s = 80, label = f"Seed 1")
    axes[0].scatter(seeds2[:, 0], seeds2[:, 1], color = "blue",   marker = 'o', s = 80, label = f"Seed 2")
    axes[0].scatter(seeds3[:, 0], seeds3[:, 1], color = "green",  marker = 'o', s = 80, label = f"Seed 3")
    axes[0].scatter(seeds4[:, 0], seeds4[:, 1], color = "yellow", marker = 'o', s = 80, label = f"Seed 4")
    axes[0].legend(loc = "upper right")

    axes[3].scatter(_seeds1[:, 0], _seeds1[:, 1], color = "red",    marker = 'o', s = 80, label = f"Seed 1")
    axes[3].scatter(_seeds2[:, 0], _seeds2[:, 1], color = "blue",   marker = 'o', s = 80, label = f"Seed 2")
    axes[3].scatter(_seeds3[:, 0], _seeds3[:, 1], color = "green",  marker = 'o', s = 80, label = f"Seed 3")
    axes[3].scatter(_seeds4[:, 0], _seeds4[:, 1], color = "yellow", marker = 'o', s = 80, label = f"Seed 4")
    axes[3].legend(loc = "upper right")

    fig.tight_layout()
    #  fig.show()
    fig.savefig("../tex/figures/RW_segmentation.png", bbox_inches = "tight")
