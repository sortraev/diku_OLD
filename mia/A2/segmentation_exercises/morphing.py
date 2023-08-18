import numpy as np
from scipy.signal import convolve, convolve2d

from utils import *
from thresholding import threshold

def erode(img, radius = 1):
    assert np.all((img == 0) | (img == 1))
    kernel = np.ones((2 * radius + 1, ) * img.ndim)
    retval = convolve2d(img, kernel, mode = "same", method = "direct") >= kernel.size
    return retval

def dilate(img, radius = 1):
    kernel = np.ones((2 * radius + 1, ) * img.ndim)
    return convolve2d(img, kernel, mode = "same", method = "direct") > 0

KERNEL_2D_GRAD_X = np.float32([[1, 0, -1],
                               [2, 0, -2],
                               [1, 0, -1]])
KERNEL_2D_GRAD_Y = KERNEL_2D_GRAD_X.T

def grad_mag_edge_detection(vol):
    grad_x = convolve2d(vol, KERNEL_2D_GRAD_X, mode = "same")
    grad_y = convolve2d(vol, KERNEL_2D_GRAD_Y, mode = "same")
    return np.sqrt(grad_x ** 2 + grad_y ** 2)

def erosion_edge_detection(img):
    return img - erode(img)

def sequence_morphs(img, seq):
    assert np.all(np.isin(seq, [0, 1])) # assert well-formed sequence.
    out = img.copy()
    for op in seq:
        out = (dilate if op else erode)(out)
    return out

def align_pixels(img, ps, PAD = 1):
    img_pad = np.pad(img, pad_width = PAD, mode = "edge")
    for i, j in np.ndindex(img.shape):
        if img[i, j] not in ps:
            area  = img_pad[i+1-PAD : i+PAD+2, j+1-PAD : j+PAD+2]
            count = np.bincount(area[np.isin(area, ps)])
            if count.size > 0: img[i, j] = np.argmax(count)
    return img

import numpy as np
from itertools import permutations
def gen_sequences(n = 5):
    combinations = [(es + 1) * [0] + (ds + 1) * [1]
                    for es, ds in [*np.ndindex(n, n)]]
    c_sets = [set(permutations(c)) for c in combinations]
    return [list(c) for c_set in c_sets for c in c_set]

def find_optimal_sequence(img, ref_mask, sequences = None):
    if sequences is None:
        print("find_optimal_sequence(): no morph sequence given -- generating ..")
        sequences = gen_sequences(5)

    print("searching for optimal morph sequence.")
    res = np.array([dice(sequence_morphs(img, sequence), ref_mask)
                    for sequence in sequences])

    print("done!")
    argmax = np.argmax(res)
    #  argsort = np.flip(np.argsort(res))
    argsort = np.argsort(res)[::-1]
    return res, argsort


#  if __name__ == "__main__":
#      from thresholding import *
#      from utils import *
#      from ccd import *
#      img  = np.load("lungct_data/lungCTSlice.npy")
#      mask = np.load("lungct_data/lungMask.npy")
#
#      img_th_ccd = ccd(img_th, k = 2)
#      img_th_ccd_morph = [0, 1, 1, 1, 1, 1, 0, 0, 0, 0] # found using find_optimal_sequence().
#      imshow_many([img, img_th, img_th_ccd, img_th_ccd_morph, mask])
#
#
