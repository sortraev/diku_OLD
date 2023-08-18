import numpy as np

def threshold(img,
              lo_th,
              lo_val,
              hi_th  = None,
              hi_val = None):

    img = np.float64(img)
    below = img <= lo_th

    if hi_val is not None:
        if hi_th is not None:
            img[img >= hi_th] = hi_val
        else:
            img[~below] = hi_val

    img[below] = lo_val
    return img

def find_optimal_threshold(img, ref_mask):
    # assumes img is normalized.

    dice_max, dice_argmax = (None, None)

    step = 0.05
    thresholds = np.arange(0, 1 + step, step)

    res = np.array([dice(threshold(img, t, lo_val = 1, hi_val = 0), ref_mask)
                    for t in thresholds])

    argmax = np.argmax(res)
    return res[argmax], thresholds[argmax]


if __name__ == "__main__":
    from utils import *
    img  = np.load("lungct_data/lungCTSlice.npy")
    mask = np.load("lungct_data/lungMask.npy")

    img = normalize(img)

    dice_opt, th_opt = find_optimal_threshold(img, mask)
    print(dice_opt, t_opt)
    img_th = threshold(img, th_opt, lo_val = 1, hi_val = 0)
    imshow_many([img, img_th, mask])
