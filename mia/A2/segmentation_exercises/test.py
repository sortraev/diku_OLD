import numpy as np

from utils        import *
from thresholding import *
from ccd          import *
from morphing     import *

img  = np.load("lungct_data/lungCTSlice.npy")
mask = np.load("lungct_data/lungMask.npy")

lo = img.min()
hi = img.max()
f = lambda x: (x - lo) / (hi - lo)

__original = img.copy()
img_normalized = normalize(img)


th_opt  = 0.25
seq_opt = [0, 1, 1, 1, 1, 1, 0, 0, 0, 0]

img_th           = threshold(img_normalized, th_opt, lo_val = 1, hi_val = 0)
img_th_ccd       = ccd(img_th, num_components = 2)
img_th_ccd_morph = sequence_morphs(img_th_ccd, seq_opt)

dice_th           = dice(img_th, mask)
dice_th_ccd       = dice(img_th_ccd, mask)
dice_th_ccd_morph = dice(img_th_ccd_morph, mask)
dice_mask         = dice(mask, mask).round(3)

imshow_many([img_normalized, img_th, img_th_ccd, img_th_ccd_morph, mask],
            titles = ["original image (normalized)",
                      f"thresholding\n(DICE: {round(dice_th, 3)})",
                      f"thresholding + CCD\n(DICE: {round(dice_th_ccd, 3)})",
                      f"thresholding + CCD + erosion/dilation (DICE: {round(dice_th_ccd_morph, 3)})",
                      f"reference mask\n(DICE: {round(dice_mask, 3)})"]
           )
