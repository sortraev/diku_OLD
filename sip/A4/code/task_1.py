#!/usr/bin/env -S ipython3 -i
import numpy as np
from scipy.signal import convolve, correlate
from skimage.io import imread

from imshow_many import imshow_many


def task_1_1():
    kernel_x = np.array([[0, 0, 0], [-1/2, 0, 1/2], [0, 0, 0]])
    kernel_y = np.array([[0, -1/2, 0], [0, 0, 0], [0, 1/2, 0]])

    img = imread("test_images/trui.png")

    derv_x  = convolve(img, np.flip(kernel_x))
    derv_y  = convolve(img, np.flip(kernel_y))
    derv_x2 = correlate(img, kernel_x)
    derv_y2 = correlate(img, kernel_y)
    imshow_many([img,
                 derv_x,
                 derv_x2,
                 None,
                 derv_y,
                 derv_y2
                ],
                ["original",
                 "convolution x",
                 "correlation x",
                 "",
                 "convolution y",
                 "correlation y"
                ]
               )
