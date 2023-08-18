#  import cv2
import numpy as np

from matplotlib import pyplot as plt, patches as patches
import matplotlib
matplotlib.rc("image", cmap = "gray")

from skimage.io import imread
import skimage.transform as transform
import skimage.draw

from math import ceil

def imshow_many(imgs,
                titles    = [],
                figsize   = (16, 8),
                fig_title = None,
                do_show   = True,
                png_out   = None):
    n = len(imgs)

    x, y = (2, 2) if n == 4 else (ceil(n / 3), min(n, 3))
    fig, axes = plt.subplots(x, y, figsize = figsize)
    axes_flat = axes.ravel()

    titles += [None] * (n - len(titles))

    for ax, (img, title) in zip(axes_flat, zip(imgs, titles)):
        if img is None:
            ax.axis("off")
        else:
            ax.imshow(img, aspect = img.shape[1] / img.shape[0])
            if title is not None:
                ax.set_title(title)
    if fig_title is not None:
        fig.suptitle(fig_title)

    fig.tight_layout()

    if do_show:
        plt.show()
    if png_out is not None:
        fig.savefig(png_out, bbox_inches = "tight")


def handin_illustration():
    phantom = imread("logan_phantom.png").mean(axis = 2)

    theta1 = np.linspace(0, 180, 20, endpoint = False)
    theta2 = np.linspace(0, 180, max(phantom.shape), endpoint = False)

    filter_name = "ramp"
    png_out     = "tex/figures/no_filter.png"
    print("radon1")
    radon1 = transform.radon(phantom, theta = theta1, circle = False)
    print("radon2")
    radon2 = transform.radon(phantom, theta = theta2, circle = False)

    print("iradon1")
    phantom1 = transform.iradon(radon1, theta = theta1, circle = False, filter_name = filter_name)
    print("iradon2")
    phantom2 = transform.iradon(radon2, theta = theta2, circle = False, filter_name = filter_name)
    phantom3 = transform.iradon(radon2, theta = theta2, circle = False, filter_name = None)

    print("plotting")
    #  imshow_many([phantom, radon1, radon2, None, phantom1, phantom2],
    #      titles = ["Original",
    #                f"{len(theta1)} projection angles in the range [0, 180)",
    #                f"{len(theta2)} projection angles in the range [0, 180)",
    #
    #                f"reconstruction of above with one backprojection at 0 degrees",
    #                f"reconstruction of above with {len(theta1)} backprojection\nangles in the range [0, 180)",
    #                f"reconstruction of above with {len(theta2)} backprojection\nangles in the range [0, 180)"
    #               ],
    #      png_out = png_out
    #  )

if __name__ == "__main__":
    handin_illustration()
