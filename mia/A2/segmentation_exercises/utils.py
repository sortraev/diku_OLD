import numpy as np
import numpy.linalg as npla
import matplotlib.pyplot as plt
import matplotlib
matplotlib.rc("image", cmap = "gray")

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

    axes_flat = axes.ravel() if n > 1 else [axes]

    imgs   += [None] * (ceil(n / 3) * 3 - n)
    titles += [None] * (len(imgs) - len(titles))

    for ax, (img, title) in zip(axes_flat, zip(imgs, titles)):
        #  ax.set_yticklabels([])
        #  ax.set_xticklabels([])
        if img is None:
            ax.axis("off")
        else:
            ax.imshow(img)
        if title is not None:
            ax.set_title(title)

    if fig_title is not None:
        fig.suptitle(fig_title)

    fig.tight_layout()

    if do_show:
        plt.show()
    if png_out is not None:
        fig.savefig(png_out, bbox_inches = "tight")

    return fig, axes_flat


def normalize(img):
    img = img.astype(float)
    _min = img.min()
    return (img - _min) / (img.max() - _min)

def dice(img, mask):
    # TP: guessed object; was object.
    # FP: guessed object; was background.
    # FN: guessed background; was object.
    # TN: guessed background; was background.
    TP = np.sum((img == 1) & (mask == 1))
    FP = np.sum((img == 1) & (mask == 0))
    FN = np.sum((img == 0) & (mask == 1))
    return 2 * TP / (2 * TP + FP + FN)

def mse(a, b):
    return np.mean((a - b) ** 2)

