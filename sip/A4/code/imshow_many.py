import matplotlib
matplotlib.rc("image", cmap = "gray")
import matplotlib.pyplot as plt

import numpy as np

def imshow_many(imgs,
                titles    = [],
                fig_dims  = None,
                fig_title = None,
                figsize   = (16, 8),
                do_show   = True,
                png_out   = None):

    n = len(imgs)
    if n < 1:
        return

    if fig_dims is not None:
        x, y = fig_dims
    else:
        x, y = (2, 2) if n == 4 else (int(np.ceil(n / 3)), min(n, 3))
    fig, _axes = plt.subplots(x, y, figsize = figsize)
    axes = _axes.ravel() if n > 1 else [_axes]

    titles += [None] * (n - len(titles))

    for ax, (img, title) in zip(axes, zip(imgs, titles)):
        if img is None:
            ax.set_visible(False)
        else:
            ax.imshow(img, aspect = img.shape[1] / img.shape[0])
            if title is not None:
                ax.set_title(title, fontsize = 14)

    for ax in axes[len(imgs):]:
        ax.set_visible(False)

    if fig_title is not None:
        fig.suptitle(fig_title, fontsize = 18)

    fig.tight_layout()
    fig.tight_layout()

    if do_show:
        plt.show()
    if png_out is not None:
        fig.tight_layout()
        fig.savefig(png_out, bbox_inches = "tight")
    fig.tight_layout()

    return fig
