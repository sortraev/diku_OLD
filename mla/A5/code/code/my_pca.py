#!/usr/bin/env python3
import numpy as np
import matplotlib.pyplot as plt
plt.style.use("science")
import matplotlib
matplotlib.rc("image", cmap = "gray")
from sklearn import datasets


def my_pca(x, bessel_correct = False):
    x_center = x - x.mean(0)

    # compute the cov matrix of the centered data.
    S = x_center.T.dot(x_center)

    # since S is symmetric, we can use eigh(), which returns
    # eig- vals/vecs sorted by eigvals.
    mags, pcs = np.linalg.eigh(S / (S.shape[0] - int(bessel_correct)))
    mags, pcs = mags[::-1], pcs[:, ::-1] # sort high to low.

    # compute explained variance. some of these can end up below 0 due to
    # numerical errors, hence .clip(0).
    exp_var = (mags / mags.sum()).clip(0)

    return pcs, mags, exp_var



if __name__ == "__main__":
    do_plot = True

    ## LOAD HANDWRITTEN DIGITS DATA

    digits = datasets.load_digits()
    X = digits.data

    ## DO THE PCA
    pcs, pc_mags, exp_var = my_pca(X)

    ## FIND OUT IF 10 COMPONENTS ARE ENOUGH TO EXPLAIN 80% OF THE VARIANCE
    cum_exp_var = exp_var.cumsum()

    # number of PC's needed to explain 80% or more of variance.
    first_above_80 = np.argwhere(cum_exp_var >= 0.8)[0, 0] + 1

    print(f"variance explained by 10 most significant PC's: "
          f"{cum_exp_var[10 - 1]}\n"
          f"at least 80% of variance explained by first 10 "
            f"PC's: {first_above_80 <= 10}.\n"
          f"to explain 80% of variance requires at least {first_above_80} PC's.")



## PLOTS
    if do_plot:
        ## PLOT EXPLAINED VARIANCE AND EIGENSPECTRUM
        x_range = range(1, len(exp_var) + 1)
        fig, ax1 = plt.subplots(1, 1, figsize = (12, 8))
        ax2 = ax1.twinx()

        ax1.set_yscale("log")

        ax1.scatter(x_range, exp_var) # just to get the y-axis on ax1.
        ax2.scatter(x_range, pc_mags, label = "Eigenvalues / explained variance")
        ax2.axvline(first_above_80, color = "crimson", label = "80\% variance explained")

        ax1.set_xlabel("Principal component")
        ax1.set_xticks([1, 10, 12, 20, 30, 40, 50, 60, 64])

        ax1.set_ylabel("Explained variance (LOGARITHMIC)")
        ax2.set_ylabel("Eigenvalues (LOGARITHMIC)")
        ax2.set_yscale("log")

        fig.legend()
        fig.show()

        ## PLOT EIGENDIGITS

        first_five_eigendigits = [img.reshape((8, 8)) for img in pcs.T[:5]]

        fig, axes = plt.subplots(2, 3, figsize = (16, 8))
        axes_flat = axes.ravel()#[:5]
        titles = ["PC" + i for i in "12345"]

        for ax, (img, title) in zip(axes_flat[:5],
                                    zip(first_five_eigendigits, titles)):
            ax.axis("off")
            ax.imshow(img)
            ax.set_title(title, fontsize = 20)

        axes_flat[-1].axis("off")
        fig.tight_layout()
        fig.show()
