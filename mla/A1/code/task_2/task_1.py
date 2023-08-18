import numpy as np
import sys
from knn import *
import matplotlib.pyplot as plt

DATADIR = "../MNIST-5-6-Subset/"
mnist0 = np.loadtxt(DATADIR + "MNIST-5-6-Subset.txt",
                    dtype = float).reshape((-1, 28 * 28))
mnist_labels = np.loadtxt(DATADIR + "MNIST-5-6-Subset-Labels.txt")

mnist0_trainset   = mnist0[:100]
mnist_trainlabels = mnist_labels[:100]

get_val_range = lambda n, i: np.arange(n) + 100 + i * n

def task1_1(do_show = True, png_out = None):
    fig, axes = plt.subplots(2, 2, figsize = (16, 8))
    axes = axes.reshape((4, ))

    max_k = 50
    ns  = [10, 20, 40, 80]
    _is = range(1, 6)
    ks  = range(1, max_k + 1)

    for (n, ax) in zip(ns, axes):
        for i in _is:

            val_range  = get_val_range(n, i)
            val_set    = mnist0[val_range]
            val_labels = mnist_labels[val_range]

            # note: predictions is transposed such that it is [k; image].
            predictions = np.array([knn_single_point_many_k(mnist0_trainset,
                                                            mnist_trainlabels,
                                                            img,
                                                            k = max_k)
                           for img in val_set]).T

            # compute mean zero/one loss.
            accuracy = np.mean(predictions == val_labels, axis = 1)
            error    = 1 - accuracy

            err_argmin = np.argmin(error)
            err_min    = error[err_argmin].round(3)

            ax.set_title(f"n = {n}")
            ax.set_xlabel("k")
            ax.set_ylabel("mean zero/one prediction error", rotation = 90)
            ax.plot(ks, error, label = f"i = {i}, (err_min, k) = {err_min, err_argmin + 1}")
            ax.legend()
            ax.grid(zorder = 3)


    fig.tight_layout()
    if do_show:
        fig.show()
    if png_out is not None:
        fig.savefig(png_out, bbox_inches = "tight")


def task1_2(do_show = True, png_out = None):
    fig, ax = plt.subplots(1, 1, figsize = (16, 8))

    max_k = 50
    ns  = [10, 20, 40, 80]
    _is = range(1, 6)
    ks  = range(1, max_k + 1)
    do_print = True
    for n in ns:
        errors_per_i = []

        for i in _is:

            val_range  = get_val_range(n, i)
            val_set    = mnist0[val_range]
            val_labels = mnist_labels[val_range]

            # note: predictions is transposed such that it is [k; image].
            predictions = np.array([knn_single_point_many_k(mnist0_trainset,
                                                            mnist_trainlabels,
                                                            p,
                                                            k = max_k
                                                            )
                           for p in val_set]).T

            errors = np.mean(predictions != val_labels, axis = 1)
            errors_per_i.append(errors)

        errors_per_i = np.array(errors_per_i)
        i_var_per_k = errors_per_i.var(axis = 0)
        print(f"errors_per_i.shape: {errors_per_i.shape}")

        var_argmax = np.argmax(i_var_per_k)
        var_max    = i_var_per_k[var_argmax].round(3)

        ax.plot(ks, i_var_per_k,
                label = f"n = {n}, (var_max, k) = {var_max, var_argmax + 1}")


    handles, labels = ax.get_legend_handles_labels()
    ax.legend(handles, labels, loc='upper left')
    ax.set_xlabel("k")
    ax.set_ylabel("variance across i", rotation = 90)
    ax.grid(zorder = 3)

    if do_show:
        fig.show()
    if png_out is not None:
        fig.savefig(png_out, bbox_inches = "tight")

task1_1(do_show = True)#, png_out = "fig2_1.png")
task1_2(do_show = True)#, png_out = "fig2_2.png")
