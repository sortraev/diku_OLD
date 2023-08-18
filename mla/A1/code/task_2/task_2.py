import numpy as np
import sys
from knn import *
import matplotlib.pyplot as plt


DATADIR = "MNIST-5-6-Subset/"
mnist_labels = np.loadtxt(DATADIR + "MNIST-5-6-Subset-Labels.txt")

load_imgs = lambda fp: np.loadtxt(fp, dtype = float).reshape((-1, 28 * 28))
mnist0 = load_imgs(DATADIR + "MNIST-5-6-Subset.txt")
mnist1 = load_imgs(DATADIR + "MNIST-5-6-Subset-Light-Corruption.txt")
mnist2 = load_imgs(DATADIR + "MNIST-5-6-Subset-Moderate-Corruption.txt")
mnist3 = load_imgs(DATADIR + "MNIST-5-6-Subset-Heavy-Corruption.txt")

mnist_sets = [(mnist0, "No corruption"),
              (mnist1, "Light corruption"),
              (mnist2, "Moderate corruption"),
              (mnist3, "Heavy corruption")
             ]



def task2_2(do_show = True, png_out = None):
    fig, axes = plt.subplots(2, 2, figsize = (16, 8))
    axes = axes.reshape((4, ))

    n = 80
    max_k = 50
    ks = range(1, max_k + 1)

    trainlabels = mnist_labels[:100]
    for ((dataset, dataset_name), ax) in zip(mnist_sets, axes):

        trainset = dataset[:100]
        for i in range(1, 6):

            val_range  = range(100 + i * n, 100 + (i + 1) * n)

            val_set    = dataset[val_range]
            val_labels = mnist_labels[val_range]

            # note: predictions is transposed such that it is [k; image].
            predictions = np.array([knn_single_point_many_k(trainset,
                                                            trainlabels,
                                                            p,
                                                            k = max_k
                                                            )
                           for p in val_set]).T

            # compute mean zero/one loss.
            error = np.mean(predictions != val_labels, axis = 1)
            ax.plot(ks, error, label = f"i = {i}")

            ax.set_title(f"{dataset_name}")
            ax.set_xlabel("k")
            ax.set_ylabel("mean zero/one prediction error", rotation = 90)
            ax.legend()
            ax.grid(zorder = 3)

    #  fig.suptitle("Mean zero/one prediction error of KNN for various\n"
                 #  "degrees of data corruption for n = 80 and varying i, k.")

    fig.tight_layout()
    if do_show:
        fig.show()
    if png_out is not None:
        fig.savefig(png_out, bbox_inches = "tight")

task2_2(do_show = True)#, png_out = "fig2_3.png")
