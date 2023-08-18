#!/usr/bin/env python 
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


from sklearn.neighbors import KNeighborsClassifier
from scipy.spatial import procrustes


def load_data(filename):
    points = pd.read_csv(filename, usecols = range(2, 66)).values
    labels = pd.read_csv(filename, usecols = [1]).values.ravel()
    return points.reshape(-1, 32, 2).astype(float), labels


def procrustes_many(target_wing, wings):
    return np.array([procrustes(target_wing, wing)[1]
                     for wing in wings])

def task_3_1(do_plot = True, do_savefig = False):
    # load data.
    train_wings, train_labels = load_data("data/BioSCAN_dataset_Train.csv")
    test_wings,  test_labels  = load_data("data/BioSCAN_dataset_Test.csv")

    # perform procrustes.
    target_wing = train_wings[0]
    train_wings_aligned = procrustes_many(target_wing, train_wings)
    test_wings_aligned  = procrustes_many(target_wing, test_wings)

    # extract standardized target wing for plotting.
    target_wing = train_wings_aligned[0]

    if do_plot:
        fig, axes = plt.subplots(4, 3, figsize = (9, 12))
        for i, wing0, wing1, ax in zip(range(10),
                                    train_wings,
                                    train_wings_aligned,
                                    axes.flat):
            ax.grid(zorder = 0)
            # standardize original wing for plotting.
            wing0 -= wing0.mean(0)
            wing0 /= np.linalg.norm(wing0)
            do_scatter_plot = True
            ax.plot(*target_wing.T, color = "black",  label = "Target wing", alpha = 1)
            ax.plot(*wing0.T, color = "#0c5da5", label = "Unaligned (but standardized) wings", alpha = 0.8)
            ax.plot(*wing1.T, color = "#ff5500",  label = "Procrustes aligned wings", alpha = 1)

            ax.set_title(f"training wing \#{i}")

        for i in range(len(axes.flat) - 10):
            axes[-1][-1 - i].axis("off")
        handles, labels = axes[0, 0].get_legend_handles_labels()
        legend_loc = 0.45, 0.10
        fig.legend(handles, labels, loc = legend_loc, fontsize = 16, frameon = True,
                   framealpha = 1)
        fig.tight_layout()
        fig.show()
        if do_savefig:
            fig.savefig("figures/task_3_1.png", bbox_inches = "tight")

def task_3_2(split_p = 0.2):
    np.random.seed(41)

    # load data.
    train_wings, train_labels = load_data("data/BioSCAN_dataset_Train.csv")
    test_wings,  test_labels  = load_data("data/BioSCAN_dataset_Test.csv")

    # perform alignments.
    target_wing = train_wings[0]
    train_wings_aligned = procrustes_many(target_wing, train_wings)
    test_wings_aligned  = procrustes_many(target_wing, test_wings)


    train_wings = train_wings.reshape(-1, 64)
    test_wings  = test_wings.reshape(-1,  64)
    train_wings_aligned = train_wings_aligned.reshape(-1, 64)
    test_wings_aligned  = test_wings_aligned.reshape(-1, 64)

    indices = np.random.permutation(len(train_wings))
    validation_split = int(split_p * len(indices))
    val_indices, train_indices = indices[:validation_split], indices[validation_split:]

    n_val, n_train = len(val_indices), len(train_indices)

    _train_labels = train_labels[train_indices]
    _val_labels   = train_labels[val_indices]

    _train_wings = train_wings[train_indices]
    _val_wings   = train_wings[val_indices]

    _train_wings_aligned = train_wings_aligned[train_indices]
    _val_wings_aligned   = train_wings_aligned[val_indices]

    print(f"#training points = {n_train}, #validation points = {n_val}")

    # create model.
    knn = KNeighborsClassifier()
    knn.fit(_train_wings, _train_labels)
    k0, acc0 = -1, 0
    for k in range(1, n_train):
        knn.n_neighbors = k
        acc = np.mean(knn.predict(_val_wings) == _val_labels)
        if acc > acc0:
            k0, acc0 = k, acc

    knn.fit(_train_wings_aligned, _train_labels)
    k1, acc1 = -1, 0
    for k in range(1, n_train):
        knn.n_neighbors = k
        acc = np.mean(knn.predict(_val_wings_aligned) == _val_labels)
        if acc > acc1:
            k1, acc1 = k, acc

    # report best k's and the accuracies for those k's.
    knn.n_neighbors = k0
    knn.fit(train_wings, train_labels)
    accuracy0 = np.mean(knn.predict(test_wings) == test_labels)

    knn.n_neighbors = k1
    knn.fit(train_wings_aligned, train_labels)
    accuracy1 = np.mean(knn.predict(test_wings_aligned) == test_labels)

    print(f"best k pre-alignment:  {k0}")
    print(f"best k with alignment: {k1}")
    print(f"accuracy pre- alignment using k = {k0}: {accuracy0:.3f}")
    print(f"accuracy with alignment using k = {k1}: {accuracy1:.3f}")

def task_3_2_b(do_savefig = False):
    def curve_len(X):
        return np.linalg.norm(X - np.roll(X, 1, 0), axis = 1).sum()

    X_train, y_train = load_data("data/BioSCAN_dataset_Train.csv")
    X_test,  y_test  = load_data("data/BioSCAN_dataset_Test.csv")
    Xs, ys = np.r_[X_train, X_test], np.r_[y_train, y_test]

    ys_unique = np.unique(ys)

    grouped = {label: np.array([x for (x, y) in zip(Xs, ys)
                                if y == label])
                     for label in ys_unique}

    mean_len_per_group = {
        label: (np.mean([curve_len(wing) for wing in wings]),
                np.std([curve_len(wing)  for wing in wings]),
                len(wings))
        for label, wings in grouped.items()
    }
    labels = list(mean_len_per_group.keys())
    means  = [x[0] for x in mean_len_per_group.values()]
    stds   = [x[1] for x in mean_len_per_group.values()]
    counts = [x[2] for x in mean_len_per_group.values()]
    labels = [f"{label} ({count})" for label, count in zip(labels, counts)]

    fig, ax = plt.subplots(1, 1, figsize = (12, 8))

    ax.tick_params(axis = 'x', rotation = 45, labelsize = 12)

    ax.set_ylabel("Mean wing curve length for species", fontsize = 18)
    ax.bar(labels, means, yerr = stds, align = "center", alpha = 0.8, capsize = 10)
    if do_savefig:
        fig.savefig("figures/task_3_2_b.png")
    fig.show()

