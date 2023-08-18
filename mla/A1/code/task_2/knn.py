import numpy as np

def knn_many_points_single_k(train_set, train_labels, xs, k):
    """
    KNN-classifies an array of new points xs for a fixed value of k.
    no input validation!
    """
    # distance from each point in xs to each point in train_set.
    tmp = np.apply_along_axis(lambda x: train_set - x, 1, xs)
    dists = np.sum(tmp ** 2, axis = 2)


    kn_indices = np.argsort(dists, axis = 1)[:, :k] # sort distances and extract
                                                    # indices corresponding to k
                                                    # nearest neighbors for each point.

    kn_labels = train_labels[kn_indices]            # use these indices to extract
                                                    # labels of k nearest neighbors
                                                    # to each point.

    return np.round(np.mean(kn_labels, axis = 1))   # the new labels are computed by
                                                    # rounding the mean of labels
                                                    # of k nearest neighbors
                                                    # for each point.

def knn_single_point_many_k(train_set, train_labels, x, k = None):
    """
    KNN-classifies a single new point x for all possible values of k.
    no input validation!
    """
    # compute square dist from x to each point in training set.
    dists = ((train_set - x) ** 2).sum(1)

    # sort distances and pick k first labels (all labels if k is None).
    labels = train_labels[dists.argsort()[:k]]

    # i'th element of return value is prediction based on i nearest neighbors.
    return np.round(labels.cumsum() / np.arange(1, len(labels) + 1))

def knn_many_points_many_k(train_set, train_labels, xs, k = None):
    # the result is transposed such that the i'th
    # row holds all predictions of xs for k = i.
    return np.apply_along_axis(lambda x:
                                   knn_single_point_many_k(train_set,
                                                           train_labels,
                                                           x, k),
                               0, xs).T
    #  return np.array([knn_single_point_many_k(train_set, train_labels, x, k)
    #                   for x in xs]).T
