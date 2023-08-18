import numpy as np
import matplotlib.pyplot as plt


class KMeans():
    """ Simple K-Means implementation. Note that you can access
    the cluster means and the cluster assignments once you have
    called the "fit" function. The cluster means are stored in the
    variable 'cluster_means' and the assignments to the cluster
    means in 'cluster_assignments'. You can also use the function
    'assign_to_clusters' to obtain such assignments for a new set
    X of points.
    """

    def __init__(self,
                 k = 2,
                 init_clusters = None,
                 plusplus      = False,
                 iterations = 100):
        assert init_clusters is None or len(init_clusters) == k

        self.k = k
        self.init_clusters = init_clusters
        self.iterations = iterations
        self.plusplus   = plusplus

        self.means  = None
        self.labels = None

    def __init_means_random(self, X, k):
        return X[np.random.choice(len(X), k, replace = False)]

    def __init_means_plusplus(self, X, k):

        means = np.zeros((k, X.shape[1]), dtype = X.dtype)
        means[0] = X[np.random.randint(len(X))]

        for k in range(1, k):
            # i is the index of the element of X which has the greatest
            # distance to its nearest (currently known) cluster mean.
            i = (np.apply_along_axis(lambda x: x - means[:k], 1, X) ** 2
                ).sum(axis = 2).min(axis = 1).argmax(axis = 0)
            means[k] = X[i]
        print(f"plusplus means: {means}")
        return means

    def fit(self, X, k = None):
        self.k = k = self.k if k is None else k
        # initialize cluster means as per chosen initialization method.
        means = (self.init_clusters if self.init_clusters is not None else
                    (self.__init_means_plusplus if self.plusplus else
                     self.__init_means_random)(X, k))

        for _i in range(self.iterations):

            # assign each point in X to the nearest cluster mean.
            labels = KMeans.__classify(X, means)

            # count number of points associated with each label.
            counts = np.bincount(labels, minlength = k)

            # compute new means.
            _bincount = lambda X_col: np.bincount(labels, X_col)
            new_means = np.apply_along_axis(_bincount, 0, X)

            nz_mask = counts > 0
            new_means[nz_mask] /= counts[nz_mask, None]

            # update current means.
            means = new_means

        # store the finaly cluster means and labels.
        self.labels = labels
        self.means  = means


    def __classify(X, means):
        return (np.apply_along_axis(lambda x: x - means, 1, X) ** 2
               ).sum(axis = 2).argmin(axis = 1)

    def classify(self, X):
        if self.means is None:
            raise ValueException("Model not fitted! Use model.fit() first.")
        return KMeans.__classify(X, self.means)



if __name__ == "__main__":

    np.random.seed(0)

    old_faithful = np.genfromtxt("old_faithful.csv", delimiter = ",")[1:]

    model = KMeans(k = 2, iterations = 30, plusplus = True) # initialize model and fit it to data
    model.fit(old_faithful)

    means      = model.means
    labels     = model.labels

    reference = np.array([[2.09433, 54.75], [4.29793023, 80.28488372]])
    _reference = reference[reference[:, 0].argsort()]
    _means = means[np.argsort(means[:, 0])]
    print(f"reference - actual:\n{np.abs(_reference - _means)}")


    ### plotting
    fig1, plot1 = plt.subplots(1, 1, figsize=(20, 10))

    # plot old faithful dataset
    plot1.scatter(old_faithful[labels == 0, 0], old_faithful[labels == 0, 1], color = "teal")
    plot1.scatter(old_faithful[labels == 1, 0], old_faithful[labels == 1, 1], color = "crimson")

    # plot derived cluster means
    plot1.scatter(means[:, 0], means[:, 1], color = "red", s = 200,
                  label = "Cluster means", marker = 'x')
    plot1.grid()
    plot1.legend(fontsize = 18)

    plt.show()
