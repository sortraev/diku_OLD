import numpy as np
from sklearn.model_selection import StratifiedKFold
from scipy.linalg import solve_triangular

#########
## PCA ##
#########
class MyPCA():
    def __init__(self):
        self.pcs     = None
        self.pc_mags = None
        self.exp_var = None
        self.mean    = None
        self.fitted  = False

    def fit(self, x, bessel_correct = False):
        self.mean = x.mean(0)
        x_center  = x - self.mean

        # compute the cov matrix of the centered data.
        S = x_center.T.dot(x_center) / (x_center.shape[0] - int(bessel_correct))

        # since S is symmetric, we can use eigh(), which returns
        # eig- vals/vecs sorted by eigvals.
        mags, pcs = np.linalg.eigh(S)
        mags, pcs = mags[::-1], pcs[:, ::-1] # sort high to low.

        # compute explained variance. some of these can end up below 0 due to
        # numerical errors; hence .clip(0).
        self.exp_var = (mags / mags.sum()).clip(0)

        self.pcs     = pcs
        self.pc_mags = mags
        self.fitted  = True
        return self

    def project(self, X, n_components = None):
        if not self.fitted or self.mean is None or self.pcs is None:
            raise ValueError("proj() error: PCA model not fitted! Use "
                             "model.fit(X) first.")
        return np.dot(X - self.mean, self.pcs[:, :n_components])

#############
## K-means ##
#############
class MyKMeans():
    def __init__(self,
                 k,
                 init = "random",
                 iterations = 100):

        assert k >= 1, "invalid k"
        assert init in ["random", "k-means++"], "invalid initialization mode"
        assert iterations >= 1, "invalid iterations"

        self.k = k
        self._init = init
        self._iterations = iterations

        self.means  = None
        self.labels = None

    def __init_means_random(self, X, k):
        return X[np.random.permutation(len(X))]

    def __init_means_plusplus(self, X, k):
        n, d, *_ = X.shape
        means    = np.zeros((k, d), dtype = X.dtype)
        means[0] = X[np.random.randint(n)]

        for j in range(1, k):
            # i is the index into X of the element with the greatest
            # distance to the nearest cluster mean.
            i = (np.apply_along_axis(lambda x: x - means[:k], 1, X) ** 2
                ).sum(2).min(1).argmax(0)
            means[j] = X[i]
        return means

    def fit(self, X, k = None, init = None):
        # allow user to set new k and/or initialization method here.
        self.k = k = self.k if k is None else k
        self._init = init = self._init if init is None else init

        # initialize cluster means as per chosen initialization method.
        means = (self.__init_means_plusplus if self._init == "k-means++" else
                 self.__init_means_random)(X, k)
        labels = None # explicit, but redundant initialization.

        for _i in range(self._iterations):

            # assign each point in X to the nearest cluster mean.
            labels = MyKMeans.__classify(X, means)

            # count number of points associated with each label.
            counts = np.bincount(labels, minlength = k)

            # compute new means.
            _bincount = lambda X_col: np.bincount(labels, X_col)
            new_means = np.apply_along_axis(_bincount, 0, X)

            nz_mask = counts != 0
            new_means[nz_mask] /= counts[nz_mask, None]

            # check convergence and update current means.
            if np.all(means == new_means):
                break
            means = new_means

        # store the finaly cluster means and labels.
        self.means  = means
        self.labels = labels
        return self

    def __classify(X, cluster_means):
        return (np.apply_along_axis(lambda x: x - cluster_means, 1, X) ** 2
               ).sum(2).argmin(1)

    def classify(self, X):
        if self.means is None:
            raise ValueException("Model not fitted! Use model.fit() first.")
        _ = MyKMeans.__classify(X, self.means)
        return self.means, self.labels



#########
## KNN ##
#########
class MyKNN():

    def __knn_single_point_single_k(X_train, y_train, x, k):
        dists     = ((X_train - x) ** 2).sum(1)
        kn_labels = y_train[np.argpartition(dists, k)[:k]]
        print(kn_labels)
        return np.bincount(kn_labels).argmax()

    # UNUSED -- weighted k-NN
    #  def __knn_single_point_single_k_weighted(X_train, y_train, x, k):
    #      dists   = ((X_train - x) ** 2).sum(1)
    #      argpart = np.argpartition(dists, k)[:k]
    #      kn_labels, kn_dists = y_train[argpart], dists[argpart]
    #      return np.bincount(kn_labels, weights = np.exp(-0.001 * kn_dists)).argmax()

    def __knn_many_points_single_k(X_train, y_train, X_test, k):
        return np.array(
            [MyKNN.__knn_single_point_single_k(X_train, y_train, x, k)
             for x in X_test])

    def __knn_many_points_many_k(X_train, y_train, X_test, ks = None):
        def __knn_single_point_many_k(X_train, y_train, x, ks):
            dists = ((X_train - x) ** 2).sum(1)
            labels = y_train[dists.argsort()]
            return np.vectorize(lambda k: np.bincount(labels[:k]).argmax())(ks)
        # the result is transposed such that the i'th
        # row holds all predictions for k = i.
        ks = ks if ks is not None else np.arange(len(X_train))
        return np.array([__knn_single_point_many_k(X_train, y_train, x, ks)
                         for x in X_test]).T

    def __init__(self, k = None):

        assert k >= 1, "invalid k"
        self.X_train = None
        self.y_train = None
        self.k = k

    def fit(self, X_train, y_train, k = None):
        self.X_train = X_train
        self.y_train = y_train
        if k is not None:
            self.k = k
        return self

    def predict(self, X_test):
        if self.X_train is None or self.y_train is None:
            raise ValueError("predict() error: k-NN model not fitted! Use "
                             "model.fit(X_train, y_train) first.")
        if self.k is None:
            raise ValueError("predict() error: k parameter not specified. Set "
                             "this with model.k = k or use model.grid_search_k().")

        return MyKNN.__knn_many_points_single_k(self.X_train,
                                                self.y_train,
                                                X_test,
                                                self.k)
    def grid_search_k(self,
                      ks,
                      num_folds = 10,
                      scores_file_out = None
                     ):
        kfolds = StratifiedKFold(n_splits = num_folds, shuffle = True, random_state = 42)
        num_ks = len(ks)
        losses = np.zeros((num_folds, num_ks), dtype = float) - 1
        for i, (train_indices, val_indices) in enumerate(kfolds.split(self.X_train,
                                                                      self.y_train)):
            fold_no = i + 1
            print(f"Training fold {fold_no:2} out of {num_folds} ...\r",
                  end = "", flush = True)

            X_train = self.X_train[train_indices]
            y_train = self.y_train[train_indices]
            X_val   = self.X_train[val_indices]
            y_val   = self.y_train[val_indices]

            # compute knn predictions for the various ks.
            preds = MyKNN.__knn_many_points_many_k(X_train, y_train, X_val, ks)
            losses[i] = (preds != y_val).mean(1)

        print()

        if scores_file_out is not None:
            np.save(scores_file_out, losses)

        mean_loss_per_k = losses.mean(0)

        # the k that produced the best loss on average.
        k_best_overall = ks[mean_loss_per_k.argmin()]

        self.k = k_best_overall
        return k_best_overall


#######################
## Linear regression ##
#######################
class MyLinReg():

    def __init__(self):
        self.w     = None
        self.b     = None
        self.model = None

    def fit(self, X, y):
        if X.ndim == 1:
            X = X.reshape((-1, 1))
        n, d, *_ = X.shape
        X_aug = np.c_[X, np.ones(X.shape[0])] # append column of ones.

        if n > d: # QR-decompose and solve system of equations.
            Q, R = np.linalg.qr(X_aug)
            _w = solve_triangular(R, Q.T.dot(y))

        else:     # if R singular, solve using pseudo-inverse.
            _w = np.linalg.pinv(X_aug).dot(y).reshape((-1, ))

        self.w, self.b = _w[:-1], _w[-1]
        self.model = lambda _X: np.dot(_X, self.w.T) + self.b

        return self

    def get_model(self):
        if self.model is None:
            raise ValueError("get_model() error: model untrained! Use "
                             "model.fit(X, y) to train model.")
        return self.model

    def predict(self, X):
        return self.get_model()(X)

    def get_model_params(self):
        if self.w is None or self.b is None:
            raise ValueError("get_model_params() error: model untrained! Use "
                             "model.fit(X, y) to train model.")
        return self.w, self.b
