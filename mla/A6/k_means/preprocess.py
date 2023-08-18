import numpy as np

def my_pca(x, bessel_correct = False):
    x_bar = x - x.mean(0)

    # compute cov of centered data.
    S = x_bar.T.dot(x_bar)

    # eigh() returns eigvals/-vecs sorted by eigvals.
    mags, pcs = np.linalg.eigh(S / (S.shape[0] - int(bessel_correct)))
    mags, pcs = mags[::-1], pcs[:, ::-1] # sort high to low.

    # explained variance.
    exp_var = (mags / mags.sum()).clip(0)

    return pcs, mags, exp_var

def preprocess(data, labels):
    valid_labels = np.isin(labels, np.argwhere(np.bincount(labels) >= 65))
    labels = labels[valid_labels]
    data   = data[valid_labels]

    normalized = (data - data.mean(0)) / data.std(0)
    return normalized, labels


train_all = np.loadtxt("VSTrain.dt", delimiter = ',', dtype = float)
train_data, train_labels = preprocess(train_all[:, :-1],
                                      np.int64(train_all[:, -1]))

pcs, mags, exp_var = my_pca(train_data)



#  test_all    = np.loadtxt("VSTest.dt", delimiter = ',', dtype = float)
#  test_labels = np.int64(test_all[:, -1])
#  test_data   = test_all[:, :-1]


