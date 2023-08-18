#!/usr/bin/env python3
import numpy as np

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

