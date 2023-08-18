import numpy as np
import numpy.linalg as npla
import scipy.linalg

class MyLinReg():

    def __init__(self):
        self.w = None
        self.b = None
        self.model = None

    def __fit_pinv(self, X, y):
        X = np.hstack((X, np.ones((X.shape[0], 1))))
        _w = npla.pinv(X).dot(y).reshape((-1, ))
        return _w[:-1], _w[-1]

    def __fit_qr(self, X, y):
        """
        assumes that X is invertible, since R must be non-singular in order
        for Rw = Q.T * y to be solvable.
        """
        X = np.hstack((X, np.ones((X.shape[0], 1))))
        Q, R = npla.qr(X)
        _w = scipy.linalg.solve_triangular(R, Q.T.dot(y))
        return _w[:-1], _w[-1]

    def fit(self, X, y):
        if X.ndim == 1:
            X = X.reshape((-1, 1))
        n, d = X.shape
        self.w, self.b = (self.__fit_qr if n > d else self.__fit_pinv)(X, y)

        self.model = lambda X: np.dot(X, self.w.T) + self.b
        return self.w, self.b

    def get_model(self):
        if self.model is None:
            raise ValueError("predict() error: model untrained! use .fit(X, y) "
                             "to train model.")
        return self.model
