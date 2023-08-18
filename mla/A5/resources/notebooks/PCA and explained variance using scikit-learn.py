#!/usr/bin/env python
# coding: utf-8

# # Computing explained variance using scikit-learn and by hand using numpy 
# ## Christian Igel, 2022

# In[1]:


import numpy as np
from sklearn.decomposition import PCA
from sklearn import datasets


# Load some data

# In[2]:


X = datasets.load_diabetes().data


# Do computations using scikit-learn

# In[3]:


pca = PCA()
pca.fit(X)
eigenvalues = pca.singular_values_**2
print("Squared singular values:\n", eigenvalues)
print("'Explained variance' (not normalized):\n", pca.explained_variance_)
print("Explained variance per component (computed from previous result):\n", pca.explained_variance_ / np.sum(pca.explained_variance_))
print("Explained variance per component:\n", pca.explained_variance_ratio_)


# Do computations by hand

# In[4]:


# Remove mean 
Xmean=X.mean(axis=0)
Xcentered=X-Xmean

# Compute scatter matrix/empirical covariance matrix
N = Xcentered.shape[0]  # Number of samples
S = np.dot(Xcentered.T, Xcentered)  # Sum up outer products

# Eigenvalue decomposition of empirical covariance matrix
decomp = np.linalg.eig(S / N) # Divide by number of samples  
eigenvalues_by_hand = -np.sort(-decomp[0])
print("Eigenvalues (not Bessel corrected):\n", eigenvalues_by_hand)
print("Explained variance per component (not Bessel corrected):\n", eigenvalues_by_hand / np.sum(eigenvalues_by_hand)) 

# Eigenvalue decomposition of empirical covariance matrix using Bessel's correction
decomp = np.linalg.eig(S / (N-1)) # Divide by number of samples minus 1
eigenvalues_by_hand = -np.sort(-decomp[0])
print("Eigenvalues (Bessel corrected):\n", eigenvalues_by_hand)  
print("Explained variance per component (Bessel corrected):\n", eigenvalues_by_hand / np.sum(eigenvalues_by_hand))  


# **That is:** `pca.singular_values_` are from the decomposition of `X`, the
#  division by the number of training examples `N` (or `N-1`) is missing. The
#  `pca.explained_variance_` corresponds to the eigenvalues of the empirical
#  covariance matrix using Bessel's correction (i.e., using `N-1`).
#  `pca.explained_variance_ratio_` is normalized such that the explained variances
#  sum up to one. (As expected, the explained variance does not depend on whether
#                  Bessel's correction is used or not.)

# In[ ]:
