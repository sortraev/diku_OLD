#!/usr/bin/env python
# coding: utf-8

# # PCA Home Assignment
# ## Christian Igel, 2021

# In[1]:


import numpy as np
from sklearn.decomposition import PCA
from sklearn import datasets

import matplotlib.pyplot as plt
get_ipython().run_line_magic('matplotlib', 'inline')


# Load handwritten digits data:

# In[2]:


digits = datasets.load_digits()
imshape = digits.images[0].shape  # The 2D image shape
X = digits.images.reshape(digits.images.shape[0],-1)  # Image


# Do the PCA:

# Find out if 10 components are enough to explain 80% of the variance:

# Plot the eigenspectrum:

# Plot "eigendigits":
