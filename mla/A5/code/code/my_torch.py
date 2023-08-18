#!/usr/bin/env python
# coding: utf-8

# # Logistic Regression with PyTorch Home Assignment
# ## Christian Igel, 2022
#
# In the following, we consider an example where 2D data points are classified using logistic regression and the solution is visualized.
#
# First, an implementation in Scikit-Learn is given.
# Then the same is partly implemented using PyTorch.
# Your assignment is to fill in the blanks in the PyTorch implementation.
#
# ## Reference implementation using Scikit-Learn

# In[ ]:


from sklearn.model_selection import train_test_split
from sklearn.datasets import make_classification
from sklearn.linear_model import LogisticRegression

import matplotlib
import matplotlib.pyplot as plt
import numpy as np



# Generate toy data:

# In[ ]:


m = 4  # Number of classes
d = 2  # Inout dimensionality
train_n = 50  # Training set size
test_n = 25  # Test set size

# Generate data with three classes
X, y = make_classification(n_samples=test_n + train_n, n_features=d, n_informative=d, n_redundant=0, n_repeated=0, n_classes=m, n_clusters_per_class=1, weights=None, flip_y=0.01, class_sep=0.75, hypercube=True, shift=0.0, scale=1.0, shuffle=True, random_state=None)

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_n, random_state=4711)


# ### Model training and evaluation
# Train and evaluate model:

# In[ ]:


# Train logistic regression
logreg = LogisticRegression(penalty='none', fit_intercept=True, multi_class='multinomial', solver='lbfgs')
logreg.fit(X_train, y_train);
# Get model parameters
ws = logreg.coef_
bs = logreg.intercept_
# Evaluate model
train_score = logreg.score(X_train, y_train)
test_score = logreg.score(X_test, y_test)
print("Training error:", train_score, "Test error:", test_score)


# ### Model visualization

# In[ ]:


# Colors for the three classes
colors = ['b', 'r', 'g', 'y']
class_colours = [colors[i] for i in y]
cmap = matplotlib.colors.ListedColormap(colors[:m])

# Compute the plot boundaries
xl, xh = np.floor(X[:,0].min() - 0.1), np.ceil(X[:,0].max() + 0.1)
yl, yh = np.floor(X[:,1].min() - 0.1), np.ceil(X[:,1].max() + 0.1)
plt.xlim(xl, xh)
plt.ylim(yl, yh)

# Create grid to calculate the decision boundary on
res = (xh-xl)/300  # Resulution of the grid
xx, yy = np.meshgrid(np.arange(xl, xh, res), np.arange(yl, yh, res))

# Make plot
plt.figure(1)
ax = plt.gca()

# Classify each point on the grid
Z = logreg.predict_proba(np.c_[xx.ravel(), yy.ravel()])
Z = np.argmax(Z, axis=1)
Z = Z.reshape(xx.shape)

# Plot points
ax.scatter(X[:,0], X[:,1], s=45, c=class_colours, edgecolor=plt.cm.gray(.95), lw=0.5, zorder=100)

# Plot classifications
cax = ax.matshow(Z, cmap=cmap, origin="lower", extent=[xl, xh, yl, yh], aspect="auto", alpha=.4)
ax.xaxis.set_ticks_position('bottom')
ax.grid(False)

if(m==2):  # Special case 2 classes
    print(ws)
    b = -bs/ws[0,1]
    a = -ws[0,0]/ws[0,1]
    x_line = np.arange(xl, xh, res)
    y_line = a * x_line + b
    ax.plot(x_line, y_line, 'k', lw=1, ls='--')
else:
    for i in np.arange(m-1):
        for j in np.arange(i+1,m):
            w = ws[i]-ws[j]
            b = bs[i]-bs[j]
            b = -b/w[1]
            a = -w[0]/w[1]
            x_line = np.arange(xl, xh, res)
            y_line = a * x_line + b
            ax.plot(x_line, y_line, "k", lw=1, ls='--')

plt.xlim(xl, xh)
plt.ylim(yl, yh)
plt.show()


#  ## Pytorch
#  Now the task is to reproduce the code above using logistic regression implemented in PyTorch.

# In[ ]:


import torch
import torch.nn as nn
import torch.optim as optim
import torch.nn.functional as F


# ### Model definition
# Let's define the logistic regression model as a simpl neural network with a single layer:

# In[ ]:


class LogisticRegressionPytorch(nn.Module):
    def __init__(self, input_dim, output_dim):
        super(LogisticRegressionPytorch, self).__init__()
        # LAYER DEFINITION MISSING
        self.l1 = nn.Linear(input_dim, output_dim)

        # explicitly initialize weight and bias.
        nn.init.uniform_(self.l1.weight)
        nn.init.zeros_(self.l1.bias)

    def forward(self, x):
        # RETURN VALUE MISSING
        return self.l1(x)

logreg_pytorch = LogisticRegressionPytorch(d, m)
print(logreg_pytorch)


# ### Model training and evaluation
# First we have to define a loss function. *Double check that output of the network and the expected input of the loss function match!*

# In[ ]:

X_train_T = torch.Tensor(X_train)
y_train_T = torch.from_numpy(np.int8(y_train))

X_test_T = torch.Tensor(X_test)
y_test_T = torch.from_numpy(np.int8(y_test))

# In[ ]:

optimizer = optim.Adam(logreg_pytorch.parameters(), lr=0.01)
# DEFINITION OF LOSS FUNCTION MISSING
loss_f = F.cross_entropy

print(y_train_T.dtype)

# In[ ]:


num_epochs = 40000  # Number of training steps
for epoch in range(num_epochs):
    # Zero the parameter gradients
    optimizer.zero_grad()

    # Forward + backward + optimize
    predictions = logreg_pytorch(X_train_T)
    # TODO: SOMETHING MISSING HERE
    loss = loss_f(predictions, y_train_T)

    # TODO: SOMETHING MISSING HERE
    loss.backward()
    optimizer.step()

    if epoch % 2000 == 0:
        with torch.no_grad():
            predictions = logreg_pytorch(X_test_T).detach().numpy().argmax(1)
            test_accuracy = (predictions == y_test).mean()
            print(f"Epoch {epoch}: test accuracy = {test_accuracy}")


# Now the trained model is evaluated:

# In[ ]:


train_predictions = logreg_pytorch(X_train_T).detach().numpy().argmax(1)
train_accuracy = (train_predictions == y_train).mean()

test_predictions = logreg_pytorch(X_test_T).detach().numpy().argmax(1)
test_accuracy = (test_predictions == y_test).mean()

print(f"Training accuracy: {train_accuracy}\nTest accuracy: {test_accuracy}")


# ### Model visualization
# For our visualization, we need the parameters of the model in the followoing format:

# In[ ]:


print("Weights:", ws, "biases:", bs)


# You can get a named list of all parameters of your Pytorch model like this:

# In[ ]:


for name, param in logreg_pytorch.named_parameters():
    print(name, param.data)


# Now get the parameters as in the format as above.
# To convert the trainable tensors to nupy arrays you may want to use `.detach().numpy()`:

# In[ ]:

ws_torch = logreg_pytorch.l1.weight.detach().numpy()
# TODO: SOMETHING MISSING HERE
bs_torch = logreg_pytorch.l1.bias.detach().numpy()


# Let's do the plotting.

# In[ ]:


# Make plot
plt.figure(2)
ax = plt.gca()

# Classify each point on the grid
X_Z_T = torch.Tensor(np.c_[xx.ravel(), yy.ravel()])  # Automatically casts to float
outputs = logreg_pytorch(X_Z_T)
_, predicted = torch.max(outputs.data, 1)

Z = predicted.numpy().reshape(xx.shape)

# Plot points
ax.scatter(X[:,0], X[:,1], s=45, c=class_colours, edgecolor=plt.cm.gray(.95), lw=0.5, zorder=100)

# Plot classifications
cax = ax.matshow(Z, cmap=cmap, origin="lower", extent=[xl, xh, yl, yh], aspect="auto", alpha=.4)
ax.xaxis.set_ticks_position('bottom')
ax.grid(False)

for i in np.arange(m-1):
    for j in np.arange(i+1,m):
        w = ws_torch[i]-ws_torch[j]
        b = bs_torch[i]-bs_torch[j]
        b = -b/w[1]
        a = -w[0]/w[1]
        x_line = np.arange(xl, xh, res)
        y_line = a * x_line + b
        ax.plot(x_line, y_line, "k", lw=1, ls='--')

plt.xlim(xl, xh)
plt.ylim(yl, yh)
plt.show()

