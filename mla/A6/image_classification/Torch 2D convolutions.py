#!/usr/bin/env python
# coding: utf-8

# # PyTorch 2D convolutions
# #### Christian Igel, 2021

# In[1]:


import torch
import torch.nn as nn


# ## One input channel, one output, no padding
# Let's define a `W`$\times$`W` filter. For the following examples, we do not need a bias parameter.

# In[2]:


# Convolution filter is of size W
W = 3
# 1 input (image) channel, 1 output channel, WxW convolution kernel
conv = nn.Conv2d(1, 1, W, bias=False)
print("We just defined:", conv)


# Let's look at the kernel dimensions:

# In[3]:


# 1 output channel, 1 input channel, 1st dimension = W, 2nd dimension = W
print(conv.weight.shape)


# The filter parameters are initialized randomly:

# In[4]:


print(conv.weight)


# We can set the parameters as follows:

# In[5]:


conv.weight = torch.nn.Parameter(torch.ones_like(conv.weight))
print(conv.weight)


# Let's define an input (image) `x`. The input is of the same shape as the filter:

# In[6]:


x = torch.arange(float(W*W))
x = torch.reshape(x, (1, 1, W, W))
print('Input:\n', x)
print('Sum of all input elements:', torch.sum(x).item())


# Because there is no padding and input and filter have the same size, there is only one valid position for the filter. Accordingly, the result is a tensor with a single value:

# In[7]:


c = conv(x)
print('Tensor:', c, 'scalar:', c.item())


# The scalar should be equal to the sum of all input elements (ensure that you understand why).

# ## One input channel, one output,  padding
# Now we add zero-padding such that the input dimensionality is preseved:
# 

# In[8]:


conv = nn.Conv2d(1, 1, W, padding=W//2, bias=False)
conv.weight = torch.nn.Parameter(torch.ones_like(conv.weight))
c = conv(x)
print(c)


# ## Several input channels, one output, no padding
# Typically, the input to a convolutional layer consists of several feature maps or channels. For example, consider a 2D input with three channels (e.g., an RGB colour image):

# In[9]:


x = torch.arange(float(3*W*W))
x = torch.reshape(x, (1, 3, W, W))
print('Input:', x)
print('Sum of all inputs:', torch.sum(x).item())


# Let's define a convolutional layer that takes three channels as input and produces a single output feature map:

# In[10]:


# 3 input (image) channels, 1 output channel, WxW convolution kernel
conv = nn.Conv2d(3, 1, W, bias=False)
conv.weight = torch.nn.Parameter(torch.ones_like(conv.weight))
print('Weight parameters of convolutional layer:', conv.weight)


# Note that there is one filter for each input channel.
# The convolutional layer first convolves each input channel with the corresponding filter.
# This results in three feature maps, whih are added to give the final result:

# In[11]:


c = conv(x)
print('number of filter parameters:', conv.weight.numel(), '\nresult of filtering the input:', c)


# It is important that the number of parameters and the dimesionality of the result is clear to you.

# Now let's apply 1$\times$1 convolutions to our three input channels. Again, we set all filter weights to 1.

# In[12]:


# 3 input (image) channels, 1 output channel, 1x1 convolution kernel
conv = nn.Conv2d(3, 1, 1, bias=False)
conv.weight = torch.nn.Parameter(torch.ones_like(conv.weight))
print(conv.weight)


# This convolutional layer adds the three input feature maps/channels:

# In[13]:


c = conv(x)
print(c)


# Thus, 1$\times$1 convolutions can be used to compute weighted sums of input feature maps/channels (in our previous example, all weights were set to 1). 

# ## Several output maps
# Typically, convolutional layer produce several feature maps or channels. For example, consider 
# extending the previous 1$\times$1 example to two output maps:

# In[14]:


# 3 input (image) channels, 2 output channel, 1x1 convolution kernel
conv = nn.Conv2d(3, 2, 1, bias=False)
conv.weight = torch.nn.Parameter(torch.ones_like(conv.weight))
print(conv.weight)


# This layer maps 3 input feature maps to 2 output feature maps, which are identical in our example, because we initialized all filters so that they are identical: 

# In[15]:


c = conv(x)
print(c)


# The first convolutional layer in a network has typically more output feature maps than input channels. Let's assume 3 input channels, 4 output channels of the same dimensionality (i.e., we use padding), and a filter size of 3. For each output channel, we have 3 filter with 9 parameters/weights each. Thus, we have 108 parameters in total:

# In[16]:


conv = nn.Conv2d(3, 4, W, padding=W//2, bias=False)
print(conv.weight)
print("Number of parameters:", conv.weight.shape.numel())


# And here are the resulting feature maps when applied to our input:

# In[17]:


c = conv(x)
print(c)


# # Image processing examples
# Now we consider a more complex example that involves some basic image transformations. First, we need to import NumPy and some image utilities.

# In[18]:


import torchvision
from PIL import Image
import numpy as np
import matplotlib.pyplot as plt


# Let's load an image and convert it to grayscale so that we just deal with a single channel: 

# In[21]:


image = Image.open('diku.jpg')  # Load image
image = torchvision.transforms.functional.to_grayscale(image)  # Transform to grayscale, because we only want one channel


# Let's plot the image:

# In[22]:


img_np = np.asarray(image) 
print("PIL image shape:", img_np.shape, "min:", img_np.min(), "max:", img_np.max())
plt.imshow(image, cmap='gray', vmin=0, vmax=255);


# The transformation of the image to a tensor maps has two important effects. First, the values are rescaled to $[0.,1.]$. Second, the channels become the first dimension.  The latter implies that, if we want to plot the image, we have to reorder the axes.

# In[24]:


import matplotlib
matplotlib.__version__


# In[23]:


x = torchvision.transforms.ToTensor()(image)
print("Tensor shape:", x.shape, "min:", x.min().item(), "max:", x.max().item())
plt.imshow(x.permute(1, 2, 0), cmap='gray', vmin=0, vmax=1);
print(x.permute(1, 2, 0).shape)
#plt.imshow(x[0], cmap='gray', vmin=0, vmax=1);


# In order to be process by a layer, the tensor needs  another dimension/axis for enumerating the elements in a batch:

# In[22]:


x.unsqueeze_(0)  # Add a dimension
print("Shape after adding batch dimension:", x.shape);


# Now we apply a simple horizontal gradient filter:

# In[23]:


hf = torch.tensor([[[[-1., 1.]]]])  # Define filter
print("Kernel:", hf, "shape:", hf.shape)

conv = nn.Conv2d(1, 1, kernel_size=(1, 2), padding=(0, 1), bias=False)  # Padding only in one dimension needed
conv.weight = torch.nn.Parameter(hf, requires_grad=False)  # Set kernel parameters to predefined filter parameters  
c = conv(x)  # Apply filter
print("Tensor shape:", c.shape, "min:", c.min().item(), "max:", c.max().item())


# We do not need a gradient for the kernel parameters, so we can use ``requires_grad=False``. This allows us to use ``c[0.0]`` as a NumPy array in the visualizaiton below. Alternatively, we could use ``c[0,0].detach()`` in the ``imshow`` call.

# In[24]:


print(c[0,0].shape)
plt.imshow(c[0,0], cmap='gray', vmin=-1, vmax=1);


# In[ ]:




