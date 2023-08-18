import torch
import torch.nn as nn
import torch.nn.functional as F

import matplotlib.pyplot as plt
from skimage.io import imread

torch.manual_seed(2)

K = 3

img_np = imread("diku.jpg").mean(axis = 2)
img_t  = torch.tensor(img_np).view((1, 1) + img_np.shape)


class Sobel(nn.Module):
    kernel_x = torch.tensor([[-1, 0, 1],
                             [-2, 0, 2],
                             [-1, 0, 1]], dtype = float).view(1, 1, 3, 3)
    kernel_y = kernel_x.transpose(3, 2)
    def __init__(self):
        super().__init__()
        K = 3

        self.x = nn.Conv2d(1, 1, K, padding = K // 2, padding_mode = "reflect", bias = False)
        self.y = nn.Conv2d(1, 1, K, padding = K // 2, padding_mode = "reflect", bias = False)
        self.x.weight = nn.Parameter(Sobel.kernel_x, requires_grad = False)
        self.y.weight = nn.Parameter(Sobel.kernel_y, requires_grad = False)

    def forward(self, img):
        return torch.sqrt(self.x(img) ** 2 + self.y(img) ** 2)


class Net(nn.Module):
    def __init__(self):
        super().__init__()

        self.conv1 = nn.Conv2d(3, 64, kernel_size = 5)
        self.pool1 = nn.MaxPool2d(2, stride = 2)
        self.conv2 = nn.Conv2d(64, 64, kernel_size = 5)
        self.pool2 = nn.MaxPool2d(2, stride = 2)
        self.fc2   = nn.Linear(1024, 43)#, bias = True)

    def forward(self, x):
        x = self.conv1(x)
        x = self.pool1(x)
        x = self.conv2(x)
        x = self.pool2(x)

        x = self.fc2(x)
        return x
