#!/usr/bin/env python 
import numpy as np
from imshow_many import imshow_many

def T(t):
    return np.c_[np.eye(3, 2), [*t, 1]]

def S(s):
    return np.diag([s, s, 1])

def R(theta, origin = None):
    cos_t, sin_t = np.cos(theta), np.sin(theta)
    return np.array([[cos_t, -sin_t, 0], [sin_t, cos_t, 0], [0, 0, 1]])

def task_2_2_transformation(center, t, theta, s):
    Tc = T(center)
    return T(t) @ Tc @ R(theta) @ S(s) @ np.linalg.inv(Tc)


def TRS(t, theta, s):
    return T(t) @ R(theta) @ S(s)

def my_warp_NN(img, f_inv, cval = 0):

    # array of homogeneous coordinates into img.
    idx1 = np.c_[[*np.ndindex(img.shape)], [1] * img.size].T

    # backward mapped coordinates with NN-interpolation.
    idx2 = (f_inv @ idx1)[:2].T.round().astype(int)

    # mask valid indices.
    mask = ((idx2 >= 0) & (idx2 < img.shape)).all(1)

    # construct result.
    res = np.ones_like(img) * cval
    res[tuple(idx1[:2, mask])] = img[tuple(idx2[mask].T)]
    return res



def task_2_2(t = (10.4, 15.7), theta = np.pi / 10, s = 2,
             do_savefig = False):
    img = np.zeros((100, 100))
    img[40:60, 40:60] = 1
    center = np.asarray(img.shape) // 2

    f = task_2_2_transformation(center, t, theta, s)
    res = my_warp_NN(img, np.linalg.inv(f))

    png_out = "figures/task_2_2.png" if do_savefig else None
    imshow_many([img, res],
                ["100x100 image with 20x20 centered square",
                 #  "$\mathbf{T}_{\mathbf t}\mathbf{T}_{\mathbf c}\mathbf {R}_",
                 #  r"\theta\mathbf{S}_{\mathbf s}\mathbf T_{\mathbf c}^{-1}$(original)",
                 ],
                figsize = (10, 5),
                png_out = png_out,
               )


def task_2_2_with_steps(do_savefig = False):
    img = np.zeros((100, 100))
    img[40:60, 40:60] = 1

    t     = (10.4, 15.7)
    theta = np.pi / 10
    s     = 2
    Tc = T(np.asarray(img.shape) // 2)

    T0 = np.linalg.inv(Tc)
    T1 = S(s) @ T0
    T2 = R(theta) @ T1
    T3 = Tc @ T2
    T4 = T(t) @ T3

    png_out = "figures/task_2_2_with_steps.png" if do_savefig else None
    imshow_many([img,
                 my_warp_NN(img, np.linalg.inv(T0)),
                 my_warp_NN(img, np.linalg.inv(T1)),
                 my_warp_NN(img, np.linalg.inv(T2)),
                 my_warp_NN(img, np.linalg.inv(T3)),
                 my_warp_NN(img, np.linalg.inv(T4))
                ],
                ["100x100 image with 20x20 centered square",
                 #  r"$\mathbf T_{\mathbf c}^{-1}$(img)",
                 #  r"$\mathbf {ST}_{\mathbf c}^{-1}$(img)",
                 #  r"$\mathbf {R}_\theta\mathbf{ST}_{\mathbf c}^{-1}$(img)",
                 #  r"$\mathbf{T}_{\mathbf c}\mathbf {R}_\theta\mathbf S_{\mathbf s}\mathbf T_{\mathbf c}^{-1}$(img)",
                 #  r"$\mathbf{T}_{\mathbf t}\mathbf{T}_{\mathbf c}\mathbf {R}_\theta\mathbf S_{\mathbf s}\mathbf T_{\mathbf c}^{-1}$(img)",
                ],
                png_out = png_out,
               )
