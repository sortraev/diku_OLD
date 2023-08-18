import numpy as np
np.random.seed(3)
from scipy.fft import fft2, ifft2, fftshift, ifftshift
from scipy.signal import convolve
from skimage.io import imread
from skimage import img_as_float
from skimage.segmentation import flood_fill

from imshow_many import imshow_many
def normalize_01(img):
    return (img - img.min()) / (img.max() - img.min())

def threshold_percentile(img, p):
    normalized_01 = (img - img.min()) / (img.max() - img.min())
    return normalized_01 >= p

def dilate(img, n_dilations):
    return convolve(img, np.ones((2*n_dilations + 1, ) * img.ndim),
                    mode = "same", method = "direct") > 0

def run_fft(img):
    img_fft = fft2(img)
    return img_fft, np.abs(img_fft) ** 2

def reconstruct(x,
                p = 0.01,
                n_dilations  = 4
               ):
    x_fft, x_psd = run_fft(x)
    x_psd_th = threshold_percentile(x_psd, p)

    center = tuple([s // 2 for s in x_psd_th.shape])

    mask = ifftshift(
        flood_fill(fftshift(x_psd_th), center, 0, connectivity = 2))
    mask = dilate(mask, n_dilations)

    img_recon = ifft2(x_fft * ~mask).real
    return img_recon, {"img_in_fft": x_fft,
                       "img_in_psd": x_psd,
                       "mask": mask
                      }

def run_example(img,
                a = None,
                v = 0.5 * np.random.uniform(0, np.pi),
                w = 0.5 * np.random.uniform(0, np.pi),
               ):

    if a is None:
        a = np.random.uniform(0.2, 1.2) * img.max()

    xs, ys, *_ = np.indices(img.shape)
    wave_noise = a * np.cos(xs * v + ys * w)

    img_noisy = normalize_01(img + wave_noise)

    recon, info = reconstruct(img_noisy)
    mask = fftshift(info["mask"])
    log_psd_th = np.log(1 + fftshift(info["img_in_psd"])) * ~mask

    imshow_many([
                 img, img_noisy, recon, mask,
                 log_psd_th,
                ],
                titles = ["original", "noisy image",
                          "recon", "mask", "log psd"
                          ],
                fig_title = f"(a, v, w) = {[round(x, 3) for x in [a, v, w]]}",
               )

img = normalize_01(imread("test_images/cameraman.tif"))
run_example(img)
