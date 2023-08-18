import numpy as np

import collections
class Queue(collections.deque):
    def enq(self, x):
        self.append(x)
    def enq_many(self, xs):
        for x in xs:
            self.append(x)
    def deq(self):
        return self.popleft() if len(self) > 0 else None

def _ccd_3d(img_in, th):
    # assumes img is a 3D, single-channel image (to use with 2D images, use
    # np.atleast_3d(img))
    # th is the threshold distance for determining neighbours (set th = 0 for
    # binary images).

    if img_in.dtype == bool:
        img_in = np.uint8(img_in)
    img = np.pad(img_in, pad_width = 1)
    out = np.zeros(img.shape, dtype = np.int64)

    q = Queue()
    component_id = 1
    for i, j, k in np.ndindex(img_in.shape):
        i, j, k = i+1, j+1, k+1

        if img[i, j, k] == 1 and out[i, j, k] == 0:
            out[i, j, k] = component_id

            q.enq(np.array([i, j, k]))
            while (p := q.deq()) is not None:
                x, y, z = p

                # find similar and unassigned neighbours.
                sub_img = img[x-1 : x+2, y-1 : y+2, z-1 : z+2]
                sub_out = out[x-1 : x+2, y-1 : y+2, z-1 : z+2]
                similar    = np.abs(sub_img - img[x, y, z]) <= th
                unassigned = sub_out == 0

                new = (np.argwhere(similar & unassigned) + p - 1)

                out[new[:, 0], new[:, 1], new[:, 2]] = component_id
                q.enq_many(new)

            component_id += 1
    return out[1:-1, 1:-1, 1:-1]

def ccd(img,
        num_components = None,
        *,
        dist_th = 0,
        preserve_ids = False):

    assert img.ndim in [2, 3]

    res = _ccd_3d(np.atleast_3d(img), dist_th).squeeze()

    # sort component ID's by size (ignoring component 0, which is background).
    ids = np.flip(np.bincount(res[res > 0].ravel()).argsort()[1:])
    component_mask = np.isin(res, ids[:num_components])
    return np.float64(component_mask * (res if preserve_ids else 1))


if __name__ == "__main__":
    #  pass

    # segmentation and plotting of the histogram segmentation example from the
    # report.
    from thresholding import *
    from utils import *
    from morphing import *
    import skimage.io
    img = skimage.io.imread("data/test.png")
    hist__ = skimage.io.imread("histogram.png")
    img_gray = normalize(img.mean(axis = 2))
    hist, bins = np.histogram(img_gray)

    four_biggest = hist.argsort()[:-5:-1]
    #  four_biggest = np.flip(hist.argsort())[:4]

    r = np.c_[bins[four_biggest], bins[four_biggest + 1]]

    _mask1 = ((img_gray >= r[0, 0]) & (img_gray < r[0, 1])) * 1
    _mask2 = ((img_gray >= r[1, 0]) & (img_gray < r[1, 1])) * 2
    _mask3 = ((img_gray >= r[2, 0]) & (img_gray < r[2, 1])) * 3
    _mask4 = ((img_gray >= r[3, 0]) & (img_gray < r[3, 1])) * 4

    mask1 = erode(dilate(ccd((img_gray >= r[0, 0]) & (img_gray < r[0, 1]), 1), 5), 5) * 1
    mask2 = erode(dilate(ccd((img_gray >= r[1, 0]) & (img_gray < r[1, 1]), 2), 6), 6) * 2
    mask3 = erode(dilate(ccd((img_gray >= r[2, 0]) & (img_gray < r[2, 1]), 1), 5), 5) * 3
    mask4 = erode(dilate(ccd((img_gray >= r[3, 0]) & (img_gray < r[3, 1]), 1), 5), 5) * 4

    for x, y in [(mask1, mask2), (mask1, mask3), (mask1, mask4),
                 (mask2, mask4), (mask2, mask3), (mask3, mask4)]:
        x[(x != 0) & (y != 0)] = 0


    pad = 10
    mask1 = mask1[pad:-pad, pad:-pad]
    mask2 = mask2[pad:-pad, pad:-pad]
    mask3 = mask3[pad:-pad, pad:-pad]
    mask4 = mask4[pad:-pad, pad:-pad]


    seg  = mask1 + mask2 + mask3 + mask4
    _seg = _mask1 + _mask2 + _mask3 + _mask4

    #  bc = np.argsort(np.bincount(seg.ravel()))[:-5:-1]
    bc = np.flip(np.argsort(np.bincount(seg.ravel())))[:4]

    PAD = 2
    def assign_invalid(img, valid, width = 5):
        PAD = width // 2
        img_pad = np.pad(img, mode = "edge", pad_width = PAD)
        for _i, _j in np.ndindex(img.shape):
            i, j = _i + PAD, _j + PAD
            if img[_i, _j] not in bc:
                tmp = img_pad[i - PAD : i + PAD + 1, j - PAD : j + PAD + 1]
                img[_i, _j] = np.argmax(np.bincount(tmp[np.isin(tmp, bc)]))
        return img

    seg = assign_invalid(seg, bc)
    mask_titles = [f"Thresholding in range [{round(lo, 2)}, {round(hi, 2)})"
                   for (lo, hi) in (r[0], r[1])]
    fig, axes = imshow_many([img, _mask1, _seg, hist__, _mask2, seg],
                            titles = ["Original image", mask_titles[0],
                                      "Finished segmentation",
                                      "Histogram of grayscaled image", mask_titles[1],
                                      "With CCD and morphological de-noising"],
                            do_show = False)
    axes[3].axis("off")
    fig.tight_layout()
    #  fig.show()
    fig.savefig("../tex/figures/histogram_segmentation.png", bbox_inches = "tight")
