import numpy as np
import matplotlib
matplotlib.rc("image", cmap = "gist_gray")
import matplotlib.pyplot as plt
plt.style.use("science")


mask_array_exp, im_array_exp, results_exp = np.load("unet_result_example.npy")

mask_array_exp = np.flip(mask_array_exp, axis = 0)
im_array_exp   = np.flip(im_array_exp, axis = 0)
results_exp    = np.flip(results_exp, axis = 0)

idx_list = ['JPCLN054.gif', 'JPCLN016.gif']

fig, axes = plt.subplots(len(im_array_exp), 2, figsize = (8, 8))

dice_scores = [0.98, 0.9]
for (filename, img, mask, res, ax, dice) in zip(idx_list, im_array_exp,
                                                mask_array_exp, results_exp,
                                                axes, dice_scores):
    #  ax[0].imshow(img)
    #  ax[0].set_title(filename, fontsize = 16)
    ax[0].imshow(img)
    ax[0].imshow(mask, alpha = 0.5)
    ax[0].set_title(f"{filename} - reference segmentation mask", fontsize = 13)
    ax[1].imshow(img)
    ax[1].imshow(res, alpha = 0.5)
    ax[1].set_title(f"Predicted segmentation (DICE = {dice})", fontsize = 13)

fig.tight_layout()
fig.savefig("unet_result_examples.png", bbox_inches = "tight")
#  fig.show()
