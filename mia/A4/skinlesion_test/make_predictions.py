import numpy as np
from glob import glob

def with_progress(generator, msg = "progress: ", interval = 1):
    out = [(not i % interval and print(f"{msg}{i}\r", end = "", flush = True))
           or x for i, x in enumerate(generator)]
    print(f"{msg}{len(out)} (finished)")
    return out


base_dir = "./"
img_dir  = base_dir + "SkinLesionTestData/"
#  model_file = base_dir + "skinlesion_model_AUGMENTED.hdf5"
model_file = base_dir + "skinlesion_model_FINAL.hdf5"



test_image_paths = sorted(glob(f"{img_dir}*.jpg"))

load_resized_from_npy = True
store_resized_to_npy  = False

if load_resized_from_npy:
    test_images = np.load(base_dir + "test_images_resized.npy")
else:
    from skimage.io import imread
    from skimage.util import img_as_ubyte
    from skimage.transform import resize

    n = None
    OUT_SHAPE = (256, 336)

    raw_images = with_progress((imread(img_file)
                                for img_file in test_image_paths[:n]),
                               "reading image ")
    test_images = with_progress((img_as_ubyte(resize(img, OUT_SHAPE))
                                 for img in raw_images),
                                "resizing image ")
    if store_resized_to_npy:
        np.save(base_dir + "test_images_resized.npy", test_images)

    test_images = resized_images


load_predictions = False
if load_predictions:
    predictions = np.load(base_dir + "predictions.npy").ravel()
else:
    import tensorflow.keras.models as models
    model       = models.load_model(model_file)
    predictions = (model.predict(test_images) >= 0.5).ravel()


test_image_names = [f.replace(img_dir, "").replace(".jpg", "")
                    for f in test_image_paths]
mapping = {0: "benign", 1: "malignant"}
results = np.array([[image_name, mapping[pred]]
                    for image_name, pred
                    in zip(test_image_names, predictions)],
                   dtype = str)
