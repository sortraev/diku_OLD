import numpy as np
from sklearn.ensemble import RandomForestClassifier as RFClassifier
import matplotlib.pyplot as plt
import joblib

do_validate    = True
do_plot        = True
do_store_model = False

data_dir = "" # "data/" 
data_extension = ".csv" # ".npy"
model_file = "RFC_model.joblib"

# MODEL TRAINING

try:
    RFC = joblib.load(model_file)
    print(">> loaded existing RFC model from file.")
except FileNotFoundError:
    print(">> no existing model from. fitting new model ...")
    train_data = np.load(data_dir + "landsat_train" + data_extension)
    train_y    = train_data[:, 0]
    train_X    = train_data[:, 1:]
    RFC = RFClassifier(n_estimators = 10,  # 10 trees in forest.
                       criterion = "gini", # use Gini impurities.
                       bootstrap = True,   # use bootstrap samples.
                       max_depth = None,   # build full trees.

                       n_jobs = -1,        # might aswell parallelize.
                       random_state = 2,   # for reproducibility.
                      )

    RFC.fit(train_X, train_y)
    if do_store_model:
        print(">> done! storing fitting model to file ..")
        joblib.dump(RFC, model_file)
    else:
        print(">> done!")


# VALIDATION
if do_validate:
    validation_data = np.load(data_dir + "landsat_validation" + data_extension)
    validation_y    = validation_data[:, 0]
    validation_X    = validation_data[:, 1:]

    print(">> predicting validation labels ...")
    validation_X_predictions = RFC.predict(validation_X)

    accuracy = np.mean(validation_X_predictions == validation_y)
    print(f">> validation accuracy: {accuracy}")


# PLOTTING
if do_plot:
    print(">> predicting landsat_area labels ...")
    landsat_area = np.load(data_dir + "landsat_area" + data_extension)
    res = RFC.predict(landsat_area)

    print(">> plotting landsat_area label predictions ...")
    img = res.reshape((3000, 3000)) / res.max()
    plt.imshow(img, cmap = "nipy_spectral")
    plt.tight_layout()
    plt.show()
