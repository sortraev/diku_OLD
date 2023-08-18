import numpy as np
from sklearn.model_selection import train_test_split
import sklearn.decomposition
import matplotlib.pyplot as plt
import my_stuff
import argparse

np.random.seed(0)
try: plt.style.use("science")
except: pass
colors = list(plt.rcParams['axes.prop_cycle'].by_key()['color']) * 4

parser = argparse.ArgumentParser(prog = 'Q4 solutions')
parser.add_argument("--datadir", default = "./data/",
                    help = "path to train/test data (default: ./data/)")
parser.add_argument("--noplot", action = "store_true",
                    help = "disable plots")
parser.add_argument("--do-gridsearch-k", action = "store_true",
                    help = "enable grid search for optimal k for k-NN")
args = parser.parse_args()

do_plot = not args.noplot
do_knn_parameter_search = args.do_gridsearch_k
data_dir = args.datadir + (args.datadir[-1] != '/') * '/'


## LOAD DATA AND CREATE VALIDATION DATA SPLIT
crop = None # None means no cropping.
try:
    X_train_full = np.loadtxt(data_dir + "X_train.csv", delimiter = ',')[:crop]
    y_train_full = np.loadtxt(data_dir + "y_train.csv", delimiter = ',')[:crop].astype(int)
    X_test = np.loadtxt(data_dir + "X_test.csv", delimiter = ',')[:crop]
    y_test = np.loadtxt(data_dir + "y_test.csv", delimiter = ',')[:crop].astype(int)
except FileNotFoundError as e:
    print(f">> Failed to find data. Use --datadir to specify data directory.")
    import sys
    sys.exit(1)
(X_train, X_val, y_train, y_val) = train_test_split(X_train_full,
                                                    y_train_full,
                                                    stratify = y_train_full,
                                                    test_size = 0.3,
                                                    shuffle = True,
                                                    random_state = 42)
############
## TASK 1 ##
############
print(">> Task 4.1: Class frequencies")
y_train_counts = np.bincount(y_train_full)
y_train_freqs  = y_train_counts / y_train_counts.sum()
y_test_counts  = np.bincount(y_test)
y_test_freqs   = y_test_counts / y_test_counts.sum()

print(f"Class | train freq | test freq")
for i, (train_freq, test_freq) in enumerate(zip(y_train_freqs, y_test_freqs)):
    print(f"{i:5} | {train_freq.round(2):10} | {test_freq.round(2):9}")

if do_plot:
    print("Plotting class frequencies ..")
    fig, [ax1, ax2] = plt.subplots(1, 2, figsize = (12, 6))
    ax1.set_axisbelow(True); ax1.grid()
    ax2.set_axisbelow(True); ax2.grid()

    for i, y in enumerate(y_train_freqs):
        ax1.bar(i, y, color = colors[i])
        ax1.text(i - 0.14, y - 0.02, f"{round(y, 2):.2f}",
                 size = 14, fontweight = "bold")
    ax1.set_xlabel("Class", size = 14)
    ax1.set_ylabel("Frequency", size = 14)
    ax2.set_xlabel("Class", size = 14)
    ax2.set_ylabel("Fequency", size = 14)

    for i, y in enumerate(y_test_freqs):
        ax2.bar(i, y, color = colors[i])
        ax2.text(i - 0.14, y - 0.02, f"{round(y, 2):.2f}",
                 size = 14, fontweight = "bold")

    ax1.set_title("Train data class frequencies", size = 18)
    ax2.set_title("Test data class frequencies" , size = 18)
    fig.tight_layout()
    plt.show()


############
## TASK 2 ##
############
print("\n>> Task 4.2: PCA")
PCA = my_stuff.MyPCA()
PCA.fit(X_train_full)
first_above_90 = (PCA.exp_var.cumsum() >= 0.9).argmax() + 1
proj1, proj2, *_ = PCA.project(X_train_full, n_components = 2).T
print(f"Num PC's: {len(PCA.pcs)}")
print(f"To explain 90% of variance requires at least {first_above_90} PC's.")

print(PCA.pc_mags)


if do_plot:
    print("Plotting eigenspectrum ..")
    ## PLOT EIGENSPECTRUM
    x_range = range(1, len(PCA.pc_mags) + 1)
    fig, ax1 = plt.subplots(1, 1, figsize = (5, 5))
    ax1.set_axisbelow(True); ax1.grid()

    ax1.plot(x_range, PCA.pc_mags, label = "Eigenvalues")

    ax1.set_xticks(x_range)
    ax1.set_xlim([1, len(PCA.pc_mags)])
    ax1.set_xlabel("Principal component", size = 14)
    ax1.set_ylabel("Eigenvalues (LOGARITHMIC)", size = 14)
    ax1.set_yscale("log")

    ax1.legend(fontsize = 12, loc = "upper right")
    fig.tight_layout()
    plt.show()



    ## PLOT PCA PROJECTION 
    print("Plotting PCA projection onto first two PC's ..")
    scale = 1.2
    x_lo, x_hi = proj1.min() * scale, proj1.max() * scale
    y_lo, y_hi = proj2.min() * scale, proj2.max() * scale

    figsize_x = 12
    figsize_y = figsize_x / ((x_hi - x_lo) / (y_hi - y_lo))
    fig, ax1 = plt.subplots(1, 1, figsize = (figsize_x, figsize_y))
    ax1.set_axisbelow(True); ax1.grid()

    for label in range(len(y_train_counts)):
        mask = y_train_full == label
        ax1.scatter(proj1[mask], proj2[mask],
                    label = f"Class {label}", s = 4, color = colors[label])

    ax1.set_xlabel(f"PC 1 ({round(PCA.exp_var[0], 3)} of variance)", fontsize = 14)
    ax1.set_ylabel(f"PC 2 ({round(PCA.exp_var[1], 3)} of variance)", fontsize = 14)

    ax1.legend(frameon = True)
    fig.tight_layout()

    plt.show()


############
## TASK 3 ##
############
kmeans = my_stuff.MyKMeans(k = 5,
                           init = "k-means++",
                           iterations = 200)

print("\n>> Task 4.3: 5-means clustering")
print("Fitting 5-means model ...")
kmeans.fit(X_train_full)
centroids, new_train_labels = kmeans.means, kmeans.labels

if do_plot:
    print("Plotting PCA projection with newly assigned labels ..")

    np.random.seed(2)
    centroids_proj = PCA.project(centroids, n_components = 2)

    fig, ax1 = plt.subplots(1, 1, figsize = (figsize_x, figsize_y))
    ax1.set_axisbelow(True); ax1.grid()

    for label, (x, y) in enumerate(centroids_proj):
        mask = new_train_labels == label
        ax1.scatter(proj1[mask], proj2[mask], color = colors[label],
                    s = 4, alpha = 0.6, zorder = 2)
        ax1.scatter(x, y, label = f"Cluster {label}", color = colors[label],
                    edgecolors = "black", s = 64, zorder = 3)

    ax1.set_xlabel(f"PC 1 ({round(PCA.exp_var[0], 3)} of variance)", fontsize = 14)
    ax1.set_ylabel(f"PC 2 ({round(PCA.exp_var[1], 3)} of variance)", fontsize = 14)

    ax1.legend(frameon = True)
    fig.tight_layout()
    plt.show()


############
## TASK 4 ##
############

## >> LOGISTIC REGRESSION USING PYTORCH
print("\n>> Task 4.4.1: Multinomial logistic regression using pytorch")
import torch
import torch.nn as nn

input_dim   = X_train.shape[1]
fc1_units   = 32
num_classes = len(np.unique(y_train_full))

try:
    model = torch.load("./linreg_pytorch_model.pt")
    print("Loaded pre-trained pytorch model from file. ", end = "")
except:
    print("Failed to load pre-trained model from file. Creating new model ...")
    model = nn.Sequential(
        nn.Linear(input_dim, fc1_units),
        nn.ELU(),
        nn.BatchNorm1d(fc1_units),
        nn.Linear(fc1_units, num_classes),
        nn.Softmax(dim = 1)
    )

    optimizer = torch.optim.Adam(model.parameters(), lr = 1e-4)
    loss_f    = nn.CrossEntropyLoss()


    # >> TRAIN NETWORK
    X_train_T = torch.Tensor(X_train)
    y_train_T = torch.from_numpy(y_train)
    X_val_T   = torch.Tensor(X_val)
    y_val_T   = torch.from_numpy(y_val)

    epochs = 2000
    visualization_update_rate = max(100, epochs // 10)
    validation_rate           = max(10, visualization_update_rate // 10)
    print("Training network ...")
    for epoch in range(1, epochs + 1):

        optimizer.zero_grad()
        predictions = model(X_train_T)
        loss        = loss_f(predictions, y_train_T)
        loss.backward()
        optimizer.step()

        if epoch % validation_rate == 0:
            with torch.no_grad():
                val_preds = model(X_val_T)
                val_loss  = loss_f(val_preds, y_val_T)
                val_accuracy = (val_preds.argmax(1) == y_val_T).float().mean()
                print(f"Epoch {epoch:4} -- val_loss, val_acc = "
                      f"({round(val_loss.item(), 4):6}, "
                      f"{round(val_accuracy.item(), 4):6})\r",
                      end = "", flush = True)
            if epoch % visualization_update_rate == 0:
                print()
    print("Finished training. ", end = "")
    torch.save(model, "./linreg_pytorch_model.pt")

## EVALUATE MODEL ON TEST DATA
X_train_full_T = torch.Tensor(X_train_full)
y_train_full_T = torch.Tensor(y_train_full)
X_test_T = torch.Tensor(X_test)
y_test_T = torch.from_numpy(y_test)

print("Evaluating model ...")
train_preds = model(X_train_full_T)
train_loss = (train_preds.argmax(1) != y_train_full_T).float().mean()
test_preds = model(X_test_T)
test_loss = (test_preds.argmax(1) != y_test_T).float().mean()
print(f"Logreg training loss: {train_loss.item()}\n"
      f"Logreg testing  loss: {test_loss.item()}")



## >> RANDOM FOREST CLASSIFICATION
print("\n>> Task 4.4.2: Random forest classification")
print("All trees trained with max_depth = 12, max_features = 8, "
      "gini impurities, and sample boostrapping enabled.")
from sklearn.ensemble import RandomForestClassifier as RFClassifier

## MODEL TRAINING AND TESTING
for num_trees in [50, 100, 200]:
    print(f"Training RF classifier of {num_trees} trees ...")
    RFC = RFClassifier(n_estimators = num_trees,
                       criterion = "gini", # use Gini impurities.
                       bootstrap = True,   # use bootstrap samples.

                       max_depth = 12,     # found via manual search.
                       max_features = 8,   # found via manual search.

                       n_jobs = -1,        # might aswell parallelize.
                       random_state = 2,   # for reproducibility.
                      )

    RFC.fit(X_train_full, y_train_full)

    train_preds = RFC.predict(X_train_full)
    train_loss  = np.mean(train_preds != y_train_full)
    test_preds  = RFC.predict(X_test)
    test_loss   = np.mean(test_preds != y_test)

    print(f"RF (num_trees = {num_trees}) training loss: {train_loss}")
    print(f"RF (num_trees = {num_trees}) testing  loss: {test_loss}\n")


## >> K-NN CLASSIFICATION
print(">> Task 4.4.3: K-NN Classification")

## 10-FOLD CROSS-VALIDATION TO FIND OPTIMAL K

k_best_overall = 51 # found via grid search.
knn = my_stuff.MyKNN(k = k_best_overall)
knn.fit(X_train_full, y_train_full)

if do_knn_parameter_search:
    grid_init = 2 ** (np.arange(12) + 2) - 1
    print(f"Performing grid pruning before actual grid search (searching "
          "powers of 2 minus 1 between 3 and 8191)")
    k_init = knn.grid_search_k(grid_init)

    print(f"Grid pruning found k = {k_init}.")

    lo = max(grid_init[0], 2 ** int(np.log2(k_init)) + 1)
    hi = min(2 ** int(np.log2(k_init + 1) + 1) - 1, grid_init[-1])

    grid_final = np.arange(lo, hi, 2)
    print(f"Performing grid search on the grid [{lo}, {lo + 2}, ..., {hi}].")
    k_best_overall = knn.grid_search_k(grid_final)

    print(f"Found optimal k = {k_best_overall}.")


print(f"Running k-NN on training and testing data for k = {k_best_overall} "
      "-- this may take a while ...")
train_preds = knn.predict(X_train_full)
test_preds  = knn.predict(X_test)

train_loss = np.mean(train_preds != y_train_full)
test_loss  = np.mean(test_preds  != y_test)
print(f"k-NN training loss for k = {k_best_overall}: {train_loss}")
print(f"k-NN testing  loss for k = {k_best_overall}: {test_loss}")
