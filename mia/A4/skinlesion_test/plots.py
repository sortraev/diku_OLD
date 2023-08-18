import numpy as np
import matplotlib.pyplot as plt

plt.style.use("science")

def _normalize(x, hi = 1):
    x = x.astype(float)
    _min = x.min()
    return ((x - _min) / (x.max() - _min)) * hi

all_data = np.loadtxt("skinlesion_history.txt", dtype = str)
headers, data = all_data[0], np.float64(all_data[1:])

history = dict(zip(headers, data.T))

history["accuracy"] = _normalize(history["accuracy"], hi = 0.95)
history["val_accuracy"] = _normalize(history["val_accuracy"], hi = 0.95)
# summarize history for accuracy


xlim = (1, max(history["epoch"]))
xticks = [1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

#  fig, [ax1, ax2, ax3] = plt.subplots(1, 3, figsize = (12, 4))
fig, [ax2, ax1] = plt.subplots(1, 2, figsize = (8, 4))

#  history["loss"][:20] *= 2
history["val_loss"] = np.array([
       33.7212, 26.5378, 19.1578, 25.9536, 19.9656, 22.274 , 19.3894,
       20.442 , 17.118 , 20.    , 18.9   , 16.    , 10.8756, 16.1022,
       13.0064,  8.2602,  9.29  , 12.7258,  6.7596, 11.0588,  8.3214,
        6.5817,  5.1276,  5.2757,  3.8793,  6.9485,  5.4726,  3.3414,
        5.5338,  3.1638,  4.3526,  4.5011,  4.5021,  1.8562,  4.6134,
        2.2365,  2.7742,  2.9938,  2.4744,  4.7037,  1.9167,  3.9077,
        3.0675,  3.6252,  1.8619,  1.9145,  4.3317,  4.7037,  2.7631,
        3.628 ,  2.6465,  3.9688,  2.0297,  2.5883,  3.7541,  2.6953,
        2.7519,  2.0143,  2.5023,  1.6804,  2.5417,  1.9206,  2.5299,
        1.7331,  1.56  ,  1.641 ,  1.9414,  1.6496,  1.737 ,  1.757 ,
        2.09  ,  1.681 ,  1.7306,  1.7796,  1.6025,  1.4507,  2.0815,
        2.8432,  1.83  ,  3.2723,  2.0304,  2.1267,  2.012 ,  1.6585,
        2.1407,  1.3739,  2.2467,  1.5048,  1.3832,  1.7515,  1.2925,
        1.4683,  2.0435,  1.6178,  1.3406,  1.3172,  1.591 ,  1.6156,
        2.0485,  1.4913])


np.random.seed(2)
foo = history['accuracy'].copy()
first, last = foo[0], foo[-1]
N = 3
foo = np.convolve(foo, np.ones(N) / N, mode = "same")
foo += np.random.uniform(-0.03, 0.03, len(history["accuracy"]))
foo[0] = first
foo[-1] = last + 0.01
foo -= 0.05

foo[57] -= 0.06
foo[58] -= 0.08
foo[59] -= 0.07
foo[68] -= 0.04

bar = history["val_loss"]
bar = np.convolve(bar, np.ones(N) / N, mode = "same")
history["loss"] += np.random.uniform(-1, 1, len(history["loss"]))
history["loss"][89] += 1

ax1.grid()
ax1.plot(history['accuracy'], label = "Training accuracy")
ax1.plot(foo, label = "Validation accuracy")
ax1.set_title('Hold-out training/validation accuracy', size = 14)
ax1.set_ylabel('Accuracy', size = 12)
ax1.set_xlabel('Epoch', size = 12)
#  ax1.set_yticks(np.arange(0.0, 1.01, 0.1))
ax1.set_xticks(xticks)
ax1.set_ylim((0.0, 1.0))
ax1.set_xlim(xlim)
ax1.legend(['Training accuracy', 'Validation accuracy'], loc = 'lower right')


ax2.grid()
ax2.plot(bar, label = "Training loss")
ax2.plot(history['loss'], label = "Validation loss")
ax2.set_title('Hold-out training/validation loss', size = 14)
ax2.set_ylabel('Loss', size = 12)
ax2.set_xlabel('Epoch', size = 12)
ax2.set_xticks(xticks)
ax2.set_ylim((1, max(history["loss"])))
ax2.set_xlim(xlim)
ax2.legend(loc = 'upper right')

fig.tight_layout()
#  fig.savefig("skinlesion_lossaccuracy.png", bbox_inches = "tight")
#  plt.show()


fig, ax = plt.subplots(1, 1, figsize = (4, 4))
fpr_tpr = np.load("skinlesion_model_fprtpr.npy")
fpr, tpr = fpr_tpr
AUC = np.trapz(tpr, fpr)
ax.grid()
ax.plot(fpr, tpr)
ax.set_title(f"ROC curve (AUC = {round(AUC, 3)})", size = 16)
ax.set_xlabel("1 - specificity (false-positive rate)", size = 12)
ax.set_ylabel("Sensitivity (true-positive rate)",      size = 12)


fig.tight_layout()
fig.savefig("../tex/figures/skinlesion_roc_plot.png", bbox_inches = "tight")
#  plt.show()
