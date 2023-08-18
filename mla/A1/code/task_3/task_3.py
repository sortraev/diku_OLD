import numpy as np
import matplotlib.pyplot as plt

import my_linreg


do_plot = True
do_save_plots = False

# load data.
data = np.loadtxt("PCB.dt")
X, y = data[:, :-1], data[:, -1]

argsort = X.ravel().argsort()
X = X[argsort]
y = y[argsort]

# compute log of label space, such that S' = (X, log_y).
log_y = np.log(y)



#################
#   task 3.2    #
#################
lin_reg = my_linreg.MyLinReg()
lin_reg.fit(X, log_y)

y_pred = np.exp(lin_reg.get_model()(X))

mse = np.mean((y - y_pred) ** 2)

a, b = lin_reg.w[0].round(3), lin_reg.b.round(3)

print(f">> task 3.2\n"
      f"   - a, b = {a}, {b}\n"
      f"   - MSE  = {mse.round(3)}")

#################
#   task 3.4    #
#################
R2 = 1 - np.sum((y - y_pred) ** 2) / np.sum((y - np.mean(y)) ** 2)
print(f">> task 3.4\n"
      f"   - R2 = {R2.round(3)}")

#################
#   task 3.3    #
#################
if do_plot:
    print(">> task 3.3: plotting ...")
    plt.scatter(X, y, label = "y")
    plt.plot(X, y_pred,
             label = f"h(x) = exp({a}x + {b})", color = "orange")

    plt.yscale("log")

    #  plt.title(f"Linear model\nMSE = {mse.round(3)}, R^2 = {R2.round(3)}")
    plt.xlabel("Fish age in years")
    plt.ylabel("PCB concentration (ppm)")

    plt.grid(zorder = 0)
    plt.legend()
    if do_save_plots:
        print("saving fig3_3")
        plt.savefig("fig3_3.png", bbox_inches = "tight")
    plt.show()

#################
#   task 3.5    #
#################
sqrt_X = np.sqrt(X)

lin_reg.fit(sqrt_X, log_y)


y_pred2 = np.exp(lin_reg.get_model()(sqrt_X))
mse2 = np.mean((y - y_pred2) ** 2)
R2_2 = 1 - np.sum((y - y_pred2) ** 2) / np.sum((y - np.mean(y)) ** 2)
a, b = lin_reg.w[0].round(3), lin_reg.b.round(3)
print(f">> task 3.5\n"
      f"   - a, b = {a}, {b}\n"
      f"   - MSE  = {mse2.round(3)}\n"
      f"   - R^2  = {R2_2.round(3)}")

if do_plot:
    print(">> task 3.5: plotting ...")
    plt.scatter(X, y, label = "y")
    plt.plot(X, y_pred2,
             label = f"h(x) = exp({a}sqrt(x) + {b})",
             color = "orange")

    plt.yscale("log")

    #  plt.title(f"Non-linear model\nMSE = {mse2.round(3)}, R^2 = {R2_2.round(3)}")
    plt.xlabel("Fish age in years")
    plt.ylabel("PCB concentration (ppm)")

    plt.grid(zorder = 0)
    plt.legend()
    if do_save_plots:
        print("saving fig3_5")
        plt.savefig("fig3_5.png", bbox_inches = "tight")
    plt.show()
