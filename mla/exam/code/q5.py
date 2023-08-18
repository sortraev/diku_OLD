import numpy as np
import argparse
import my_stuff

parser = argparse.ArgumentParser(prog = 'Q5 solution')
parser.add_argument("--datadir", default = "./data/",
                    help = "path to train/test data (default: ./data/)")
args = parser.parse_args()
data_dir = args.datadir + (args.datadir[-1] != '/') * '/'

X = np.loadtxt(data_dir + "smallFishInput.dt")
y = np.loadtxt(data_dir + "smallFishLabel.dt")

model = my_stuff.MyLinReg()
model.fit(X, y)

w, b = model.get_model_params()
predictions = model.predict(X)

mse = np.mean((predictions - y) ** 2)
sample_var = y.var()
R2 = 1 - (mse / sample_var)

print(f"model parameters: w = {w.round(3)}, b = {b.round(3)}\n"
      f"model MSE:       {mse.round(3)}\n"
      f"sample variance: {y.var().round(3)}\n"
      f"R^2:             {R2.round(3)}")
