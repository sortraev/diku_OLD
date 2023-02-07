import matplotlib
from plot_utils import *

ns = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512]
xs = np.arange(len(ns))

local_thruput    = [8542.36, 12224.90, 20144.85, 84987.71, 628257.13, 2666820.00, 2402244.75, 3433484.75, 5279553.00, 1634891.75]
nonlocal_thruput = [ 129.54,   297.97,  1799.51,  2989.76,   7791.99,   13105.41,   29130.35,   43557.34,   29319.18,   93259.64]



fig, plot = init_plot("Plot1: CertainWorkload throughput (higher is better)", y = 16, x = 8, title_size = 16)

plot.scatter([1], [max(local_thruput) + 2*10**7], color='white')
local_bars    = plot.bar(xs-0.075, local_thruput,    0.15, zorder=3, color=teal,
                         label="Local thruput")
nonlocal_bars = plot.bar(xs+0.075, nonlocal_thruput, 0.15, zorder=3, color=red,
                         label="Nonlocal thruput")

#  plot.plot(xs, local_thruput,    0.15, zorder=3, color=red,  label="Local")
#  plot.plot(xs, nonlocal_thruput, 0.15, zorder=3, color=teal, label="Nonlocal")


#  ispc_speedups = speedups(scan_c, scan_ispc)

autolabel(plot, local_bars, local_thruput, label_suffix="")
autolabel(plot, nonlocal_bars, nonlocal_thruput, label_suffix="")
finish_plot(plot=plot,
            xlabel="Number of worker threads",
            ylabel="Throughput (number of successful interactions per second)",
            xtickslabels=ns, log=True)

fig.savefig("certainbookstore_thruput.png", bbox_inches="tight")





local_latency    = [0.12, 0.25, 0.53,  0.62,  0.35,  0.73,  5.89,  20.10,   51.94,  269.64]
nonlocal_latency = [7.72, 9.98, 5.70, 12.42, 19.53, 39.60, 73.12, 189.43, 1128.74, 1412.24]
fig, plot = init_plot("Plot 2: CertainWorkload latency (lower is better)", y = 16, x = 8, title_size = 16)


plot.scatter([1], [4000], color='white')
local_bars    = plot.bar(xs-0.075, local_latency,    0.15, zorder=3, color=green,
                         label="Local latency")
nonlocal_bars = plot.bar(xs+0.075, nonlocal_latency, 0.15, zorder=3, color=orange,
                         label="Nonlocal latency")


#  ispc_speedups = speedups(scan_c, scan_ispc)

autolabel(plot, local_bars, local_latency, label_suffix = "")
autolabel(plot, nonlocal_bars, nonlocal_latency, label_suffix = "")
finish_plot(plot=plot,
            xlabel="Number of worker threads",
            ylabel="Latency in milliseconds of successful interactions",
            xtickslabels=ns, log=True)

fig.savefig("certainbookstore_latency.png", bbox_inches="tight")
