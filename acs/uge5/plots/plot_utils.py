import numpy as np
import matplotlib.pyplot as plt

red="firebrick"
green="olivedrab"
orange="darkorange"
teal="teal"

gray="gray"
gold="goldenrod"

def speedups(before, after):
    return np.array(before) / np.array(after)


def autolabel(plot, rects, speedups, label_suffix = "x", hor_offs=0.0, rot=90, height_mult=1.02):
    for i in range(len(rects)):
        rect = rects[i]

        height = rect.get_height()
        width  = rect.get_width()

        plot.text(rect.get_x() + hor_offs + width/2., height_mult*height,
               '%10.2f%s' % (speedups[i], label_suffix),
               ha='center', va='bottom', rotation=rot)

def init_plot(title, y=20, x=10, title_size=25):
    fig, plot = plt.subplots(1, 1, figsize=(y, x))
    plot.set_title(title, size=title_size)
    plot.ticklabel_format(style="plain")
    plot.bar
    return (fig, plot)


def finish_plot(plot, xlabel, ylabel, xtickslabels, log=False):

    if log:
        ylabel += " (logarithmic)"

    plot.set_xlabel(xlabel, size=10)
    plot.set_ylabel(ylabel, size=10, rotation=90)

    (handles, _) = plot.get_legend_handles_labels()
    if len(handles) > 0:
        plot.legend(loc='upper left', framealpha=1)

    xticks = np.arange(len(xtickslabels))
    plot.set_xticks(xticks)
    plot.set_xticklabels(xtickslabels, rotation=45)


    plot.grid(zorder=0)

    if log:
        plot.set_yscale('log')
