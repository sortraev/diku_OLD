import sys
import numpy as np
import matplotlib.pyplot as plt


# task 1
def q1(p,
       alpha_lo = None,
       alpha_hi = 1,

       n = 1000000,

       do_show = True,
       png_out = None,
       rand_seed = 2): # the seed used in my plots.

    np.random.seed(rand_seed)

    if alpha_lo is None:
        alpha_lo = p

    # alphas and empirical frequencies.
    alphas    = np.arange(alpha_lo, alpha_hi + 0.05, 0.05).reshape((-1, 1))
    samples   = np.random.binomial(np.ones(n * 20, dtype = int), p).reshape((-1, 20))
    emp_freqs = (samples.mean(1) >= alphas).mean(1)

    # bounds.
    markov_bounds    = p / alphas
    chebyshev_bounds = (p * (1 - p) / (20 * (alphas - p)**2)).clip(0, 1)
    hoeffding_bounds = np.exp(-40 * (alphas - p) ** 2)

    # plotting.
    fig, ax = plt.subplots(1, 1, figsize = (12, 6))
    ax.grid()

    ax.scatter(alphas, emp_freqs, label = r"Frequency of $\overline{X}_{20} \geq \alpha$",
               color = "red", marker = 'x', s = 50)

    ax.plot(alphas, markov_bounds,    label = "Markov bounds")
    ax.plot(alphas, chebyshev_bounds, label = "Chebyshev bounds")
    ax.plot(alphas, hoeffding_bounds, label = "Hoeffding bounds")

    ax.set_ylabel("Frequency")
    ax.set_xlabel(r"$\alpha$")
    ax.set_xticks(alphas)
    ax.legend(loc = "upper right")
    fig.suptitle(rf"Markov's, Chebyshev's, and Hoeffding's bounds for $p = ${p}")

    if png_out is not None:
        fig.tight_layout()
        fig.savefig(png_out, bbox_inches = "tight")
    if do_show:
        plt.show()

def q2():
    pass

if __name__ == "__main__":
    q1(float(sys.argv[1]) if len(sys.argv) > 1 else 0.5)

    # plots for the report.
    #  q1(p = 0.5, alpha_lo = 0.5, png_out = "task_1_a.png")
    #  q1(p = 0.1, alpha_lo = 0.1, png_out = "task_1_b.png")
