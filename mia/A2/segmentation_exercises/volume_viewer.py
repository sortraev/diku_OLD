import matplotlib.pyplot as plt
import numpy as np

class VolumeViewer():
    def __init__(self, vol, index = [0, 0, 0], dim = 2):

        assert vol.ndim == 3

        self.vol   = vol
        self.shape = self.vol.shape

        self.current_dim = dim if 0 <= dim < 3 else 2

        self.index = index if len(index) == 3 and\
                                index[0] < self.shape[0] and\
                                index[1] < self.shape[1] and\
                                index[2] < self.shape[2]\
                      else [0, 0, 0]

    def set_dim(self, new_dim):
        if new_dim not in [0, 1, 2] or new_dim == self.current_dim:
            return False
        self.current_dim = new_dim
        return True

    def inc_index(self):
        dim = self.current_dim
        self.index[dim] = (self.index[dim] + 1) % self.shape[dim]
    def dec_index(self):
        dim = self.current_dim
        self.index[dim] = (self.index[dim] - 1) % self.shape[dim]

    def inc_dim(self):
        self.current_dim = (self.current_dim + 1) % 3
    def dec_dim(self):
        self.current_dim = (self.current_dim - 1) % 3

    def get_current_image(self):
        dim = self.current_dim
        return self.vol.take(self.index[dim], axis = dim)

    def get_current_dim_and_index(self):
        return self.current_dim, "xyz"[self.current_dim], self.index

def process_key(event):
    key = event.key
    if key in "hjklxyz":
        fig = event.canvas.figure
        ax = fig.axes[0]
        if key == 'h':
            ax.volviewer.dec_index()
        elif key == 'l':
            ax.volviewer.inc_index()
        elif key == 'k':
            ax.volviewer.inc_dim()
        elif key == 'j':
            ax.volviewer.dec_dim()
        else:
            new_dim = ord(key) - 120
            if not ax.volviewer.set_dim(new_dim):
                return

        ax.images[0].set_array(ax.volviewer.get_current_image())

        _, dim_str, index = ax.volviewer.get_current_dim_and_index()
        fig.suptitle(f"Current dim: {dim_str}\nIndex: {index}")
        fig.canvas.draw()

def remove_keymap_conflicts(new_keys_set):
    for prop in plt.rcParams:
        if prop.startswith('keymap.'):
            keys = plt.rcParams[prop]
            remove_list = set(keys) & new_keys_set
            for key in remove_list:
                keys.remove(key)

def volume_viewer(volume, index = [0, 0, 0], dim = 2):
    remove_keymap_conflicts({'j', 'k', 'l', 'h', 'x', 'y', 'z'})
    fig, ax = plt.subplots()
    ax.volviewer = VolumeViewer(volume, index, dim)
    ax.imshow(ax.volviewer.get_current_image())

    _, dim_str, index = ax.volviewer.get_current_dim_and_index()
    fig.suptitle(f"Current dim: {dim_str}\nIndex: {index}")
    fig.canvas.mpl_connect('key_press_event', process_key)
