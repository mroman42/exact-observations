import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

heatmap_data = np.loadtxt("tempfile.txt", delimiter=",")
sns.heatmap(heatmap_data, cmap="mako")
plt.show()