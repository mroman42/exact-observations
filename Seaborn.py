import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

heatmap_data = np.array(
    [[1, 2, 3, 4, 4],
     [4, 2, 3, 3, 4],
     [9, 1, 0, 10, 2]]
)

sns.heatmap(heatmap_data, cmap="mako")
plt.show()