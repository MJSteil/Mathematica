import numpy
from matplotlib import cm
import csv
maps = ['viridis', 'plasma','Spectral','jet']

for m in maps:
    cmap = cm.get_cmap(m)
    with open(m+'.csv', 'w', newline='') as file:
        writer = csv.writer(file)
        for x in numpy.linspace(0, 1, 101):
            writer.writerow(['{:.8e}'.format(i) for i in ((x,)+cmap(x))])
            
#import numpy as np
#import matplotlib.pyplot as plt
#from matplotlib import cm
#from matplotlib.colors import ListedColormap, LinearSegmentedColormap

#viridis = cm.get_cmap('viridis', 100).colors
#np.savetxt("cm_viridis.csv", viridis, delimiter=",")

#plasma = cm.get_cmap('plasma', 100).colors
#np.savetxt("cm_plasma.csv", plasma, delimiter=",")