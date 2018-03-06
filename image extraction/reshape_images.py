# -*- coding: utf-8 -*-

# Imports
import numpy as np
import re
import os
from scipy import ndimage, misc

# import theano
# from keras.models import Sequential
# from keras.layers import Dense, Dropout, Activation, Flatten
# from keras.layers import Convolution2D, MaxPooling2D
# from keras.utils  import np_utils

# settings and seed
np.random.seed(123)  # for reproducibility


# Import and transform images to csv
images = []
for root, dirnames, filenames in os.walk("../2018-02-04-Calgary-AB/"):
    for filename in filenames:
        if re.search("\.(jpg|jpeg|png|bmp|tiff)$", filename):
            filepath = os.path.join(root, filename)
            image = ndimage.imread(filepath, mode="RGB")
            image_resized = misc.imresize(image, (64, 64))
            images.append(image_resized)

print(images.shape)


# # Print info on shapes and reshape where necessary
# print("Original x shape:", x.shape)
# X = x.reshape((400, 4096))
# print("New x shape:", X.shape)
# print("y shape", y.shape)

# Save the numpy arrays
# np.savetxt("./olivetti_X.csv", X, delimiter = ",")
# np.savetxt("./olivetti_y.csv", y, delimiter = ",", fmt = '%d')

print("\nDownloading and reshaping done!",images)