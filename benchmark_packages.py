__author__ = 'jgwall'

import argparse
import random
import numpy as np
import livia
import itertools
from PIL import Image, ImageOps
import matplotlib.image as image
from pycallgraph2 import PyCallGraph
from pycallgraph2.output import GraphvizOutput


debug = False

# Need to benchmark the different potential packages to see which is fastest, or if makes a difference.
# Mutate() function took the most time in R, so presumably also will in Python, so start with that.
#   Also benchmark fitness calculations

# Global variables
mutation_rate = 0.1
mutation_mean = 0
mutation_sd = 0.1
grayscale=False

def main():

    target = Image.open("../smiley_small.png")
    target = ImageOps.grayscale(target)

    # Test evolution wrapper


    with PyCallGraph(output=GraphvizOutput()):
        mytest = livia.evolve_images(target,
                                   popsize=10,
                                   selection=0.1,
                                   mutation_rate=0.1,
                                   mutation_sd=0.01,
                                   generations=1000,
                                   #gen_interval=100,
                                   verbose=True,
                                   #outprefix="tmp/test",
                                   seed=1 )


    # # Convert to numpy array (presumably faster than a PIl Image
    # #example = image.pil_to_array(example)  # Maintains non-writable flags for some reason
    # example = np.array(example)
    #
    # # Get locations to change
    # random.seed(1)  # Set random seed
    # tochange = np.random.choice((True, False), size=example.size, p=[mutation_rate, 1-mutation_rate])
    # tochange = np.reshape(tochange, newshape = example.shape)
    # indices = np.argwhere(tochange)   # Get the indices involved
    # print(indices.shape)
    #
    # # Alter pixels
    # for i in range(1000):
    #     changes = np.random.normal(loc=mutation_mean, scale = mutation_sd, size = indices.shape[0]) * 255
    #     newvals = example[tochange] + np.array(changes, dtype = example.dtype)
    #     newvals [newvals < 0] = 0
    #     newvals[newvals > 255] = 255
    #     example[tochange] = newvals



    # Pillow

    # scikit-image

    # matplotlib

    # scipy.ndimage

    # Open-CV-python


# def mutate_pillow(img):

#     # Mutate pixes
#     img[tochange] = img[tochange] + changes
#
#     # Fix any pixels that are outside of [0,1] and return the fixed image
#     to_zero = which(img[tochange] < 0)
#     to_one = which(img[tochange] > 1)
#
#     img[tochange[to_zero]] = 0
#     img[tochange[to_one]] = 1
#
#     return (img)







if __name__ == '__main__': main()