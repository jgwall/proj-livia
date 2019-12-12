__author__ = 'jgwall'

import argparse
import livia
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image, ImageOps

debug = False


def main():
    args = parse_args()

    target = Image.open("../smiley_small.png")
    #target = ImageOps.grayscale(target)

    # Test evolution wrapper
    mytest = livia.evolve_images(target,
                                   popsize=10,
                                   selection=0.1,
                                   mutation_rate=0.1,
                                   mutation_sd=0.005,
                                   generations=5000,
                                   gen_interval=250,
                                   verbose=True,
                                   outprefix="poster/progress",
                                   filetype="png",
                                   seed=2 )



    mytest['avg_image'].save("test_generations.png")
    plt.plot(np.array(mytest['avg_fitness']))
    plt.savefig("test_generations.fitness.png")


    # Usefufl for later:
    # Pillow library docs: https://pillow.readthedocs.io/en/stable/handbook/index.html
    # OpenCV library docs: https://docs.opencv.org/trunk/d6/d00/tutorial_py_root.html
    # img.save(path, format) -  Saving an Image


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--infile")
    parser.add_argument("--debug", default=False, action="store_true")
    args = parser.parse_args()

    # Handle debug flag
    global debug
    debug = args.debug

    return parser.parse_args()

def test_stuff():
    test = Image.open("../smiley_small.png")
    test = ImageOps.grayscale(test)

    # Test resize
    test_resize = livia.resize_image(test, 50)
    test_resize = livia.resize_image(test, 5000)
    test_resize.save("test_resize.png")

    test = np.array(test)

    # Test random
    test_random = livia.initialize_random(test)
    Image.fromarray(test_random).save("test_random.png")

    # Test mutation
    test_mutate = livia.mutate(test, mutation_rate=0.5, mutation_sd=1)
    Image.fromarray(test_mutate).save("test_mutate.png")

    # Test average TODO: Still not certain this is doing it correctly
    image_list = [livia.initialize_random(test) for i in range(10)]
    print([i[0:2,0:2] for i in image_list])
    test_average = livia.average_images(image_list)
    print(test_average[0:2,0:2])
    print(test_average.shape)

    # Test fitness
    #print(livia.calc_fitness(test, test))   # Should be 1
    #print(livia.calc_fitness(test_random, test))   # Should be low (~0.5)
    t1 = np.array([[255,255],[255,255]])
    t2 = np.array([[0, 0], [0, 0]])
    t3 = np.array([[0, 255], [0, 255]])
    # print(livia.calc_fitness(t1, t1))  # Should be 1
    # print(livia.calc_fitness(t1, t2))   # Should be 0
    # print(livia.calc_fitness(t1, t3))   # should be ~0.5

    # Test evolve once - TODO: Got functioning (= no errors), but still need to test that working correctly
    test_evolve = livia.evolve_images_once(test)
    # print(test_evolve)

    # Test evolution wrapper TODO: Seems to work. Time to confirm test, and then do speed checks
    test_generations = livia.evolve_images(test, gen_interval=100, verbose=True, generations=1000)
    #print(test_generations)
    Image.fromarray(test_generations['avg_image']).save("test_generations.png")

if __name__ == '__main__': main()