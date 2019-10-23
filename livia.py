__author__ = 'jgwall'

from math import sqrt, ceil, log10
import numpy as np
import pickle
from PIL import Image, ImageOps   # not sure I want to keep this. Do OpenCV instead?
import random

# Teahcing goals
#  - Can teach basic "random chance + selection can make complex things"
#  - More advanced, can teach mutation-selection balance

# library(imager)
# library(stringr)

# Resize an Image object to a specified number of max pixels
# TODO: Unit testing for actual resizing, skipping things below specified sizes, illegal sizes (negatives, 0), etc.
def resize_image(img: Image, max_pixels: int):
    width, height = img.size
    orig_size = width * height

    # If pic is fine (=fewer than max pixels), don't do anything
    if orig_size <= max_pixels:
        print("Image is already below", max_pixels, "pixels; returning unchanged\n")
        return (img)

    # Resize anything that is too big
    print("Resizing image to below", max_pixels, "pixels\n")
    area_ratio = sqrt(max_pixels / orig_size)
    new_size = np.array([area_ratio * width, area_ratio * height], dtype=int)
    new = img.resize( new_size, resample = Image.BICUBIC )

    # Return altered image
    return (new)



#  Create a random image with the same dimensions as a provided image
def initialize_random(img: np.ndarray):
    #values = np.random.randint(low=0,high=255, size=img.size)
    values = np.random.random(size = img.size)
    newimage = np.reshape(values, newshape=img.shape)
    return newimage


# Mutate an image object by randomly changing some pixels
def mutate(img:np.ndarray, mutation_rate = 0.1, mutation_mean = 0, mutation_sd = 0.1):
    # Determine pixels to mutate
    tochange = np.random.choice((True, False), size=img.size, p=[mutation_rate, 1 - mutation_rate])
    tochange = np.reshape(tochange, newshape=img.shape)

    # Get the indices involved
    indices = np.argwhere(tochange)    # Initial method
    #indices = np.transpose(np.unravel_index(np.flatnonzero(tochange), tochange.shape))  # Faster method from hpaulj on StackOverflow
    # TODO: Need to test that this actually still functions properly

    # Alter pixels
    changes = np.random.normal(loc=mutation_mean, scale=mutation_sd, size=indices.shape[0])
    newvals = img[tochange] + np.array(changes)
    newvals[newvals < 0] = 0
    newvals[newvals > 1] = 1
    img[tochange] = newvals
    return img


#  Average all images in a list
def average_images(images):
    total = np.sum(np.array(images), axis=0)
    avg = np.array(total / len(images), dtype = images[0].dtype)    # TODO: This truncates, whereas rounding would be better. How to fix?
    return avg

# Convert PIL Image (range 0-255) to numpy float array in range 0-1. This allows more granularity in evolution so it proceeds faster (and is how the R app works)
def pil_to_numpy(image):
    converted = np.array(image, dtype=float)
    converted /= 255
    return converted

# Convert numpy float array in range 0-1 to PIL Image (range 0-255)
def numpy_to_pil(image):
    converted = image * 255
    converted = np.around(converted, decimals=0)
    converted = np.array(converted, dtype="uint8")  # Get back into the correct integer format
    return Image.fromarray(converted)


# A convenience function to evolve images a specific number of generations and return the cumulative result
def evolve_images(target, generations = 1000, gen_interval = None, outprefix = None, verbose = False, seed = None, *args, **kwargs):

    # Set random seed if supplied
    if seed:
        np.random.seed(seed)

    # Convert target to a numpy array
    target = pil_to_numpy(target)

    # Do evolution
    current_pop = dict()
    for i in range(generations):
        current_pop = evolve_images_once(target=target, current_pop=current_pop, *args, **kwargs)

        # Print out progress every n generations
        if (verbose and (gen_interval is not None) and (i % gen_interval == 0)):
            print("Generation " + str(i) + ": Average population fitness is " + str(current_pop['avg_fitness'][i]))

        # Save intermediate progress every n generations
        if ( (outprefix is not None) and (gen_interval is not None) and (i % gen_interval == 0)):
            # Get output file name
            num_digits = ceil(log10(generations))
            mygen = str(i).zfill(num_digits)
            filestem = outprefix + "." + mygen
            if verbose:
                print("\tSaving progress to intermediate files with output prefix " + filestem)
                print("\t\tCurrent image has size",current_pop['avg_image'].shape)


            # Save image and data
            numpy_to_pil(current_pop['avg_image']).save(filestem + ".png")
            pickle.dump(current_pop, file=open(filestem + ".pickle", 'wb'))

    # Convert working values to more manageable ones
    #current_pop['avg_fitness'] = np.array(current_pop['avg_fitness']) / (target.size)  # Normalize fitness to 0-1 scale
    current_pop['pop'] = [numpy_to_pil(p) for p in current_pop['pop']]

    print("Final image has size", current_pop['avg_image'].shape)
    current_pop['avg_image'] = numpy_to_pil(current_pop['avg_image'])
    print("\tChanged to", current_pop['avg_image'])

    return current_pop



# Function to evolve images for the Shiny app
def evolve_images_once(target,
    current_pop = dict(),   # Dictionary with "pop", "avg_fitness", and "avg_image" -> Or define my own class?
    popsize = 10,
    selection = 0.1,
    mutation_rate = 0.1,
    mutation_mean = 0,
    mutation_sd = 1):

    # Make initial population if none is supplied
    if "pop" not in current_pop.keys():     # TODO: make a class to store results instead of a dict?
        pop = [initialize_random(target) for i in range(popsize)]
        fitness = [calc_fitness(p, target) for p in pop]
        current_pop = dict(pop=pop,
                           avg_fitness=[np.mean(fitness)],
                           avg_image=average_images(pop))

    # Do evolution

    # Replicate
    replicate_size = int(popsize / selection)
    child_indices = np.random.choice(range(len(current_pop["pop"])), size = replicate_size, replace = True)
    children = np.array(current_pop["pop"])[child_indices]

    # Mutate
    children = [mutate(child, mutation_rate=mutation_rate, mutation_mean=mutation_mean, mutation_sd=mutation_sd) for child in children]

    # Score fitness based on sum of differences with the target
    fitness = [calc_fitness(child, target) for child in children]

    # Keep the best X ones to maintain population size
    tokeep = np.argsort(fitness)[-popsize:]
    pop = np.array(children)[tokeep]

    # Determine average fitness
    avg_fitness = np.mean(np.array(fitness)[tokeep])

    # Compile results to return
    result = dict(pop=pop,
                  avg_fitness= current_pop['avg_fitness'] + [avg_fitness],
                  avg_image = average_images(pop))
    return result


# Score fitness based on sum of differences with the target
def calc_fitness(image, target):    # TODO: Need to confirm this calculating values correctly
    max_score = target.size
    differences = np.sum(np.abs(target - image))  # Sum of total differences between image and target
    fitness =  (max_score - differences) / max_score   # Normalized fraction of similarity to target image
    return fitness


#
# # ' Get mutation size spectrum
# # '
# # ' @param mutation_mean The mean mutation size
# # ' @param mutation_sd The standard deviation of mutation sizes
# # '
# # ' @return A [data.frame] suitable for plotting the mutation sizes
# # ' @export
# # '
# # ' @examples
# get_mutation_sizes = function(mutation_mean, mutation_sd)
# {
# # Basic distribution
# points = seq(
# from = -1, to = 1, by = 0.02)
# density = dnorm(points, mean=mutation_mean, sd=mutation_sd)
#
# # Add in things outside the -1, 1 range
# lower_tail = pnorm(-1,
#                    mean=mutation_mean,
#                    sd=mutation_sd,
#                    lower.tail = TRUE)
# upper_tail = pnorm(1,
#                    mean=mutation_mean,
#                    sd=mutation_sd,
#                    lower.tail = FALSE)
#
# density[1] = density[1] + lower_tail
# density[length(density)] = density[length(density)] + upper_tail
#
# # Return
# return (data.frame(size=points, density=density))
# }
