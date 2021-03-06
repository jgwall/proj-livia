# Teaching goals
#  - Can teach basic "random chance + selection can make complex things"
#  - More advanced, can teach mutation-selection balance


library(imager)
library(stringr)
library(assertthat)


#' Resize a [cimg] object to a specified number of max pixels
#'
#' This function resizes a [cimg] object to have at most a specified number of
#' pixels, using the [imager::imresize()] function. (Images already smaller than
#' this are returned unchanged)
#'
#' @param img A [cimg] image object.
#' @param max_pixels The maximum number of pixels allowed. The image will be
#'   resized to be at or just under this many total pixels while keeping the
#'   same aspect ratio. If the image has multiple frames, all are affected.
#' @param verbose TRUE/FALSE flag of whether to output additional information to the console
#'
#' @return The resized [cimg] object, or the original object if it was already
#'   smaller than `max_pixels`.
#' @export
#'
#' @examples
resize_image = function(img, max_pixels, verbose = FALSE) {
    orig_size = height(img) * width(img)
    # If pic is fine (=fewer than max pixels), don't do anything
    if (orig_size <= max_pixels) {
        if (verbose) {
            cat("Image is already below",max_pixels,"pixels; returning unchanged\n")
        }
        return(img)
    }
    
    # Resize anything that is too big
    if (verbose) {
        cat("Resizing image to below", max_pixels, "pixels\n")
    }
    area_ratio = max_pixels / orig_size
    new = imresize(img, scale = sqrt(area_ratio))
    
    # Return altered image
    return(new)
}


#' Create a random image with the same dimensions as a provided image
#'
#' This function uses a template [cimg] image to create a random image of the
#' same size and color depth (RGB or grayscale).
#'
#' @param img A [cimg] image object.
#'
#' @return a [cimg] image object of the same size but with random pixels.
#' @export
#'
#' @examples
initialize_random = function(img) {
    newimage = img
    numpix = width(newimage) * height(newimage)
    # Go through available color channels and randomize to 0-1 range
    for (channel in 1:spectrum(newimage)) {
        new_values = runif(numpix)
        newimage[, , , channel] = new_values
    }
    return(newimage)
}

#' Mutate an image object by randomly changing some pixels
#'
#' This function takes a [cimg] image and "mutates" it to randomly change some
#' pixels by a specified amount. The mutation rate and distribution of effects
#' can both be controlled by function arguments.
#'
#' @param img The [cimg] image object to be mutated.
#' @param mutation_rate The probability that any given pixel's value will
#'   mutate.
#' @param mutation_mean The mean of the normal distribution from which mutation
#'   effects are taken. (Generally 0).
#' @param mutation_sd The standard deviation of the normal distribution from
#'   which mutation effects are taken. Larger values are more extreme mutations.
#'   (Remember that [cimg] color values go from 0 to 1, so a `mutation_sd``
#'   value of 1 is actually pretty high.).
#'
#' @return a mutated [cimg] object
#' @export
#'
#' @examples
mutate = function(img,
                  mutation_rate = 0.1,
                  mutation_mean = 0,
                  mutation_sd = 0.1) {
    # Determine pixels to mutate; faster to use sample() than rbinom()
    tochange = sample(
        c(T, F),
        size = nPix(img),
        replace = T,
        prob = c(mutation_rate, 1 - mutation_rate)
    )
    tochange = which(tochange)
    
    # Get size of mutation
    changes = rnorm(n = length(tochange),
                    mean = mutation_mean,
                    sd = mutation_sd)
    
    # Mutate pixes
    img[tochange] = img[tochange] + changes

    # Fix any pixels that are outside of [0,1] and return the fixed image
    to_zero = which(img[tochange] < 0)
    to_one = which(img[tochange] > 1)
    
    img[tochange[to_zero]] = 0
    img[tochange[to_one]] = 1
    
    return(img)
}

#' Average all images in a list
#'
#' Take a list of [cimg] objects and average them together. All images should
#' have the same dimenstions (height, width, frames, color depth)
#'
#' @param images A [list] of [cimg] objects, all of the same size (height,
#'   width, frames, color depth)
#'
#' @return A single [cimg] object representing the average across all the provided images
#' @export
#'
#' @examples
average_images = function(images) {
    avg = images[[1]]
    for (i in 2:length(images)) {
        avg = avg + images[[i]]
    }
    avg = avg / length(images)
    return(avg)
}

#' A convenience function to evolve images a specific number of generations and return the cumulative result
#'
#' @param target A [cimg] image object used to select the population of images each generation.
#' @param generations How many generations to let the evolution go for.
#' @param gen_interval How often to output intermediate progress & files. (so gen_interval=100 means "every 100 generations")
#' @param outprefix Output fiel prefix for saving intermediate steps
#' @param verbose If TRUE, print out progress reports of population fitness every few generations.
#' @param seed The seed for randomization
#' @param ... Other arguments to be passed on to evolve_image_once(), including mutation size, population size, etc.
#'
#' @return a [list] of 3 components: 'pop' being a [list] of [cimg] objects
#'   representing the current population state, 'avg_fitness' a [numeric] vector
#'   recording the fitness history of the population over time, and 'avg_image',
#'   the average of the current population
#' @export
#'
#' @examples
evolve_images = function(target,
                         generations = 1000,
                         gen_interval = NULL,
                         outprefix = NULL,
                         verbose = FALSE,
                         seed = NULL,
                         ...) {
    # Set random seed if supplied
    if (!is.null(seed)) {
        set.seed(seed)
    }
    
    
    # Do evolution
    current_pop = list()
    for (i in 1:generations) {
        current_pop = evolve_images_once(target = target,
                                         current_pop = current_pop,
                                         ...)

        # If generation interval supplied, do progress checks & output
        if (!is.null(gen_interval) && (i==1 || i %% gen_interval == 0) )  { # If gen interval specified, print out initial conditions or every N generations
            # Print progress reports
            if (verbose) {
                cat(
                    "Generation",
                    i,
                    ": Average population fitness is",
                    current_pop$avg_fitness[i],
                    "\n"
                )
            }
            
            # Save intermediate progress
            if (!is.null(outprefix)) {
                mygen = str_pad(i, ceiling(log10(generations)), pad = '0')
                saveRDS(current_pop,
                        file = paste(outprefix, mygen, "RDS", sep = "."))
                save.image(current_pop$avg_image,
                           file = paste(outprefix, mygen, "png", sep = "."))
            }
        }
        
    }
    
    return(current_pop)
}

#' Evolve an image for a single generation
#'
#' @param target A [cimg] object (image) of what the population is being evolved toward. This is used to determine the fitness of all the progeny.
#' @param current_pop A [list] recording the current state of the population. If [NULL], a new one is initialized with random values
#' @param popsize [Integer] value indicating how large the population is each generation
#' @param selection [Numeric] value in the range of 0-1 indicating how intense
#'   selection is. Zero means nothing is selected (and is not a legal value), 1
#'   means everything is selection. The population size divided by the selection
#'   intensity determines how many progeny are created.
#' @param mutation_rate 
#' @param mutation_mean 
#' @param mutation_sd 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
evolve_images_once = function(target,
                              current_pop = list(),
                              popsize = 10,
                              selection = 0.1,
                              mutation_rate = 0.1,
                              mutation_mean = 0,
                              mutation_sd = 0.15,
                              verbose = FALSE) {
    
    # Sanity checks
    assert_that(selection > 0 && selection <= 1)
    
    # Make initial population if none is supplied
    if (is.null(current_pop$pop)) {
        pop = list()
        for (i in 1:popsize) {
            pop[[i]] = initialize_random(target)
        }
        
        fitness = sapply(pop, get_fitness, target = target)
        current_pop = list(
            pop = pop,
            avg_fitness = mean(fitness),
            avg_image = average_images(pop)
        )
        
    }
    
    # Do evolution
    
    # Replicate
    replicate_size = popsize / selection
    children = sample(current_pop$pop, size = replicate_size, replace = T)
    
    # Mutate (note: lapply() faster than mclapply() here)
    children = lapply(
        children,
        mutate,
        mutation_rate = mutation_rate,
        mutation_mean = mutation_mean,
        mutation_sd = mutation_sd
    )
    
    #   # Score fitness by correlation with the target image - DEPRECATED
    #   fitness = sapply(children, cor, y = target)
    
    # Score fitness based on sum of differences with the target
    fitness = sapply(children, get_fitness, target = target)
    
    
    # Keep the best X ones to maintain population size
    tokeep = order(fitness, decreasing = T)[1:popsize]
    pop = children[tokeep]
    
    # Determine average fitness
    avg_fitness = mean(fitness[tokeep])
    
    result = list(
        pop = pop,
        avg_fitness = c(current_pop$avg_fitness, avg_fitness),
        avg_image = average_images(pop)
    )
    return(result)
}

#' Title
#'
#' @param child 
#' @param target 
#'
#' @return
#' @export
#'
#' @examples
get_fitness = function(child, target) {
    num_pixels = nPix(target)
    (num_pixels - sum(abs(target - child))) / num_pixels    # Expresses fitness as a fraction of the maximum possible distance
}

#' Get mutation size spectrum
#'
#' @param mutation_mean The mean mutation size
#' @param mutation_sd The standard deviation of mutation sizes
#'
#' @return A [data.frame] suitable for plotting the mutation sizes
#' @export
#'
#' @examples
get_mutation_sizes = function(mutation_mean, mutation_sd) {
    # Basic distribution
    points = seq(from = -1, to = 1, by = 0.02)
    density = dnorm(points, mean = mutation_mean, sd = mutation_sd)
    
    # Add in things outside the -1, 1 range
    lower_tail = pnorm(-1,
                       mean = mutation_mean,
                       sd = mutation_sd,
                       lower.tail = TRUE)
    upper_tail = pnorm(1,
                       mean = mutation_mean,
                       sd = mutation_sd,
                       lower.tail = FALSE)
    
    density[1] = density[1] + lower_tail
    density[length(density)] = density[length(density)] + upper_tail
    
    # Return
    return(data.frame(size = points, density = density))
}
