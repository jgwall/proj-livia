#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
source("breedart.r")

# TODO: Convert to shinydashboard and use collapsable boxes for left-hand menu items? Or do a 3-column layout?
# TODO: Any use of including icon() calls to make prettier?
# Define UI for application that draws a histogram

################
# Header layout
################
header = dashboardHeader(title = "Livia - Exploring Evolution with Images",
                         titleWidth = "40%")

################
# Sidebar layout
################
sidebar = dashboardSidebar(disable = TRUE)
#

###################
# Main panel layout
###################
body =  dashboardBody(fluidRow(
  column(
    width = 4,
    # Box for file input parameters
    box(
      title = "File Input Options",
      width = "30%",
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      background = 'navy',
      
      # Input file selector
      fileInput("infile", "Target image (file)"),
      # Maximum pixels allowed
      numericInput(
        "max_pixels",
        "Maximum pixels allowed",
        value = 1000,
        min = 1,
        max = 10000,
        step = 500
      ),
      # Grayscale image?
      checkboxInput("grayscale", "Grayscale",
                    value = FALSE, width = '100%'),
      # Run button
      actionButton("run", "Evolve!")
    )
  ),
  
  # Population parameters
  column(
    width = 4,
    box(
      title = "Population parameters",
      width = "30%",
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      background = 'navy',
      
      # Population size
      sliderInput(
        "popsize",
        "Population Size:",
        min = 1,
        max = 1000,
        value = 10
      ),
      # Selection intensity
      sliderInput(
        "selection",
        "Selection Intensity (= fraction of offspring that are selected):",
        min = 0.001,
        max = 1,
        value = 0.1
      ),
      # Number of generations
      numericInput(
        "generations",
        "Generations of selection",
        value = 1000,
        min = 1,
        max = 10000,
        step = 500
      )
    )
  ),
  # TODO - ADD A DISPLAY OF THE RESULTING POPULATION SIZE
  
  # Mutation parameters
  column(
    width = 4,
    box(
      title = "Mutation parameters",
      width = "30%",
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      background = 'navy',
      
      # Mutation rate
      sliderInput(
        "mutation_rate",
        "Mutation rate:",
        min = 0.0001,
        max = 1,
        value = 0.1,
        step = 0.05
      ),

      # Mutation size distribution
      sliderInput(
        "mutation_sd",
        "Mutation size (standard deviation):",
        min = 0,
        max = 2,
        value = 0.1,
        step = 0.05
      ),
      
      # Mutation size plot
      plotOutput("mutation_sizes", height = "100px")
      
    )
  )
),

# Output displays
fluidRow(
  column(
    width = 4,
    box(
      title = "Target Image",
      status = "primary",
      width = "100%",
      solidHeader = TRUE,
      plotOutput("target_image")
    )
  ),
  column(
    width = 4,
    box(
      title = "Current Pop Average",
      status = "success",
      width = "100%",
      solidHeader = TRUE,
      plotOutput("current_pop")
    )
  ),
  column(
    width = 4,
    box(
      title = "Fitness",
      status = "warning",
      width = "100%",
      solidHeader = TRUE,
      plotOutput("fitness_progress")
    )
  )
))


##################
# Make unified user interface
##################
ui <- dashboardPage(header, sidebar, body,
                    skin = "purple")


##################
# Server functions
##################
server <- function(input, output, session) {
  # Load Image
  image_loaded = reactive({
    # If no file uploaded, return NuLL
    if (is.null(input$infile)) {
      return(NULL)
    }
    load.image(input$infile$datapath) # Internal path to the loaded data
  })
  
  # Resize image
  image_resized = reactive({
    if (is.null(image_loaded())) {
      # Pass NULL if image not loaded yet
      return(NULL)
    }
    resize_image(image_loaded(), max_pixels = input$max_pixels)
  })
  
  # Grayscale image
  image_target = reactive({
    if (is.null(image_resized())) {
      # Pass NULL if image not loaded yet
      return(NULL)
    }
    if (input$grayscale) {
      grayscale(image_resized())
    } else{
      image_resized()
    }
  })
  
  # Flip evolving population between grayscale and color
  observeEvent(input$grayscale, {
    #print("Flipping pop to grayscale")
    for (i in 1:length(current_pop$pop)) {
      if (input$grayscale) {
        current_pop$pop[[i]] = grayscale(current_pop$pop[[i]])
      } else{
        current_pop$pop[[i]] = add.color(current_pop$pop[[i]])
      }
    }
    #print(sapply(current_pop$pop, spectrum))
  })
  
  # Display current target image
  output$target_image <- renderPlot({
    if (length(image_target()) > 0) {
      par(mar = c(0, 0, 0, 0))
      plot(image_target(),
           axes = FALSE,
           interpolate = FALSE)
    }
  })
  
  # Current population average
  output$current_pop <- renderPlot({
    if (!is.null(current_pop$avg_image)) {
      par(mar = c(0, 0, 0, 0))
      plot(current_pop$avg_image,
           axes = FALSE,
           interpolate = FALSE)
    }
  })
  
  # Fitness plot
  output$fitness_progress <- renderPlot({
    if (!is.null(current_pop$avg_fitness)) {
      plot(current_pop$avg_fitness)
    }
  })
  
  
  # Plot spectrum of mutation sizes
  output$mutation_sizes <- renderPlot({
    mutation_dist = get_mutation_sizes(mutation_mean = 0,
                                       mutation_sd = input$mutation_sd)
    x = mutation_dist$size
    y = mutation_dist$density
    par(mar = c(3, 0, 0, 0))
    plot(x, y, type = 'l')
    polygon(c(min(x), x, max(x)),
            c(0, y, 0),
            col = 'royalblue',
            border = FALSE)
  })
  
  
  # TODO: Make it so image and fitness update every N generations
  # TODO: Let user adjust parameters only when evolution stopped?
  
  # Reactive data to store the image and fitness data to plot
  current_pop = reactiveValues(avg_image = NULL,
                               avg_fitness = NULL,
                               pop = NULL)
  is_running = reactiveVal(FALSE) # Flag to set process running
  
  # Trigger for starting and stopping the evolution process
  observeEvent(input$run, {
    if (input$run %% 2 == 0) {
      is_running(FALSE)
      updateActionButton(session, inputId = "run", label = "Evolve!")
    } else{
      is_running(TRUE)
      updateActionButton(session, inputId = "run", label = "Pause")
    }
  })
  
  # Run the evolutino process 1 generation at a time; have to tie to is_running
  #   and use invalidateLater() to get output partway through. (Not quite clear why.)
  observe({
    invalidateLater(0) # Invalidates this observe() function so it gets run again, like a loop
    isolate({
      if (is_running()) {
        result = evolve_images_once(
          target = image_target(),
          current_pop = current_pop,
          popsize = input$popsize,
          selection = input$selection,
          mutation_rate = input$mutation_rate,
          mutation_sd = input$mutation_sd,
          verbose = TRUE,
          seed = 1
        )
        
        # Update population data so it triggers an updated display
        current_pop$avg_image = result$avg_image
        current_pop$avg_fitness = result$avg_fitness
        current_pop$pop = result$pop
        
        # Set the is_running flag to false when finished (otherwise would go forever)
        if (length(current_pop$avg_fitness) > input$generations) {
          is_running(FALSE)
        }
      }
    })
  })
  
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
