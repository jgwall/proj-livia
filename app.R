library(shiny)
library(shinydashboard)
source("breedart.r")

# TODO: Allow user to load a new image (and reset the evolution) throughout.
#       Bonus: Allow different image to be used seamlessly if dimensions are the same
# TODO: Allow user to set random seed?

################
# Header layout
################
header = dashboardHeader(title = "Livia - Exploring Evolution with Images",
                         titleWidth = "40%")

################
# Input Components
################

# Input file selector
file_load_input = fileInput("infile", "Target image (file)")

# Maximum pixels allowed
max_pixels_input = numericInput(
  "max_pixels",
  "Maximum pixels allowed",
  value = 1000,
  min = 1,
  max = 10000,
  step = 500
)

# Grayscale image?
grayscale_input = checkboxInput("grayscale", "Grayscale",
                                value = FALSE, width = '100%')
# Run button
run_button = actionButton("run", 
                          "Evolve!",
                          icon=icon("play"),
                          width="90%",
                          align="center",
                          style="font-size: x-large"
                          )

# Population size
population_size_input = sliderInput(
  "popsize",
  "Population Size:",
  min = 1,
  max = 1000,
  value = 10
)

# Selection intensity
selection_intensity_input = sliderInput(
  "selection",
  "Selection Intensity (= fraction of offspring that are selected):",
  min = 0.001,
  max = 1,
  value = 0.1
)

# Number of generations
num_generations_input = numericInput(
  "generations",
  "Generations of selection",
  value = 1000,
  min = 1,
  max = 10000,
  step = 500
)

# Mutation rate
mutation_rate_input = sliderInput(
  "mutation_rate",
  "Mutation rate:",
  min = 0,
  max = 1,
  value = 0.1,
  step = 0.01
)

# Mutation size distribution
mutation_size_input = sliderInput(
  "mutation_sd",
  "Mutation size (standard deviation):",
  min = 0,
  max = 2,
  value = 0.1,
  step = 0.05
)

# Warning to load an image before trying to evolve it
warning_box = span(
  h3(textOutput("warn_box")), # Reactive so it goes away when no longer an issue
  style="text-align:center; color:crimson"
)

# ##############
# Output Components (usually wrapped into boxes)
# ##############

# Mutation size plot (not in a box)
mutation_info = valueBox(
  value = plotOutput("mutation_sizes", height="85px"),
  subtitle="Mutation size distribution",
  icon=icon("bolt"),
  color='maroon',
  width=12
)

# Number of resulting offspring each generation
offspring_info = valueBoxOutput("num_offspring", width=12)

# Current population fitness
current_fitness_info = valueBoxOutput("current_fitness", width=12)

# Display of target image
target_image_box = box(
  title = "Target Image",
  status = "primary",
  width = "100%",
  solidHeader = TRUE,
  plotOutput("target_image")
)

# Current population average
current_pop_box = box(
  title = "Current Pop Average",
  status = "info",
  width = "100%",
  solidHeader = TRUE,
  plotOutput("current_pop")
)

fitness_progress_box = box(
  title = "Fitness",
  status = "warning",
  width = "100%",
  solidHeader = TRUE,
  plotOutput("fitness_progress")
)

info_box = box(
  title = "Derived parameters",
  width = "100%",
  solidHeader = TRUE,
  status = 'warning',
  
  # Components
  mutation_info, 
  offspring_info,
  current_fitness_info
  
)

###################
# Gather input components into boxes
###################
file_input_box = box(
  title = "Image options",
  width = 14,
  status = "primary",
  solidHeader = TRUE,
  collapsible = TRUE,
  collapsed=FALSE,
  background = 'navy',
  
  # Panel components
  file_load_input,
  max_pixels_input,
  grayscale_input
)

population_box = box(
  title = "Population parameters",
  width = 14,
  status = "primary",
  solidHeader = TRUE,
  collapsible = TRUE,
  collapsed = TRUE,
  background = 'navy',
  
  # Panel components
  population_size_input,
  selection_intensity_input,
  num_generations_input
)

mutation_box = box(
  title = "Mutation parameters",
  width = 14,
  status = "primary",
  solidHeader = TRUE,
  collapsible = TRUE,
  collapsed = TRUE,
  background = 'navy',
  
  # Mutation rate
  mutation_rate_input,
  mutation_size_input
)


################
# Organize Sidebar
################
sidebar = dashboardSidebar(
  width = "30%",
  
  # Sidebar components
  file_input_box,
  population_box,
  mutation_box,
  run_button,
  warning_box
)

###################
# Organize main panel
###################
body =  dashboardBody(
  
  fluidRow(
    column(width = 6,
         target_image_box),
    column(width = 6,
           current_pop_box)
  ),

  # Output displays
  fluidRow(
    column(width = 6,
           info_box),
    column(width = 6,
           fitness_progress_box)
  )
)


##################
# Combine final user interface
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
    # is_running(FALSE) # Stop the evolution if a new image is loaded
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
    if (!is.null(current_pop$pop)) {
      for (i in 1:length(current_pop$pop)) {
        # Turn to grayscale if button is checked
        if (input$grayscale) {
          current_pop$pop[[i]] = grayscale(current_pop$pop[[i]])
          # If button not checked, add color channels
        } else{
          current_pop$pop[[i]] = add.color(current_pop$pop[[i]])
        }
      }
    }
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
  
  # Display current population average image
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
  
  
  # Display number of resulting offspring
  output$num_offspring = renderValueBox({
    valueBox(
      value = round(input$popsize / input$selection),
      subtitle = "Offspring per generation",
      icon=icon("users"),
      color = "purple"
    )
  })
  
  # Calculate current fitness
  current_fitness = reactive({
    print(str(current_pop))
    if (is.null(current_pop$avg_fitness)) {
      return(NA) # If nothing happened yet, return NA
    }else{
      round(current_pop$avg_fitness[length(current_pop$avg_fitness)], digits=3)
    }
  })
  
  # Display current population fitness
  output$current_fitness = renderValueBox({
    valueBox(
      value = current_fitness(),
      subtitle = "Current Fitness",
      icon=icon("frog"),
      color = "olive"
    )
  })

  # Plot spectrum of mutation sizes
  output$mutation_sizes <- renderPlot({
    mutation_dist = get_mutation_sizes(mutation_mean = 0,
                                       mutation_sd = input$mutation_sd)
    x = mutation_dist$size
    y = mutation_dist$density
    par(mar = c(1.5, 0, 0, 0), cex.axis=1, col.axis='white')
    plot(x, y, type = 'l', yaxt='n', xaxt='n', bty='n')
    polygon(c(min(x), x, max(x)),
            c(0, y, 0),
            col = 'maroon',
            border = NA
    )
    axis(side=1, at=c(-1, 0, 1), font=2, lwd=0, cex.axis=1.5, padj=-0.75)
    }, bg='transparent')
  
  # Reactive data to store the image and fitness data to plot
  current_pop = reactiveValues(avg_image = NULL,
                               avg_fitness = NULL,
                               pop = NULL)
  
  # Flag to set process running
  is_running = reactiveVal(FALSE) 
  
  # Help text prompting to load an image if try to evolve before one is up
  warn_box=reactiveVal("")
  output$warn_box = renderText({warn_box()})

  # Trigger for starting and stopping the evolution process
  observeEvent(input$run, {
    if(is.null(image_target())){  # Don't evolve if haven't loaded image
      warn_box("Please load an image first") 
    }else if (is_running()) {  # Pause if sim is running
      is_running(FALSE)
      warn_box("")
    } else{  # Run if sim is paused
      is_running(TRUE)
      warn_box("")
    }
  })
  
  # Update the Run button based on whether sim is running or not
  observe({
    if(is_running()){
      updateActionButton(session, inputId = "run", label = " Pause", icon=icon("pause"))
    }else{
      updateActionButton(session, inputId = "run", label = " Evolve!", icon=icon("play"))
    }
  })
  
  
  
  # Run the evolution process 1 generation at a time; have to tie to is_running
  #   and use invalidateLater() to get output partway through. (Not quite clear why.)
  observe({
    invalidateLater(0) # Invalidates this observe() function so it gets run again, like a loop
    isolate({
      if (is_running()) {
        
        # Check to make sure dimensions are compatible (largely if someone loads a new image)
        if(!is.null(current_pop$avg_image) && 
          height(image_target()) != height(current_pop$avg_image) &&
          height(image_target()) != height(current_pop$avg_image )){
          mywarning = paste("Cannot evolve: Target has", nPix(image_target()), 
                            "pixels but population has", nPix(current_pop$avg_image),
                            ". Please choose an image with compatible dimensions.")
          warn_box(mywarning)
          is_running(FALSE)
          return(NULL)
        }
        
        # Run the evolution
        result = evolve_images_once(
          target = image_target(),
          current_pop = current_pop,
          popsize = input$popsize,
          selection = input$selection,
          mutation_rate = input$mutation_rate,
          mutation_sd = input$mutation_sd,
          verbose = TRUE #,
          # seed = 1
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
