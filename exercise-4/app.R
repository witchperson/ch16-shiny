# Exercise 4: Interactive plots

# Load the Shiny library


# Define a UI using a `fluidPage()` layout


  # Set the page titlePanel to be "Milage by Engine Power"


  # Output a plot 'milage', and respond to clicks on the plot


  # Output the word "Highlighting:" followed by a 'selected' text output


# Define a server function


  # Create a `reactiveValues()` list to store reactive data values


  # Assign a key 'selected.class' to the reactiveValue with a default value of ""


  # Render the 'milage' plot output


    # Return a ggplot scatterplot for the `mpg` dataset, with `displ` on the x 
    # and `hwy` on the y axis. 
    # Color each point by whether the `class` is %in% the `selected.class` 
    # reactive value.
    # Add `guides()` so that the `color` legend is not shown (FALSE)


  # Render the 'selected' text output, which should be the (text) value of your
  # the `selected.class` reactive value


  # Use `observeEvent()` to respond to plot clicks


    # Use `nearPoints()` to get selected rows in the `mpg` data set near to the 
    # click location


    # Store `unique()` values from the `class` feature of the selected rows in 
    # the `selected.class` reactiveValue


  # Bonus: also use `observeEvent()` to respond to brushing and change the plot
  # in a different way!
  


# Create a new `shinyApp()` using the above ui and server

