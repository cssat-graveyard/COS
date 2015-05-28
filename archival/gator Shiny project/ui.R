# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 3/27/2015

###############################################################################
## SCRIPT OVERVIEW

# goal: This is a core Shiny project file. It defines the user interface
#       for the Sim Tool prototype - including which data objects are visible,
#       where they are visible, and how they can be interacted with (if
#       at all.)

# sketch of script
# - Configuration Settings
#   - for defining the options in the various UI features and, if needed,
#     for defining the relationship between the options and the actual
#     data objects
# - Shiny UI Loop
#   - page settings
#   - define the user tools
#   - define the visualization

###############################################################################
## CONFIGURATION SETTINGS
# what you want the user to see on the UI selection features and visualization
x_axis_options <<- list("Age", 
                       "Income", 
                       "IQ")
facet_options <<- list("None", 
                      "Sex")

# the relationship between the UI names and the data objects
x_axis_conversions <<- list("Age" = "age", 
                        "Income" = "income", 
                        "IQ" = "iq")
facet_conversions <<- list("None" = NULL, 
                       "Sex" = "sex")

###############################################################################
## SHINY UI LOOP

shinyUI(fluidPage(
    # settings for the entire page
    titlePanel("SimTool Demo"),
    
    # define user tools in the first column
    # width = 3 of 12 (Shiny divides the horizontal space up into 12 sections)
    column(3, 
           wellPanel(
               helpText("Adjust predictors to explore how likelihoods change."),
               
               radioButtons("x_axis_choice", label = h3("Select X-Axis"), 
                            choices = x_axis_options),
               
               radioButtons("facet_choice", 
                            label = h3("Facet Choice"),
                            choices = facet_options),
               
               sliderInput("ci_choice", 
                           label = "Confidence Interval",
                           min = 0, max = 100, value = 95)
           ),
           
           wellPanel(
               helpText("Adjust fixed (non-x-axis) predictors."),
               
               uiOutput("fixed_predictors")
               )
    ),
    
    # define the visualization in the second column
    # width = 9 of 12
    column(9, 
           plotOutput("demo_plot")
    )
))

###############################################################################
## END OF SCRIPT
###############################################################################