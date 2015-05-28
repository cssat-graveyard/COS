# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 3/27/2015

###############################################################################
## SCRIPT OVERVIEW

# goal: This is a core Shiny project file. It defines the creation of the
#       data objects that the user interface will display and allow interaction
#       with.
#
#       Supporting libraries, data imports, etc., all happen here. For the 
#       SimTool prototype, the majority of the work is handled by custom 
#       functions (loaded, along with supporting libraries, by R scripts 
#       named below).

# sketch of script
# - source the model script and sim/vis functions script
#
# - build any static objects needed for simulation/visualization
#   - select the source data
#   - define the model formula
#   - we need to "spread" the factors and interactions in a second data frame  
#     so they are handled properly; this also results in an expanded model
#     formula matchign the "spread" data frame
#   - the first data frame is retained for factor/level information, the second
#     is the data frame we will use for the model fitting and simulation
#   - create all simulation objects except the new data and the visualization
#
# - define the shinyServer loop to turn the data objects into Shiny's output
#   objects (and define any desired interactivty)
#   - collect user inputs
#   - generate the new data based on user input
#   - feed the new data to the simulated coefficients
#   - generate the visualization based on user input

###############################################################################
## SOURCE THE MODEL AND SIM/VIS SCRIPTS

source("COS test models.R")
source("COS sim and vis functions.R")
source("COS custom mlogitsimev.R")

###############################################################################
## STATIC OBJECTS FOR SIMULATION/VISUALIZATION

# choose the data object we will be working with and specify the formula
# (with respect to this data object)
base_data <<- base_data
model_formula <<- outcome ~ sex + age + income + iq

# expand the factors in the data object, re-add the outcome, drop the intercept
exp_data <<- model.matrix(model_formula, base_data)
outcome_variable <<- as.character(model_formula[[2]])
exp_data <<- data.frame(base_data[outcome_variable],  
                        exp_data)
exp_data[, "X.Intercept."] <<- NULL

# fit the model to the expanded dataset (using the expanded model)
# multinom: "fit a multinomial log-linear model via neural networks"
exp_model <<- multinom(formula(exp_data), data = exp_data, Hess = T)

# get the static simulation/visualization features
point_estimates <<- get_point_estimates(exp_model)
cov_matrix <<- get_covariance_matrix(exp_model)
coeff_estimates <<- get_coefficient_estimates(1000, point_estimates, 
                                              cov_matrix, exp_model)

###############################################################################
## DEFINE THE OBJECTS FOR THE UI TO DISPLAY

shinyServer(function(input, output, session) {
    # create fixed user inputs
    # all are reactive objects so that updates to these can be relied on to
    # update any dependencies
    x_axis_selected <- reactive({
        x_axis_conversions[[input$x_axis_choice]]
    })
    
    facet_selected <- reactive({
        facet_conversions[[input$facet_choice]]
    })

    ci_selected <- reactive({
        input$ci_choice/100
    })
    
    x_label <- reactive({
        input$x_axis_choice
    })
    
    # generate representative data to feed coefficients
    new_data <- reactive({
        get_new_data(exp_data,
                     base_data,
                     exp_model, 
                     x_axis_selected(), 
                     facet_variable = facet_selected())
    })
    
    # generate dynamic UI features (non-x-axis predictor adjustments) and
    # set their default values
    slider_set <- reactive({
        get_sliders(base_data, 
                    outcome_variable, 
                    x_axis_selected())
    })
    
    output$fixed_predictors <- renderUI({
        lapply(1:nrow(slider_set()), function(i) {
            sliderInput(inputId = slider_set()$var_name[[i]], 
                        label = toupper(slider_set()$var_name[[i]]),
                        min = slider_set()$var_min[i], 
                        max = slider_set()$var_max[i],
                        value = slider_set()$var_mean[i])
        })
    })
    
    # update the new data with inputs from the sliders
    new_data_fixed <- reactive({
        # conditions under which new_data_fixed will get regenerated
        # (important since we isolate the source new_data object itself)
        # NOTE: this also updates any time one of the relevant sliders is
        #       adjusted since those are reactive objects referenced below
        #       without any isolation (i.e., input[[current_var]])
        x_axis_selected()
        facet_selected()
        
        # isolate the current new_data object
        new_data_initial <- isolate(new_data())
        
        # update the values based on current slider inputs
        for(i in 1:nrow(slider_set())) {
            current_var <- as.character(slider_set()$var_name[i])
            # all the input variables initialize as "NULL" - we want to avoid
            # working with them until they've been assigned a value
            if(!is.null(input[[current_var]])) {
                new_data_initial[current_var] <- input[[current_var]]
            }
        }
        
        return(new_data_initial)
    })
    
    # feed the representative data to the sampled coefficients to generate
    # our final simulated outcome likelihoods
    prediction_object <- reactive({
        mlogitsimev_med(new_data_fixed(), 
                        coeff_estimates, 
                        ci = ci_selected())
    })
    
    # visualize the outcome likelihoods
    output$demo_plot <- renderPlot({
        visualize_predictions(prediction_object(), 
                              exp_model, 
                              base_data,
                              new_data_fixed(), 
                              x_axis_selected(), 
                              facet_variable = facet_selected(),
                              y_lab = "Probability", 
                              x_lab = x_label())
    })
})



###############################################################################
## END OF SCRIPT
###############################################################################