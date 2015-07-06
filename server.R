# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 7/6/2015

###############################################################################
## SCRIPT OVERVIEW

# GOAL: server.R is a core Shiny project file that generates and manages data
#       objects and user interactions with these objects. The user-facing
#       displays of these data objects and the points of interaction are
#       arranged in ui.R.
#
#       For the Multinomial Outcome Simulator (MOS) application, the server.R
#       file handles the processing of a given model formula and data to
#       generate outcome-likelihood visualizations, along with the variety
#       of intermediate products needed to get those visualizations.
#
#       NOTE: Because ui.R is run before server.R, this application has been 
#       designed so that all resources needed for server.R are loaded in ui.R.
#
# SCRIPT OUTLINE:
# - Server Initialization (Steps That Don't Need To Be Reactive)
#   - Anything placed in this section will be processed only the when the server
#     is initialized. It is the place to define the point MOS users should
#     start from when they connect to the application.
#   - We need to "spread" the factors and interactions in a second data frame  
#     so they are handled properly; this also results in an expanded model
#     formula matching the "spread" data frame.
#   - The first data frame is retained for factor/level information, the second
#     is the data frame we will use for the model fitting and simulation.
#   - We identify interaction columns in the expanded data.
#   - We re-sample the model coefficients many times to get a set of estimates
#     of these coefficients. This is the set that we will pass our
#     counterfactual cases through.
#
# - shinyServer Loop (Steps That Do Need To Be Reactive)
#   - This is where the bulk of the action takes place - it handles all of the
#     simulation steps that need to be responsive to user input, all of the
#     visualization steps, and also contributes to the creation of dynamic
#     parts of the UI. It proceeds roughly as follows:
#   - Collect user inputs.
#   - Update the dynamic parts of the UI.
#   - Create the base counterfactual case set.
#   - Update the counterfactual case set based on user inputs.
#   - Pass the counterfactual cases through the sampled coefficients to get
#     outcome likelihoods for each case.
#   - Generate the visualizations of the outcome likelihoods for the
#     counterfactual cases.

###############################################################################
## Server Initialization (Steps That Don't Need To Be Reactive)

# TESTING SETTINGS TO INCREASE STABILITY ## TEMP ##
# options(warn = -1)

# snag the outcome variable from the formula (simplifies later calls)
outcome_variable <<- as.character(base_formula[[2]])

# flip the formula to a character vector so we can use this vector to subset the
# dataframe so that any unused variables are dropped
char_bf <- paste0(as.character(base_formula[2:3]), collapse = "~")
# split the character string into individual terms (may include interactions)
char_bf <- strsplit(char_bf, "\\+|\\~")
# drop any white space around the terms and force the result to be a vector
# of terms
char_bf <- as.vector(sapply(char_bf, trimws))
# drop any interaction terms
char_bf <- char_bf[!grepl("\\*|\\:", char_bf)]
# subset the dataframe
base_data <- base_data[char_bf]

# expand the factors in the data object, re-add the outcome, drop the intercept
exp_data <<- model.matrix(base_formula, base_data)
exp_data <<- data.frame(base_data[outcome_variable],  
                        exp_data)
exp_data[, "X.Intercept."] <<- NULL

# fit the model to the expanded dataset (using the expanded model)
# multinom: "fit a multinomial log-linear model via neural networks"
exp_model <<- multinom(formula(exp_data), data = exp_data, Hess = T)

# extract the interaction column names from the expanded data
interaction_cols <<- get_interaction_col_names(base_formula, exp_data)

# get the static simulation/visualization features
point_estimates <<- get_point_estimates(exp_model)
cov_matrix <<- get_covariance_matrix(exp_model)
coeff_estimates <<- get_coefficient_estimates(1000, point_estimates, 
                                              cov_matrix, exp_model)

###############################################################################
## shinyServer Loop (Steps That Need To Be Reactive)

shinyServer(function(input, output, session) {
    ## Collect user inputs.
    # all are reactive objects so that updates to these can be relied on to
    # update any dependencies
    x_axis_raw_name <- reactive({
        # explore raw_pretty_pairs to find the correct raw name
        index <- match(input$x_axis_choice, raw_pretty_pairs)
        raw_name <- names(raw_pretty_pairs)[index]
        return(raw_name)
    })
    
    facet_raw_name <- reactive({
        # filter the variable_configuration dataframe for the row with the
        # matching UI name, extract the column name
        # UNLESS special variable "None" is selected, in which case return NULL
        if(input$facet_choice == "No Comparison Selected") {
            return(NULL)
        } else {
            # explore raw_pretty_pairs to find the correct raw name
            index <- match(input$facet_choice, raw_pretty_pairs)
            raw_name <- names(raw_pretty_pairs)[index]
            return(raw_name)
        }
    })
    
    ## Update the dynamic parts of the UI.
    # generate the input set (sliders only) for the "Explore Mode"
    output$explore_slider_set <- renderUI({
        # establish a reactive link to the "Reset" button - if this button is
        # triggered the inputs will be recreated (resetting them to their 
        # default values)
        input$reset_explore_inputs
        
        # make the inputs
        make_inputs(variable_config_list = variable_configuration,
                    variables_to_drop = x_axis_raw_name(),
                    append_name = "explore",
                    return_sliders = TRUE,
                    return_facets = FALSE)
    })
    
    # generate the input sets (sliders + dropdowns) for the "Single Case Mode"
    output$sc_slider_set <- renderUI({
        # establish a reactive link to the "Reset" button - if this button is
        # triggered the inputs will be recreated (resetting them to their 
        # default values)
        input$reset_sc_inputs
        
        # make the inputs
        make_inputs(variable_config_list = variable_configuration,
                    variables_to_drop = NA,
                    append_name = "sc",
                    return_sliders = TRUE,
                    return_facets = FALSE)
    })
    
    output$sc_dropdown_set <- renderUI({
        # establish a reactive link to the "Reset" button - if this button is
        # triggered the inputs will be recreated (resetting them to their 
        # default values)
        input$reset_sc_inputs
        
        # make the inputs
        make_inputs(variable_config_list = variable_configuration,
                    variables_to_drop = NA,
                    append_name = "sc",
                    return_sliders = FALSE,
                    return_facets = TRUE)
    })
    
    # enable/disable update/simulate button for for "Explore Mode"
    observeEvent(
        return_inputs(variable_config_list = variable_configuration,
                      input_call = input,               
                      append_name = "explore",
                      base_data = base_data,
                      use_slider_values = TRUE,
                      use_dropdown_values = FALSE,
                      variables_to_drop = isolate(x_axis_raw_name())),
        
        ({updateButton(session, "update_explore_cf_cases",
                       label = "UPDATE PLOT",
                       disabled = FALSE,
                       style = "danger")
        })
    )
    
    observeEvent(
        input$update_explore_cf_cases,
        
        ({updateButton(session, "update_explore_cf_cases",
                       label = "PLOT UPDATED",
                       disabled = TRUE,
                       style = "success")
          
        })
    )
    
    # enable/disable update/simulate button for "Single Case Mode"
    observeEvent(
        return_inputs(variable_config_list = variable_configuration,
                      input_call = input,               
                      append_name = "sc",
                      base_data = base_data,
                      use_slider_values = TRUE,
                      use_dropdown_values = TRUE),
        
        ({if(isolate(input$update_sc_cf_cases) == 0) {
            updateButton(session, "update_sc_cf_cases",
                         label = "RUN FIRST SIMULATION",
                         style = "danger")
        } else {
            updateButton(session, "update_sc_cf_cases",
                         label = "RE-SIMULATE",
                         disabled = FALSE,
                         style = "danger")
        }
        })
    )
    
    observeEvent(
        input$update_sc_cf_cases,
        
        ({updateButton(session, "update_sc_cf_cases",
                       label = "SIMULATION UPDATED",
                       disabled = TRUE,
                       style = "success")
          
        })
    )
    
    # on reset, force the Single Case mode input panels open so that all
    # values reset correctly
    observeEvent(
        input$reset_sc_inputs,
        
        ({
            updateCollapse(session, "sc_panels",
                           open = c("Describe Numeric Features",
                                    "Describe Categorical Features")
            )
        })
    )
    
    # construct the summary text for the ribbon plot ("Explore Mode")
    output$ribbon_text <- renderText({
        build_ribbon_summary(x_axis_raw_name(), 
                             facet_raw_name(),
                             variable_configuration,
                             # if "Advanced Options" are open, we want to drop
                             # the plot summary
                             include_plot_summary = is.null(input$adv_options))
        
    })
        
    ## Create the base counterfactual case set.
    base_cf_cases <- reactive({
        get_cf_cases(exp_data,
                     base_data,
                     exp_model, 
                     x_axis_raw_name(), 
                     facet_selected = facet_raw_name(),
                     interaction_col_names = interaction_cols)
    })
    
    ## Update the counterfactual case set based on user inputs.
    # update for the "Explore Mode" visualizations
    explore_cf_cases <- reactive({
        # establish the reactive link to the "Update" button
        input$update_explore_cf_cases
        
        # this next section only gets evaluated if the "Show sliders?" option is
        # set to TRUE - in that case, the slider values will be used to 
        # create an updated data object each time the "UPDATE PLOT" button is
        # clicked
        # NOTE: the "Update Plot" button will only be visible if the "Show
        #       sliders?" option is set to true, so we are restricting our
        #       update pathways to just TWO possibilities:
        #       1. the "Update Plot" button is visible and is pressed, updating 
        #          the data with slider values
        #       2. a new x-axis is selected (refreshes the plots that use
        #          explore_cf_cases() and resets the sliders if they are 
        #          visible)
        # because "input$adv_options" is not isolated, we also get the desired
        # behavior that the data is restored to its default state if the
        # "Advanced Options" panel is collapsed
        if(!is.null(input$adv_options) & input$update_explore_cf_cases > 0) {
            # note that the update_target here is allowed to be reactive
            # to create a reactive link when the sliders are visible
            apply_input_values(update_target = base_cf_cases(), 
                               interaction_col_names = interaction_cols,
                               variable_config_list = variable_configuration,
                               input_call = isolate(input),               
                               append_name = "explore",
                               base_data = base_data,
                               use_slider_values = TRUE,
                               use_dropdown_values = FALSE,
                               variables_to_drop = isolate(x_axis_raw_name()))
        } else {
            # reactive link for when the sliders are hidden
            return(base_cf_cases())
        }
    })
    
    # update for the "Single Case" visualizations
    sc_cf_cases <- reactive({        
        # establish the reactive link to the "Update" button
        input$update_sc_cf_cases
        
        # make the sliders
        # NOTE: there is only a single reactive pathway here - the "update_sc_
        #       cf_cases" input must be triggered - this insures that the
        #       visualization chain is only triggered on user request        
        apply_input_values(update_target = isolate(base_cf_cases()), 
                           interaction_col_names = interaction_cols,
                           variable_config_list = variable_configuration,
                           input_call = isolate(input),               
                           append_name = "sc",
                           base_data = base_data,
                           use_slider_values = TRUE,
                           use_dropdown_values = TRUE,
                           variables_to_drop = NA)
    })
    
    ## Pass the counterfactual cases through the sampled coefficients to get
    ## outcome likelihoods for each case.
    # get the "Explore Mode" likelihoods (note that these are actually 
    # summaries of the many likelihood sets generated for each case)
    explore_likelihoods <- reactive({
        # get the unformatted summary likelihoods
        likelihoods_raw <- MOS_mlogitsimev(explore_cf_cases(), 
                                           coeff_estimates, 
                                           ci = c(0.95, 0.50))
        
        # format the summary likelihoods for visualization
        ribbon_ready <- format_for_ribbon_plot(likelihoods_raw,
                                               exp_model,
                                               base_data,
                                               isolate(explore_cf_cases()),
                                               isolate(x_axis_raw_name()),
                                               facet_selected = 
                                                   isolate(facet_raw_name())
        )
        
        # return the formatted object
        return(ribbon_ready)
    })
    
    # get the "Single Case Mode" likelihoods (note that here we retain all the
    # likelihood sets for just the first case; this works because all of the
    # cases are actually identical in single case - we don't want summarized
    # values per case, we want the raw likelihoods associated with each
    # coefficient set for our specific example case)
    sc_likelihoods <- reactive({
        # get the estimates for each coefficient set for just the first case
        likelihoods_cloud <- MOS_mlogitsimev(sc_cf_cases(), 
                                             coeff_estimates, 
                                             ci = c(0.95, 0.50),
                                             return_first_case_likelihoods = 
                                                 TRUE)
        
        # format the single point estimates for visualization
        likelihoods_cloud <- data.frame(likelihoods_cloud)
        names(likelihoods_cloud) <- exp_model$lab
        likelihoods_cloud$index <- 1:nrow(likelihoods_cloud)
        dotplot_ready <- gather(likelihoods_cloud, outcome, single_pe, 
                                -index)
        
        # return the collection
        return(dotplot_ready)
    })
    
    ## Generate the visualizations of the outcome likelihoods for the
    ## counterfactual cases.
    # "Explore Mode" ribbon plot
    output$ribbon_plot <- renderPlot({
        # isolate the x_axis_variable name and its associated transform_for_ui
        x_axis_var <- isolate(x_axis_raw_name())
        ui_transform <- variable_configuration[[x_axis_var]]$transform_for_ui
        
        # apply the transform to the predictor column in explore_likelihoods to 
        # make it ui friendly
        ribbon_likelihoods <- explore_likelihoods()
        ribbon_likelihoods$predictor <- 
            ui_transform(ribbon_likelihoods$predictor)
        
        # draw the plot
        rp <- get_ribbon_plot(ribbon_likelihoods, 
                        facet_selected = isolate(facet_raw_name()),
                        y_lab = "Simulated Probability", 
                        x_lab = "",
                        plot_title = variable_configuration[[x_axis_var]]$
                            pretty_name,
                        custom_colors = custom_outcome_colors,
                        custom_breaks = isolate(
                            variable_configuration[[x_axis_var]]$
                                custom_x_breaks),
                        custom_labels = isolate(
                            variable_configuration[[x_axis_var]]$
                                custom_x_labels
                        )
        )
        
        # return the plot
        return(rp)
    })
    
    # "Single Case Mode" dot cloud plot
    output$dot_cloud_plot <- renderPlot({
        # draw the plot
        get_dot_cloud_plot(sc_likelihoods(),
                           y_lab = "Simulated Outcome Probability",
                           x_lab = "",
                           custom_colors = custom_outcome_colors)
        
    })
})



###############################################################################
## END OF SCRIPT
###############################################################################