# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 5/18/2015

###############################################################################
## SCRIPT OVERVIEW

# GOAL: ui.R is a core Shiny project file that defines and arranges the 
#       interface features for interacting with Shiny data objects (defined
#       in server.R).
#
#       For the Multinomial Outcome Simulator (MOS) application, the ui.R file
#       also handles loading any R resources needed by ui.R and server.R, along
#       with the relevant config file (MOS_config.R) to load the base 
#       data, base formula, and variable configuration list needed for the MOS 
#       application to function correctly.

# SCRIPT OUTLINE:
# - Load Supporting Packages and Scripts
#   - ui.R is run before server.R, so it makes sense to load any resources 
#     needed for either script here.
#   - We also make sure Shiny uses Cairo here.
#
# - Load MOS Configuration Script
#   - This it is the place where the application adminstrator defines:
#     (a) what data will be used by the MOS
#     (b) the formula relating predictor attributes to the outcome attribute
#     (c) which variables will be visible in the UI, how they can be 
#         interacted with (slider or facet), and how they should be
#         understood by users
#     (d) (optional) custom visualization colors
#     (e) (optional) custom bootstrap.css file to format the application
#
# - Define Visualization Theme
#   - Common ggplot theme settings are collected into a custom theme that can
#     be layered on top of plots for a consistent look across the application.
#
# - Process the Variable Configuration List
#   - Key MOS configuration features are processed so they are ready to be used 
#     with the Shiny UI Loop and the simulation/visualization functions.
#
# - Shiny UI Loop
#   - Global Application Settings
#   - Ribbon Plot UI and Visualization ("Explore Mode")
#   - Dot Cloud Plot UI and Visualization ("Single Case Mode")

###############################################################################
## Load Supporting Packages and Scripts

library(dplyr)      # serves various formatting needs
library(shinyBS)    # expands base Shiny features (e.g., popovers)
library(Cairo)      # supports plot quality across devices
library(ggplot2)    # for specifying cos theme

# insure that Shiny makes use of Cairo
options(shiny.usecairo=T)

# load the custom functions built for MOS
source("MOS_custom_functions.R")

###############################################################################
## Load MOS Configuration Script

source("MOS_config.R")

###############################################################################
## Define Visualization Theme

# define the visualization theme to be applied to all plots for a consistent
# style
MOS_theme <<- theme_bw(16) +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          strip.text = element_text(color = "white"),
          axis.text = element_text(size = 12),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 3),
          plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
          aspect.ratio = 2 / (1 + sqrt(5)),
          strip.background = element_rect(color = "#10475B", fill = "#10475B"),
          panel.border = element_rect(color = "#10475B"),
          axis.ticks = element_line(color = "#10475B")
    )

###############################################################################
## Process the Variable Configuration List

# use base data to expand variable_configuration to have all features/values 
# needed to define user inputs
variable_configuration <<- add_input_features(variable_configuration,
                                              base_data)

# extract the fixed UI options from the variable configuration list
fixed_ui_options <<- get_fixed_ui_options(variable_configuration)

# create a simple collection of raw = pretty name pairs (to make it easy to
# associate the pairs)
raw_pretty_pairs <<- do.call(rbind, variable_configuration)
raw_pretty_pairs <<- as.data.frame(raw_pretty_pairs)$pretty_name

###############################################################################
## SHINY UI LOOP

shinyUI(navbarPage(
    # global application settings
    MOS_instance_name,
    
    # set custom bootstrap.css if desired/available
    theme = custom_css,
    
    # using MOS to explore trends per predictor ("Explore Mode")
    tabPanel("Explore Mode", fluidPage(
        # define user tools in the first column
        # width = 3 of 12 (Shiny divides the horizontal space up into 12 sections)
        column(3, 
               wellPanel(               
                   radioButtons("x_axis_choice", label = h4("Select X-Axis"), 
                                choices = fixed_ui_options$x_axis_options),
                   
                   radioButtons("facet_choice", 
                                label = h4("Compare By..."),
                                choices = c("None", 
                                            fixed_ui_options$facet_options))
               ),
               
               wellPanel( 
                   helpText(h4("Advanced Options")), 
                   
                   checkboxInput("slider_show", 
                                 label = "Show?",
                                 FALSE),
                   
                   conditionalPanel(
                       # only show if the "Advanced Options" box is ticked
                       condition = "input.slider_show == true",
                       
                       bsButton("update_explore_cf_cases",
                                "Update Plot",
                                block = TRUE),
                       br(),
                       br(),
                       
                       uiOutput("explore_input_set")
                   )
               )
        ),
        
        # define the visualization in the second column
        # width = 9 of 12
        column(9, 
               plotOutput("ribbon_plot"),
               
               wellPanel(
                   uiOutput("ribbon_text"),
                   HTML(ribbon_addendum)
               )
        )
    )),
    
    # using MOS to simulate outcomes for fixed predictor values ("Single Case 
    # Mode")
    tabPanel("Single Case Mode", fluidPage(
        # define user tools in the first column
        column(3, 
               wellPanel(
                   helpText(h4("Case Values")),
                   
                   bsButton("update_sc_cf_cases",
                            "Simulate",
                            block = TRUE),
                   br(),
                   br(),
                   
                   uiOutput("sc_input_set")
               )
        ),
        
        # define the visualization in the second column
        column(9,
               plotOutput("dot_cloud_plot"),
               
               wellPanel(
                   HTML(dot_cloud_addendum)
               )
        )
    ))
))

###############################################################################
## END OF SCRIPT
###############################################################################