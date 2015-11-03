# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 7/8/2015

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

# all the supporting packages and scripts for this project are defined in a
# single script for consistency/clarity
source("MOS_packages_and_custom_functions.R")

# insure that Shiny makes use of Cairo
options(shiny.usecairo=T)

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
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
          plot.title = element_text(vjust = 3, face = "bold"),
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

shinyUI(fluidPage(
    # adjust the page style to be less cray cray
    style = "padding:0;",
    
    # customize the HTML head elements for the app (i.e., the HTML metadata)
    withTags(
        head(
            # load custom CSS (this will look for CSS files in the "www"
            # folder of the app by default)
            link(rel = "stylesheet", type = "text/css", 
                 href = main_style)
        )
    ),
    
    # POC custom nav elements
    HTML('
        <nav class="navbar navbar-default navbar-static-top" role="navigation">
            <div class="container">
                <a class="navbar-brand" href="http://pocdata.org/" style="padding:10px;">
                    <img src="logo-01.png">
                </a>
                <ul class="nav navbar-nav pull-right">
                    <li><a href="http://pocdata.org/browse" class="btn btn-primary">Back to Browse</a></li>
                </ul>
            </div>
        </nav>
    '),
    
    # POC custom title panel
    HTML('
        <div class="jumbotron text-center">
            <h1>The Case Outcome Simulator</h1>
        </div>   
    '),
    
    # apply a global style to the tab nav
    div(
        class = "gray-tabs",
        
        tabsetPanel(
            # using MOS to simulate outcomes for fixed predictor values ("Single Case 
            # Mode")
            tabPanel("Single Case Mode",
                     # set panel class
                     class = "container",
                     
                     # define user tools in the first column
                     column(3,
                            div(
                                # button that will apply the slider/dropdown values (i.e., 
                                # will re-run the simulation with current values)
                                bsButton("update_sc_cf_cases",
                                         "Simulate",
                                         block = FALSE,
                                         disabled = TRUE,
                                         class = "btn-primary"),
                                # button that will reset the values to their defaults (i.e.,
                                # to the median values in the base data set)
                                bsButton("reset_sc_inputs",
                                         "Reset",
                                         block = FALSE,
                                         class = "btn-primary"),
                                class = "btn-group btn-group-justified"
                            ),
                            
                            bsCollapse(id = "sc_panels",
                                       multiple = TRUE,
                                       
                                       bsCollapsePanel(
                                           "Numeric Features",
                                           
                                           uiOutput("sc_slider_set")
                                       ),
                                       
                                       bsCollapsePanel(
                                           "Categorical Features",
                                           
                                           uiOutput("sc_dropdown_set")
                                       )
                                       
                            )
                     ),
                     
                     # define the visualization in the second column
                     column(9,
                            plotOutput("dot_cloud_plot"),
                            
                            br(),
                            HTML(dot_cloud_addendum)
                     )
            ),
            
            # using MOS to explore trends per predictor ("Explore Mode")
            tabPanel("Explore Mode",
                     # set panel class
                     class = "container",
                     
                     # define user tools in the first column
                     # width = 3 of 12 (Shiny divides the horizontal space up into 12 sections)
                     column(3, 
                            bsCollapse(id = "explore_panels",
                                       multiple = TRUE,
                                       
                                       bsCollapsePanel(
                                           "Select X-Axis",
                                           
                                           radioButtons("x_axis_choice", 
                                                        label = NULL, 
                                                        choices = fixed_ui_options$x_axis_options)
                                       ),
                                       
                                       bsCollapsePanel(
                                           "Compare By...",
                                           
                                           radioButtons("facet_choice", 
                                                        label = NULL,
                                                        choices = c("No Comparison Selected", 
                                                                    fixed_ui_options$facet_options))
                                       )
                            ),
                            
                            bsCollapse(id = "adv_options",         
                                       bsCollapsePanel( 
                                           "Advanced Options",
                                           
                                           # button that will apply the slider values (i.e., 
                                           # will re-run the simulation with current values)
                                           bsButton("update_explore_cf_cases",
                                                    "Apply Slider Values",
                                                    block = TRUE,
                                                    style = "danger"),
                                           # button that will reset the values to their defaults (i.e.,
                                           # to the median values in the base data set)
                                           bsButton("reset_explore_inputs",
                                                    "Reset",
                                                    block = TRUE,
                                                    style = "warning"),
                                           br(),
                                           br(),
                                           
                                           uiOutput("explore_slider_set"),
                                           br(),
                                           
                                           actionLink("advanced_options_link",
                                                      "What do Advanced Options do?")
                                       )
                            )
                     ),
                     
                     # define the visualization in the second column
                     # width = 9 of 12
                     column(9, 
                            plotOutput("ribbon_plot"),
                            
                            uiOutput("ribbon_text"),
                            
                            HTML(ribbon_addendum),
                            
                            bsModal("advanced_options_description",
                                    "What do 'Advanced Options' do?",
                                    "advanced_options_link",
                                    HTML(paste0("In the default view, when you select ",
                                                "an x-axis variable the unselected ",
                                                "variables are set to a reasonable ",
                                                "value for you. Basically, we ",
                                                "say: 'If we didn't know these values ",
                                                "for a particular case, what would be ",
                                                "the most appropriate guess to make?' ",
                                                "We use the median values observed ",
                                                "in our source data to set the ",
                                                "values for the unselected ",
                                                "variables.<br><br>",
                                                "Advanced Options allows you to set ",
                                                "the unselected x-axis variables to ",
                                                "values of your choosing.<br><br>",
                                                "This can be used to explore how the ",
                                                "observed relationship between the ",
                                                "selected x-axis variable and the ",
                                                "simulated case outcomes changes ",
                                                "dependent on the context of the other ",
                                                "variables.<br><br>",
                                                "More technically, this allows you to ",
                                                "explore possible interactions among ",
                                                "the predictor variables. For the ",
                                                "interested reader, a simple, solid ",
                                                "tutorial on interactions is available ",
                                                "<a href=\"https://cdn1.sph.harvard.edu/wp-content/uploads/sites/603/2013/03/InteractionTutorial.pdf\">",
                                                "here</a>."
                                    )),
                                    size = "large")
                     )
            ),
            
            # an "About" section where additional information can be provided about
            # the application instance
            tabPanel(about_title,
                     # set panel class
                     class = "container",
                     column(8, offset = 2,
                            
                            HTML(about_body)
                     )
            )
        )
    ),
    
    # define the app footer
    withTags(
            footer(
                class = "page-footer",
                HTML("
        <div class='row'>
            <div class='description text-center'>
                <p>
                Partners for Our Children's Child Well-Being Data Portal promotes access to and 
                understanding of child welfare in Washington.
                </p>
            </div>
            <div class='text-center'>
                <p>PO Box 359476</p>
                <p>Seattle, WA 98195-9476</p>
                <p>206 221-3100</p>
            </div>
        </div>
        <div class='footer-copyright text-right'>
            <p>&copy;2015 Partners for Our Children</p>
        </div>
            "))
    )
))

###############################################################################
## END OF SCRIPT
###############################################################################