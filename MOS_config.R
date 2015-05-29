# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 5/12/2015
# Date updated: 5/29/2015

###############################################################################
## SCRIPT OVERVIEW

# GOAL: MOS_config.R is where a Multinomial Outcome Simulator (MOS) instance
#       is defined by an application administrator.
#
#       The MOS is a Shiny application that allows users to explore a given
#       multinomial logit model by interacting with simulations and 
#       visualizations based on that model.
#
#       The model formula and the data the model are fit against are provided to 
#       the application, which then builds the visualizations by:
#       (a) getting a model fit
#       (b) simulating outcomes from that model based on user inputs
#       (c) plotting the outcomes in the context of the user inputs
#
#       The MOS should (theoretically) take any multinomial logit formula
#       and its accompanying, properly formatted R data frame.
#
#       However, certain features of the MOS need to be specified for it
#       to work.
#
#       This config file is where those features are specified. It is sourced
#       by the MOS ui.R file when the application is initialized.
#
#       Non-functional example code has been provided in each configuration
#       section. A working example project with code can be observed here:
#       ##UPDATE NEEDED##
#
# SCRIPT OUTLINE:
# - Name the Application Instance
#   - This is the title that will be displayed in the instance navigation and
#     should be a very concise description.
#
# - Import and Name the Data Object as Needed
#   - The multinomial logit model needs to be fit to a dataset. This dataset
#     needs to be be an R data frame named "base_data" (no quotes).
#   - This section is where the data frame is created (however that needs to 
#     be done) and assigned to "base_data".
#
# - Specify the Multinomial Logit Formula
#   - The multinomial logit formula needs to be provided explicitly and it
#     needs to appropriately reference the "base_data" data frame. The formula
#     needs to be assigned to "base_formula".
#
# - Variable Configuration
#   - We need to specify which variables will the user will be able to interact
#     with (via slider or facet). Key information must be provided for each
#     of these variables. See the section for details.
#
# - Custom Visualization Colors (Optional)
#   - Assign the custom colors (the same number as there are outcomes) to
#     the character string "custom_outcome_colors".
#   - If you don't want to use custom colors, set "custom_outcome_colors" to 
#     NULL.
#
# - Custom bootstrap.css (Optional)
#   - The bootstrap.css file should be placed in a subdirectory of the
#     application titled "www".
#   - Assign the name of the bootstrap.css file to the character string 
#     "custom_css" (just the name, Shiny will know to look in "www").
#   - If you don't want to use a custom bootstrap.css, set "custom_css" to NULL.
#
# - Ribbon Plot Addendum (Optional)
#   - If you want to provide any additional text (e.g., caveats, general
#     context) beneath the ribbon plot text body, you can assign an HTML-
#     formatted string to "ribbon_plot_addendum".
#   - Set this variable to NULL if you don't want to add anything.
#
# - Dot Cloud Plot Addendum (Optional)
#   - Same but for the dot cloud plot (dot_cloud_addendum).
#   - Set this variable to NULL if you don't want to add anything.
#
# - More Info Modal (Optional)
#   - If you want additional information about the plots to be available to
#     the user but you don't want to clutter up the space beneath the plot
#     with a lot of text...
#   - One option is to simply put HTML anchor tags and links into the relevant
#     addendum.
#   - However, if you don't want your user to have to leave the application,
#     you can adjust the features in this section to make a modal link and
#     window available to the user. The link will appear after the plot
#     addendum (if there is one).
#   - Modal details: http://ebailey78.github.io/shinyBS/docs/Modals.html
#   - Simply set "more_info_link_text" to NULL if you don't want to use the
#     modal at all.

###############################################################################
## Name the Application Instance

MOS_instance_name <- "The Case Outcome Simulator"

###############################################################################
## Import and Name the Data Object as Needed

# source the base data and base model
load("data_model_V4.RData")

# explicitly choose the data object we will be working with
# NOTE: incomplete cases will be dropped to avoid modeling/plotting issues
base_data <- data[which(complete.cases(data)), ]

# ensure the levels in the outcome variable are in RAGE order
base_data$outcome <- factor(base_data$outcome, c("Reunification", 
                                                 "Adoption",
                                                 "Guardianship", 
                                                 "Emancipation"))

###############################################################################
## Specify the Multinomial Logit Formula

# Note that the formula needs to correctly reference the base_data object
# column names.
base_formula <-
    # outcome column
    outcome ~ 
    # additive terms
    mist_scores + wrkg_scores + recep_scores + buyn_scores + log_age_eps_begin + 
    non_min + male + log_par_age + married + hhnum_c + rel_plc + log_eps_rank + 
    housing_hs_cnt + high_in + sm_coll + employ + REG + 
    # interaction terms
    high_in : housing_hs_cnt + 
    housing_hs_cnt : employ

###############################################################################
## Variable Configuration

# The following features must be specified for every model variable that you
# want the user to be able to see and interact with.

# variable_configuration <- list(
#     RAW_NAME = list(
#         pretty_name         = UI_friendly name (REQUIRED),
#         definition          = a concise explanation of how the user should
#                               understand the variable (OPTIONAL),
#         ribbon_plot_summary = a concise summary of the trends observed in the
#                               ribbon plot when this variable is seleted as
#                               the x-axis (OPTIONAL, only useful for slider
#                               variables),
#         custom_x_axis_ticks = NULL or a character vector with as many items
#                               as there are ribbon plot x-axis ticks; the
#                               x-axis tick labels will be replaced with the
#                               character vector items in the order they are
#                               given
#         x_axis_candiate     = TRUE or FALSE, allow the variable to be
#                               selected as the x-axis on the ribbon plot
#                               (REQUIRED),
#         slider_candidate    = TRUE OR FALSE, where appropriate, make a slider
#                               for this variable (REQUIRED)
#         slider_rounding     = NA or a number, refines slider behavior (e.g., 
#                               1 will force the slider for this variable to
#                               snap to whole numbers) (REQUIRED, defaults to
#                               0.1 if NA, only impacts slider_candidates),
#         facet_candidate     = TRUE or FALSE, allow the variable to be 
#                               selected as a facet on the ribbon plot
#                               (REQUIRED, variable will be forced to factor if
#                               TRUE),
#         transform_for_ui    = defaults to "identity" (no transformation) but
#                               can can take other transformations if 
#                               variable needs to be transformed for user 
#                               presentation (REQUIRED),
#         transform_for_model = reverses the user-friendly transformation so
#                               that values are model-friendly again (REQUIRED)
#     ),
#     ...
# )

variable_configuration <- list(   
    mist_scores = list(
        pretty_name         = "Engagement: Parent Trusts Case Worker",
        definition          = paste0("Parental belief that the agency or ", 
                                     "case worker is sincere, honest, or ",
                                     "well-intentioned, with intent to help ",
                                     "the parent."),
        ribbon_plot_summary = paste0("There is a positive association between ",
                                     "this index of parental trust and ",
                                     "Reunification: the likelihood that ",
                                     "simulated cases end in Reunification ",
                                     "increases as the trust index increases.",
                                     "<br><br>The likelihood of both Adoption ",
                                     "and Guardianship declines as the trust ",
                                     "index increases. The likelihood of ",
                                     "Emancipation (very unlikely) remains ",
                                     "stable at all index levels."),
        custom_x_axis_ticks = c("very low", "low", 
                                "moderate", 
                                "high", "very high"),
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) abs(x - 3),
        transform_for_model = function(x) -(x) + 3
    ),    
    wrkg_scores = list(
        pretty_name         = paste0("Engagement: Working Relationship ",
                                     "Between Parent and Case Worker"),
        definition          = paste0("Parental perception of the ",
                                     "interpersonal relationship with case ",
                                     "worker characterized by a sense of ",
                                     "reciprocity or mutuality and good ",
                                     "communication."),
        ribbon_plot_summary = paste0("There is a positive, but weak, ",
                                     "association between this index of the ",
                                     "parent--case worker relationship and ",
                                     "Reunification: the likelihood that ",
                                     "simulated cases end in Reunification ",
                                     "slightly increases as the relationship ",
                                     "index increases.<br><br>The likelihood ",
                                     "of Adoption declines as the ",
                                     "relationship index increases. The ",
                                     "likelihood of Guardianship (fairly ",
                                     "unlikely) and Emancipation (very ",
                                     "unlikely) remain stable all index ",
                                     "levels."),
        custom_x_axis_ticks = c("very low", "low", 
                                "moderate", 
                                "high", "very high"),
        x_axis_candidate    = TRUE,    
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),   
    recep_scores = list(
        pretty_name         = "Engagement: Parent Receptivity",
        definition          = paste0("Parental openness to receiving help, ",
                                     "characterized by recognition of ", 
                                     "problems or circumstances that resulted ",
                                     "in agency intervention and by a ",
                                     "perceived need for help."),
        ribbon_plot_summary = paste0("The association between this index of ",
                                     "parent receptivity and the case ",
                                     "outcomes is very weak. In other words, ",
                                     "this index - at least by itself - is ",
                                     "has little effect on the likelihood of ",
                                     "simulated case outcomes."),
        custom_x_axis_ticks = c("very low", "low", 
                                "moderate", 
                                "high", "very high"),
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),    
    buyn_scores = list(
        pretty_name         = "Engagement: Parent Buy-In",
        definition          = paste0("Parental perception of benefit; a sense ",
                                     "of being helped or the expectation of ",
                                     "receiving help through the agency ",
                                     "involvement; a feeling that things are ",
                                     "changing (or will change) for the ",
                                     "better. Also includes a commitment to ",
                                     "the helping process, characterized by ",
                                     "active participation in planning or ",
                                     "services, goal ownership, and ",
                                     "initiative in seeking and using help."),
        ribbon_plot_summary = paste0("There is a positive association between ",
                                     "this index of parental ",
                                     "commitment/participation and ",
                                     "Reunification: the likelihood that ",
                                     "simulated cases end in Reunification ",
                                     "increases as the buy-in index increases.",
                                     "<br><br>The likelihood of Guardianship ",
                                     "decreases as the buy-in index ",
                                     "increases. The likelihood of Adoption ",
                                     "(moderately likely) and Emancipation ",
                                     "(very unlikely) remain stable."),
        custom_x_axis_ticks = c("very low", "low", 
                                "moderate", 
                                "high", "very high"),
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),    
    log_age_eps_begin = list(
        pretty_name         = "Child Age at Episode Begin",
        definition          = paste0("The age of the child (in years) as of ",
                                     "the start of their placement in ",
                                     "out-of-home care."),
        ribbon_plot_summary = paste0("There is a high likelihood that ",
                                     "simulated cases end in Reunification if ",
                                     "the case starts when the child about 2 ",
                                     "to 12 years of age.<br><br>Prior to the ",
                                     "second year, Adoption is also fairly ",
                                     "likely - but it declines steeply from 0 ",
                                     "to 5 years and then stabilizes until ",
                                     "about 10 to 12 years.<br><br>The ",
                                     "likelihood that simulated cases end in ",
                                     "Guardianship slowly increases ",
                                     "(complimenting the decline in Adoption) ",
                                     "until about 12 years.<br><br>At 10 to ",
                                     "12 years, Reunification, Adoption, and ",
                                     "Guardianship become rapidly less likely ",
                                     "as child age increases. Instead, ",
                                     "Emancipation becomes increasingly ",
                                     "likely. By 13 to 15 years of age, it is ",
                                     "the most likely outcome for simulated ",
                                     "cases."),
        custom_x_axis_ticks = NULL,
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) exp(x) - 1,
        transform_for_model = log1p
    ),  
    housing_hs_cnt = list(
        pretty_name         = "Count of Housing Hardships",
        definition          = paste0("The count of affirmative responses to ",
                                     "survey questions concerning housing ",
                                     "hardships (e.g. difficulty paying rent, ",
                                     "couch-surfing, etc.)."),
        ribbon_plot_summary = paste0("There is a strong negative assocation ",
                                     "between this index of housing hardships ",
                                     "and Reunification: the likelihood that ",
                                     "simulated cases end in Reunification ",
                                     "decreases as the housing hardship index ",
                                     "increases.<br><br>The likelihood of ",
                                     "Adoption increases as the housing ",
                                     "hardship index increases. Guardianship ",
                                     "(unlikely) and Emancipation (very ",
                                     "unlikely) remain stable at all index ",
                                     "levels."),
        custom_x_axis_ticks = NULL,
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),   
    REG = list(
        pretty_name         = "Administrative Region",
        definition          = paste0("An indicator of the administrative ",
                                     "region of the child welfare case."),
        ribbon_plot_summary = paste0(""),
        custom_x_axis_ticks = NULL,
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),
    employ = list(
        pretty_name         = "Parental Employment Status",
        definition          = paste0("An indicator as to whether or not the ",
                                     "parent reported full or part-time ",
                                     "employment."),
        ribbon_plot_summary = paste0(""),
        custom_x_axis_ticks = NULL,
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),
    sm_coll = list(
        pretty_name         = "Parental Education Level",
        definition          = paste0("An indicator as to whether or not the ",
                                     "parent reported any education beyond ",
                                     "high-school."),
        ribbon_plot_summary = paste0(""),
        custom_x_axis_ticks = NULL,
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),
    high_in = list(
        pretty_name         = "Parental Income Status",
        definition          = paste0("An indicator as to whether or not the ",
                                     "reported parental income is less than ",
                                     "(or equal to) 10,000 dollars."),
        ribbon_plot_summary = paste0(""),
        custom_x_axis_ticks = NULL,
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    )
)

###############################################################################
## Custom Visualization Colors (Optional)

# Colors are applied in the order they are given to outcomes in level order.
# If no custom colors are desired, set this to NULL.
custom_outcome_colors <- c("#D9BB32", "#6DB33F", "#6E9CAE", "#B1662B", 
                           "#5B8067", "#444D3E", "#994D3E", "#10475B", 
                           "#7D6E86", "#D47079", "#262F1D", "#B0B0B0")

###############################################################################
## Custom bootstrap.css (Optional)

# Custom bootstrap.css file must be in the www subdirectory of the MOS
# application. Set "custom_css" to NULL if you don't want to use one.
custom_css = "sandstone_bootstrap.css"
# CSS theme for entire project (current theme from here:
# https://bootswatch.com/sandstone/)

###############################################################################
## Ribbon Plot Addendum (Optional)

# This needs to be an HTML formatted string. It will immediately begin adding
# text after the auto-generated ribbon plot text (variable name, definition,
# and key trends) - you will need to add line breaks where needed. Set to NULL
# if you don't want any added text.
ribbon_addendum <-
    paste0("<br><strong>Please Keep In Mind</strong>",
           
           "<br>Our simulation does not test whether the observed ",
           "relationships between the predictor variables and the outcomes ",
           "are causal. The reader is advised to treat these as useful ",
           "associations and to be cautious about inferring cause/effect ",
           "relationships.")

###############################################################################
## Dot Cloud Plot Addendum (Optional)

# Like "ribbon_addendum", this also needs to be an HTML formatted string.
# No text is automatically created for the dot cloud plot. A default 
# explanation of the plot is provided below, but you may want to adjust
# the language to be appropriate for the application instance and audience. Set
# to NULL if you simply want the this are to be blank.
dot_cloud_addendum <- 
    paste0("<strong>What Does This Tool Do?</strong>",
           
           "<br>This tool allows you to describe a specific child welfare ",
           "case and observe how likely each outcome is for simulations based ",
           "on that case.", 

           "<br><br>You describe the case by setting the inputs to the values ",
           "that best fit the case.",

           "<br><br>Each time the 'SIMULATE' button is clicked, the child ",
           "welfare case you described is run through 1000 versions of our ",
           "case outcome model. These versions vary based on how much ",
           "uncertainty there is in the model.",
           
           "<br><br>For each model version, we get an estimate of how likely ",
           "the four outcomes are. We plot every estimate by its outcome.",
           
           "<br><br>So, there are 1000 points by each outcome - one for every ",
           "model version.",
           
           "<br><br>The resulting plot gives us a sense of how likely the ",
           "outcomes tend to be across all the model versions (where do the ",
           "dots tend to cluster for each outcome?) while also suggesting ",
           "how much confidence we should have in our model's ability to ", 
           "accurately simulate outcomes for the described case ",
           "(how spread out are the dots for each outcome?)")

###############################################################################
## More Info Modal (Optional)

# What should be the text for the user-facing link that can open the more info
# modal? Plain text string - no HTML formatting (will be formatted to look like 
# a section title in ui.R). Set to NULL if you don't want to use the more info
# modal at all.
more_info_link_text <- "What is the simulation based on?"

# Set the title for the modal window. 
more_info_title <- "What is the simulation based on?"

# Build the body for the modal window. This can technically be any Shiny R
# UI objects, but by default should simply be a block of HTML formatted text.
# You will need to reference and adjust the ui.R and server.R scripts if you 
# want more complex Shiny R features here.
more_info_body <- 
    paste0("The simulation is modeled on real data: a survey of child ",
           "welfare-involved parents performed in 2008 by Partners for Our ",
           "Children and linked to administrative data from Children's ",
           "Administration.",
           
           "<br><br>The data is of limited scope, it includes only cases ",
           "where the child was removed with an active dependency petition ",
           "and entered care in 2008 in Washington State.")


###############################################################################
## END OF SCRIPT
###############################################################################