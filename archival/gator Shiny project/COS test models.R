# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/25/2015
# Date updated: 3/27/2015

# NOTE: The models built here were largely developed in the 
#       "gator model example V3.R" script. That script was archived to allow us 
#       to seperate the models from the functions working with the models.

###############################################################################
## SCRIPT OVERVIEW

# goal: Create a set of models for developing the Case Outcome Simulator (COS)
#       application.
#
#       The gator dataset comes from the simcf package and the example models
#       are extensions from the model described in the help documentation for
#       the mlogitsimev function from that package. 
#
#       Where to find the original example code:
#       library(simcf)
#       help(mlogitsimev)
#
#       This script is called by the COS R Shiny code. One of the models is
#       selected and visualized using the functions in the 
#       "COS sim and vis functions.R" script.

# sketch of script
# - LOAD SUPPORTING LIBRARIES
#
# - LOAD THE BASE EXAMPLE DATA
#
# - EXPAND AND FORMAT THE DATA TO MEET DEVELOPMENT NEEDS
#   - we want more predictors than provided in the original example
#   - we want a more complex factor than provided in the original example
#   - we also want to work with a properly formatted data frame
#

###############################################################################
## LOAD SUPPORTING LIBRARIES
#   - to get simcf (a custom package on github), use the install_github 
#     function from the devtools package:
#     install_github("chrisadolph/tile-simcf", subdir = "simcf")

library(simcf)      # provides the example data
library(nnet)       # processes the multinomial logit

###############################################################################
## LOAD THE BASE EXAMPLE DATA

###############################################################################
## LOAD SUPPORTING LIBRARIES

data(gator)

###############################################################################
## EXPAND AND FORMAT THE DATA TO MEET DEVELOPMENT NEEDS

# rename the variables to be more example appropriate
outcome <- food
age <- size
sex <- female
rm(food, size, female)

# adjust the sex predictor to have a third level
sex[sample(1:length(sex), 6, replace = F)] <- 2

# create new continuous predictors
# first a fake income relationship
income <- rnorm(outcome, mean = outcome, sd = 1)
income <- abs(income) * 20000
# second create a fake IQ relationship
iq <- rnorm(age, mean = age, sd = 10)
iq <- abs(iq) * 5

# gather into a properly formatted dataframe
base_data <- data.frame(outcome, sex, age, income, iq)
base_data$outcome <- factor(base_data$outcome, 
                            levels = c(1, 2, 3), 
                            labels = c("reunification", 
                                       "adoption", 
                                       "guardianship")
                            )
base_data$sex <- factor(base_data$sex, 
                        levels = c(0, 1, 2), 
                        labels = c("male", "female", "asexual"))

# quick clean-up of the no longer needed data objects
rm(age, income, iq, outcome, sex)

# multinom: "fit a multinomial log-linear model via neural networks"
#add_logit <- multinom(outcome ~ sex + age + income + iq, 
#                        data = base_data, Hess=TRUE)

# alternate test model with an interaction term
#int_logit <- multinom(outcome ~ sex + age + income + iq + age * iq, 
#                      data = base_data, Hess=TRUE)

###############################################################################
## END OF SCRIPT
###############################################################################