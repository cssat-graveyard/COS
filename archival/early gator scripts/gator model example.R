# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/17/2015
# Date updated: 3/20/2015

###############################################################################
## SCRIPT OVERVIEW

# goal: Prototype fitting a multonomial logit model and simulating the data.
#       Uses the simcf package and the example described in the help docs for
#       the mlogitsimev function.
#
#       Where to find the original example code:
#       library(simcf)
#       help(mlogitsimev)
#
#       The original example was vague, limited and somewhat broken. It has
#       been extended and corrected for this prototype.
#
#       Our goal is to take this example and build a Shiny package on top of it.
#       We will explore how efficient it will be to simulate data from a 
#       multonomial logit model and and visualize the results on the fly via 
#       Shiny.

# sketch of script
# - COPY OF SCRIPT FROM HELP DOCS
#

###############################################################################
## Load libraries and data
# NOTES: 
#   - library load order and style was corrected to insure proper loading
#   - to get simcf (a custom package on github), use the install_github 
#     function from the devtools package:
#     install_github("chrisadolph/tile-simcf", subdir = "simcf")
#   - the gator dataset comes from the simcf package (the original doc fails
#     to specify this and loaded it out of order)

library(nnet)       # processes the multinomial logit
library(MASS)       # allows for multivariate sampling
library(simcf)      # creates counterfactual sets (and provides example data)
library(tidyr)      # for reformatting data for visualization
library(ggplot2)    # for visualizing the data

# loading the example data
data(gator)
# adding a new continuous predictor: number of teeth
# (seed set to insure consistency during prototyping)
set.seed(1)
teeth <- round(runif(59, 13, 30))
set.seed = NULL

# set some key variables (probably should be adjusted to become function input
# or be calculated from function input)
number_predictors <- 4
number_outcomes <- 3

# fit a multinomial log-linear model via via neural networks
mlogit.result <- multinom(food ~ size + female + teeth, Hess=TRUE)

# get the point estimates out of the result
# the multinom function returns a lot of 0s - first we find identify where the
#   non-reference weights begin (one set of weights per outcome but we 
#   skip the initial reference set)
index_starts <- NULL
for(i in 1:(number_outcomes - 1)) {
    # go the end of the current chunk... add two to skip the placeholder 0...
    index_starts[i] <- i * (number_predictors + 1) + 2
}
# then we use the start indices and the the number of parameters to make the
#   index to collect all of the non-reference weights
wts_index <- NULL
for(i in index_starts) {
    wts_index <- c(wts_index, i:(i + number_outcomes))
}
# finally we get the desired weights
pe <- mlogit.result$wts[wts_index]

# point estimates
vc <- solve(mlogit.result$Hess)       # var-cov matrix

## Coefficient Estimates
# Simulate parameters from predictive distributions
sims <- 1000
# draw parameters, using MASS::mvrnorm
simbetas <- mvrnorm(sims, pe, vc)
# re-arrange simulates to array format for MNL simulation
simb <- array(NA, dim = c(sims, number_predictors, number_outcomes - 1))  
index_starts <- seq(from = 1, to = number_predictors * (number_outcomes - 1), by = number_predictors)
for(i in 1:(number_outcomes - 1)) {
    simb[, , i] <- simbetas[, index_starts[i]:(index_starts[i] + number_predictors - 1)]
}

## New Data
# Create full factorial set of counterfactuals
# BRIAN UPDATE: original doc had a typo (cffactorial rather than the correct 
#   cfFactorial)
sizerange <- seq(1, 4, by = 0.1)          # range of counterfactual sizes
femalerange <- c(0, 1)                 # range of counterfactual sexes
teethrange <- seq(min(teeth), max(teeth), by = 0.1) # range of teeth counts
xhyp <- cfFactorial(size = sizerange, female = femalerange, teeth = teethrange)

# does this work?
xx = expand.grid(sizerange, femalerange, teethrange)
names(xx) = c("size", "female", "teeth")

xhyp2 = list(x = xx, xpre = xx)
class(xhyp2) = c( "list", "counterfactual")
identical(xhyp, xhyp2)
lapply(xhyp, head)
lapply(xhyp2, head)

hisx = xhyp$x
ourx = xhyp2$x

diffs = hisx != ourx

## Feed New data to Simulated Coefficient Estiamtes
# Simulate expected probabilities
system.time(mlogit.qoi1 <- mlogitsimev(xhyp,simb,ci=0.67)

mlogit.qoi2 <- mlogitsimev(xhyp2,simb,ci=0.67)

###############################################################################
## BRIAN ADDITIONS: Cleaning up for visualizing with ggplot
# load the libraries for cleaning and visualizing

# the mlogit structure is a collection of arrays but ggplot wants dataframes...
# extracting the arrays as matrices and binding them together
tidy_sim <- rbind(matrix(predictions1$lower, ncol = 3),
                  matrix(predictions1$upper, ncol = 3),
                  matrix(predictions1$pe, ncol = 3)
)

# now formatting the resulting collection into a dataframe that fits our
# visualizing goals
tidy_sim <- data.frame(tidy_sim)
# the name info is specified in Adolph's lecture example, slide 53:
# http://faculty.washington.edu/cadolph/mle/topic5.p.pdf
names(tidy_sim) <- c("invertebrates", "fish", "other")
# adding factors to identify key features of the data - these may seem 
# arbitrary (sorry!) but are intuited from the example itself
tidy_sim$measure_type <- rep(c("lower", "upper", "pe"), each = nrow(predictions1$upper))
tidy_sim$size <- rep(nd1$size)
# collapsing and spreading variables to make visualizing easy - this is
# a bit arbitrary (convenient for how Brian uses ggplot)
tidy_sim <- gather(tidy_sim, food, measure, invertebrates, fish, other)
#tidy_sim$row <- nrow(tidy_sim)
spread(tidy_sim, measure_type, measure)

# plot
ggplot(tidy_sim, aes(x = size, y = pe, 
                     group = food, 
                     ymin = lower, ymax = upper)) + 
    geom_line() +
    # takes the ymin and ymax and draws a ribbon around the lines
    geom_ribbon(alpha = 0.5, aes(fill = food)) + 
    theme_bw() +
    xlab("Length of Gator (Meters)") +
    ylab("p(Primary Food Type Is ... | Length)")



###############################################################################
## END OF SCRIPT
###############################################################################