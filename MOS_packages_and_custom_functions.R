# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/25/2015
# Date updated: 6/9/2015

###############################################################################
## SCRIPT OVERVIEW

# GOAL: This script provides the suite of custom functions that allow the 
#       Multinomial Outcome Simulator (MOS) to generate and manage the 
#       simulations, visualizations, and user interactions that make up the 
#       application.
#
#       The simulation process was heavily inspired by code authored by
#       Chris Adolph (http://faculty.washington.edu/cadolph/) and this script
#       makes use of original and modified versions of this code in several
#       places.
#
#       Where to find the original example code:
#       library(simcf)
#       help(mlogitsimev)
#
#       The original code was not designed for the generalizations or end uses
#       needed by the MOS. It has been corrected, extended, and made into
#       a function set (performance testable).
#
#       Given the importance of the simulation process to the MOS, here is a
#       quick outline of it:
#       - take a given data set and model formula and fit a multinomial model
#         to the data
#       - sample a range of values for each model coefficient with respect to 
#         the uncertainty associated with each coeffecient
#       - based on user inputs, generate a set of counterfactuals - a collection
#         of example cases that span a user-defined range (e.g., if an x-axis
#         is selected, we want a data frame where the x-axis variable varies
#         and all other variables are fixed to a reasonable value)
#       - pass these example cases through our sampled model coefficients, 
#         generated likelihood estimates for each outcome for each case
#       - visualize the relationship between the likelihoods and the outcomes
#         with respect to user inputs (this may or may not require that the
#         likelihood estimates be summarized for each case)
#
# SCRIPT OUTLINE:
# - Load Supporting Packages
#
# - Functions to Simulate Outcome Likelihoods
#   - The family of functions that used to go from base data to a data frame
#     of outcome likelihoods that can be visualized.
#
# - Functions to Visualize Outcome Likelihoods
#   - The family of functions that re-formats (if needed) the likelihood
#     data frames and generates the relevant visualizations (ribbon plot
#     and dot cloud plot).
#
# - Assorted Helper Functions
#   - A function that makes easy-to-access lists of variable names associated
#     with fixed UI options (e.g., which variables are x-axis candidates).
#   - A function to identify which columns in the expanded data frame represent
#     interactions.
#   - A trio of functions to generate additional definitions for user inputs,
#     create the user inputs, and apply the values from the user inputs.
#   - A function to define the ribbon plot summaries based on variable
#     configuration features.
#   - A function to simply request all the values for the input collections.
#     This is primarily useful for testing if the inputs have been interacted
#     with.

###############################################################################
## Load Supporting Packages
#   - devtools and packrat are highly useful to support MOS instances - I
#     suggest installing, learning, and using these applications
# install.packages(c("devtools", "packrat"))

#   - To install the batch of packages below, except for simcf:
# package_names <- c("shiny", "shinyBS", "shinythemes", "Cairo", "nnet",
#                    "combinat", "MASS", "dplyr", "reshape2", "tidyr",
#                    "ggplot2", "scales", "grid")
# install.packages(package_names)

#   - To get simcf (a custom package on github), use the install_github 
#     function from the devtools package:
# devtools::install_github("chrisadolph/tile-simcf", subdir = "simcf")

library(shiny)       # insuring that Shiny is packrat friendly and loaded
library(shinyBS)     # expands base Shiny features (e.g., popovers)
library(shinythemes) # allows using basic bootswatch without custom CSS file
library(Cairo)       # supports improved plot quality across devices
library(nnet)        # for fitting multinomial logit model
library(combinat)    # for building interaction term permutations
library(MASS)        # for multivariate sampling
library(simcf)       # for creating counterfactual sets
library(dplyr)       # serves various formatting needs
library(reshape2)    # serves various formatting needs
library(tidyr)       # serves various formatting needs
library(ggplot2)     # our data visualization workhorse
library(scales)      # for nice plot scales
library(grid)        # need for "unit" function passed to ggplot

###############################################################################
## Functions to Simulate Outcome Likelihoods
#   - create an expanded version of the source dataset (using same model as
#     will be used in the multinom fitting)
#   - fit the multinom model
#   - extract point estimates from the model object
#   - solve the model object Hessian matrix for the covariance matrix
#   - get coefficient estimates via simulation
#   - generate data to feed to coefficient estimates
#   - get the outcome predictions (feed the estimates!)

# create an expanded version of the source dataset
# simply use model.matrix(formula, dataset)

# fit the multinom model
# simply use multinom(formula, dataset, Hess = T)

# function to extract non-reference point estimates from the model object
get_point_estimates <- function(model_object) {
    # determine the number of coefficents (intercepts, predictors, interactions) 
    # and outcomes
    number_coefficients <- length(model_object$coefnames)
    number_outcomes <- length(model_object$lab)
    
    # the multinom function returns a lot of 0s - first we find identify where 
    # the non-reference weights begin (one set of weights per outcome but we 
    # skip the initial reference set)
    index_starts <- NULL
    for(i in 1:(number_outcomes - 1)) {
        # go the end of the current chunk... add two to skip the placeholder 0...
        index_starts[i] <- i * (number_coefficients + 1) + 2
    }
    
    # then we use the start indices and the the number of coefficients to
    # define the index that will align with all the non-reference weights
    wts_index <- NULL
    for(i in index_starts) {
        wts_index <- c(wts_index, i:(i + number_coefficients - 1))
    }
    
    # finally we return the point estimates (weights) for the coefficients
    return(model_object$wts[wts_index])
}

# function to get the covariance matrix from the Hessian in the model object
get_covariance_matrix <- function(model_object) {
    cov_matrix <- chol2inv(chol(model_object$Hess))
    dimnames(cov_matrix) <- dimnames(model_object$Hess)
    
    return(cov_matrix)
}

# function to get coefficient estimates via simulation
get_coefficient_estimates <- function(sample_size, 
                                      point_estimates, 
                                      covariance_matrix,
                                      model_object) {
    # draw parameters, using MASS::mvrnorm
    sim_betas <- mvrnorm(sample_size, point_estimates, covariance_matrix)
    
    # data needs to be re-arranged into an array format
    # first determine array dimensions...
    # looks crazy, but we're essentially taking all the UNIQUE variables in
    # the model formula then subtract 1 for the outcome variable and 1 for the
    # reference variable
    number_arrays  <- length(model_object$lab) - 1
    number_columns <- length(point_estimates) / number_arrays
    number_rows    <- sample_size
    
    # then re-arrange simulates to array format for MNL simulation
    sim_beta_array <- array(NA, dim = c(number_rows, 
                                        number_columns, 
                                        number_arrays)
    ) 
    
    index_starts <- seq(from = 1, to = number_columns * number_arrays, 
                        by = number_columns)
    for(i in 1:number_arrays) {
        sim_beta_array[, , i] <- sim_betas[, index_starts[i]:(index_starts[i] 
                                                              + number_columns -
                                                                  1)]
    }
    
    # return the re-arranged coefficient estimates
    return(sim_beta_array)
}

# generate counterfactual cases to feed the coefficient estimates
get_cf_cases <- function(exp_data, 
                         base_data, 
                         model_object,
                         x_axis_selected, 
                         x_range = NULL, 
                         x_range_density = 100,
                         facet_selected = NULL,
                         interaction_col_names = NA) {
    # check if an explicit range has been provided for the x-axis variable
    if(is.null(x_range)) {
        # if not provided, calculate the range from the dataset
        # floor and ceiling used to insure some space around the observed data
        x_range[1] <- floor(min(exp_data[x_axis_selected]))
        x_range[2] <- ceiling(max(exp_data[x_axis_selected]))
    }
    
    # initialize the minimum set of counterfactuals (the x-axis variable cuts)
    counterfactuals <- seq(x_range[1], x_range[2], length.out = x_range_density)
    # check if a facet variable has been set
    if(!is.null(facet_selected)) {
        # if a facet variable has been set, we expand the counterfactuals
        # to include all x-axis variable/facet variable combinations
        # first we get the levels from original dataset
        var_levels <- with(base_data, levels(get(facet_selected)))
        # get all combinations of the factor name combined with the level name  
        # (in the order that the levels are set)
        factor_var_combinations <- paste0(facet_selected, var_levels)
        # expand the counterfactual set to include appropriate combinations of 
        # the factor/level columns - all having range(0, 1, 1)
        # first treat the initial counterfactual set explicitly as the x_axis
        # cuts
        x_axis_cuts <- counterfactuals
        # then create the long, factor format of all x_axis/factor combos
        # NOTE: sorts the facet_var column which messes up variable order
        counterfactuals <- data.frame(x_axis_cuts, facet_var = 
                                          rep(factor_var_combinations, 
                                              each = length(x_axis_cuts))
        )
        # create the wide format (x_axis_cuts column gets dropped)
        counterfactuals <- spread(counterfactuals, facet_var, x_axis_cuts)
        # correct the variable order
        counterfactuals <- counterfactuals[factor_var_combinations]
        # convert the results to 0s and 1s
        counterfactuals <- ifelse(is.na(counterfactuals), 0, 1)
        # add the x_axis_cuts back in as the first column
        counterfactuals <- data.frame(rep(x_axis_cuts), counterfactuals)
        # drop the reference level
        counterfactuals <- counterfactuals[, -2]
        # label the x_axis_cuts column properly
        names(counterfactuals)[1] <- x_axis_selected
    } else {
        # if no facet variable, simply expand the counterfactual vector to a
        # one-column dataframe and label the column properly
        counterfactuals <- expand.grid(counterfactuals)
        names(counterfactuals) <- x_axis_selected
    }
    # finally, we check if there are additional predictors...
    # (the "formula" call ensures we get the actual formula object rather
    # than a reference to the object)
    exp_formula <- formula(model_object$call[[2]])
    variable_names <- all.vars(exp_formula)
    predictor_names <- variable_names[-1]   # drop off the outcome variable
    # we compare the total number of predictors against the number of columns
    # in the counterfactual table...
    if(length(predictor_names) > ncol(counterfactuals)) {
        # if there are predictors not yet represented in the counterfactual set
        # we define a regex search term that will match the x-axis and (if used)
        # facet variables and we drop ALL partial and complete matches
        # (getting rid of any interaction terms as well)
        if(!is.null(facet_selected)) {
            retained_index <- !grepl(paste(x_axis_selected, 
                                           facet_selected, 
                                           sep = "|"), 
                                     predictor_names)
        } else {
            retained_index <- !grepl(x_axis_selected, 
                                     predictor_names)
        }
        # we drop all the matches, leaving just the (non-interaction) 
        # predictors that we need to fix to a single value
        extra_predictors <- predictor_names[retained_index]
        # we quickly capture the current number of columns in our 
        # countefactual table and add one to it (giving us the index
        # for where we are adding new columns)
        offset_amount <- ncol(counterfactuals) + 1
        
        # now we get the means for the fixed predictors...
        mean_set <- NULL
        for(i in 1:length(extra_predictors)) {
            mean_set[i] <- mean(as.numeric(exp_data[, extra_predictors[i]]), 
                                na.rm = T)
        }
        # and attach those means to the current counterfactual set
        for(i in 1:length(extra_predictors)) {
            counterfactuals <- cbind(counterfactuals, mean_set[i])
        }
        names(counterfactuals)[offset_amount:ncol(counterfactuals)] <- 
            extra_predictors
    }
    
    # now we check if we have interaction columns in our predictors
    if(!is.na(interaction_col_names)) {
        # snag our column names and split terms
        column_names <- interaction_col_names$column_name
        split_terms <- interaction_col_names$split_term
        # if we find them, we pull those terms out
        interaction_index <- column_names %in% predictor_names
        interaction_vars <- column_names[interaction_index]
        # drop any split terms if needed
        split_terms <- split_terms[interaction_index]
        # create a list with the items in each term split based on our split
        # terms
        interaction_list <- list()
        for(index in 1:length(interaction_vars)) {
            current_split <- strsplit(interaction_vars[index], 
                                      split_terms[index], 
                                      perl = TRUE)
            
            interaction_list[[index]] <- current_split
        }
        # we quickly capture the current number of columns in our 
        # countefactual table and add one to it (giving us the index
        # for where we are adding new columns)
        offset_amount2 <- ncol(counterfactuals) + 1
        # for each split term, we take the matching columns in the 
        # counterfactual table and multiply them together to create a new
        # column for the interaction term
        for(current_set in 1:length(interaction_list)) {
            matching_names <- unlist(interaction_list[[current_set]])
            matching_cols <- counterfactuals[matching_names]
            new_col <- apply(matching_cols, 1, prod)
            counterfactuals <- cbind(counterfactuals, new_col)
        }
        # we then give our interaction columns their proper names
        names(counterfactuals)[offset_amount2:ncol(counterfactuals)] <- 
            interaction_vars
    }
    
    # now we quickly reorder our new data object so that the columns match
    # the order of our simulated coefficients objects
    counterfactuals <- counterfactuals[
        all.vars(exp_formula)[2:length(all.vars(exp_formula))]
        ]
    
    # we wrap up by returning the counterfactual set
    return(counterfactuals)
}

# Simulate expected probabilities using the new data and the coefficients:
# This is a minor revision of Chris Adolph's mlogitsimev function from
# his simcf package (https://github.com/chrisadolph/tile-simcf).
#
#       The function passes representative generated data to a collection of
#       simulated coefficients. It then summarizes the results to return key
#       features for each representative case passed to the cofficients:
#       point estimate (mean), upper value (quantile based on given confidence 
#       interval), lower value (quantile based on given confidence interval).
#
#       The updated function (changes are marked with ## REVISION ##):
#       1. Changes the point estimate technique from "mean" (as described above)
#          to "median". This is perhaps a more common choice for this kind of
#          simulation and avoids issues that arise as confidence intervals get
#          narrow (e.g., the mean falling outside the upper and lower 
#          quartiles).
#       2. Allows the user to request the un-summarized likelihoods that results 
#          from feeding the first case of represenative data to the coefficient 
#          estimates. This prevents the normal function behavior, which returns 
#          a summary of the likelihoods across all the coefficient sets for 
#          each case.
#       3. Adjusts order of the outcome columns to match intuitive expectations
#          (reference outcome is ordered as the FIRST column rather than LAST).
#          Originally, the reference outcome was ordered as the LAST column, but
#          in most models/outputs the reference outcome is ordered FIRST.
MOS_mlogitsimev <- function (x, b, ci = 0.95, constant = 1, z = NULL, g = NULL, 
                             predict = FALSE, sims = 10, 
                             ## REVISION ##
                             # added return_first_case_likelihoods argument - 
                             # if TRUE, the function will be interrupted after 
                             # feeding the first row of counterfactual data to  
                             # the coefficients and we'll get back the 
                             # unsummarized likelihood for each coefficient set 
                             # for that single row
                             return_first_case_likelihoods = FALSE) 
{
    if (!is.array(b)) {
        stop("b must be an array")
    }
    if (any(class(x) == "counterfactual") && !is.null(x$model)) {
        x <- model.matrix(x$model, x$x)
        x <- array(x, dim = c(nrow(x), ncol(x), dim(b)[3]))
    }
    else {
        if (any(class(x) == "list")) 
            x <- x$x
        if (is.data.frame(x)) 
            x <- as.matrix(x)
        if (!is.array(x)) {
            if (!is.matrix(x)) {
                x <- t(x)
            }
            x <- array(x, dim = c(nrow(x), ncol(x), dim(b)[3]))
        }
        else {
            x <- array(x, dim = c(nrow(x), ncol(x), dim(b)[3]))
        }
        if (!is.na(constant)) {
            xnew <- array(NA, dim = c(nrow(x), (ncol(x) + 1), 
                                      dim(b)[3]))
            for (i in 1:dim(x)[3]) {
                xnew[, , i] <- appendmatrix(x[, , i, drop = FALSE], 
                                            rep(1, dim(x)[1]), constant)
            }
            x <- xnew
        }
    }
    if (!is.null(g)) {
        usegamma <- TRUE
    }
    else {
        usegamma <- FALSE
    }
    if (usegamma && !is.array(z)) {
        stop(paste0("if g is provided, z must be an array with dimension 3 ",
                    "equal to the number of categories"))
    }
    esims <- nrow(as.matrix(b))
    res <- list(lower = array(0, dim = c(dim(x)[1], (dim(x)[3] + 1), 
                                         length(ci))), 
                upper = array(0, dim = c(dim(x)[1], (dim(x)[3] + 1), 
                                         length(ci)))
    )
    if (predict) 
        res$pv <- NULL
    for (iscen in 1:dim(x)[1]) {
        simdenom <- 0
        for (icat in 1:(dim(b)[3])) {
            if (usegamma) {
                newdenom <- exp(b[, , icat] %*% x[iscen, , icat] + 
                                    g %*% z[iscen, , icat])
                if(any(is.infinite(newdenom))) {
                    stop("Getting unreasonable values (e.g., 'infinite') when ",
                         "trying to simulate data. Your model may be over fit.")
                }
            } else {
                newdenom <- exp(b[, , icat] %*% x[iscen, , icat])
                if(any(is.infinite(newdenom))) {
                    stop("Getting unreasonable values (e.g., 'infinite') when ",
                         "trying to simulate data. Your model may be over fit.")
                }
            }
            simdenom <- simdenom + newdenom
        }
        if (usegamma) {
            simdenom <- simdenom + exp(g %*% z[iscen, , dim(z)[3]])
        }
        else {
            simdenom <- simdenom + 1
        }
        simy <- matrix(NA, nrow = dim(b)[1], ncol = (dim(b)[3] + 1))
        
        for (icat in 1:dim(x)[3]) {
            if (usegamma) 
                simy[, icat] <- exp(b[, , icat] %*% x[iscen, , icat] + 
                                        g %*% z[iscen, , icat])/simdenom
            else simy[, icat] <- exp(b[, , icat] %*% x[iscen, , icat])/simdenom
        }
        if (usegamma) 
            simy[, ncol(simy)] <- exp(g %*% z[iscen, , dim(g)[3]])/simdenom
        else simy[, ncol(simy)] <- 1/simdenom
        
        simy <- apply(simy, 2, sort)
        
        ## REVISION ##
        # reorder columns so the REFERENCE OUTCOME is now the FIRST column
        simy <- simy[, c(ncol(simy), 1:(ncol(simy) - 1))]
        
        ## REVISION ##
        # if requested, return the first case likelihoods for each coefficient
        # set
        if(return_first_case_likelihoods) {
            return(simy)
        }
        
        ## REVISION ##
        # technique for calculating point estimate (pe) changed from mean to
        # median
        res$pe <- rbind(res$pe, apply(simy, 2, median))
        length.simy <- nrow(simy)
        low <- up <- NULL
        for (k in 1:length(ci)) {
            for (icat in 1:(dim(b)[3] + 1)) {
                res$lower[iscen, icat, k] <- 
                    rbind(low, quantile(simy[, icat], 
                                        probs = (1 - ci[k])/2))
                res$upper[iscen, icat, k] <- 
                    rbind(up, quantile(simy[, icat], 
                                       probs = (1 - (1 - ci[k])/2)))
            }
        }
        if (predict) {
            pv <- NULL
            for (ipred in 1:dim(b)[1]) {
                pv <- c(pv, resample(1:dim(simy)[2], size = sims, 
                                     prob = simy[ipred, ], replace = TRUE))
            }
            res$pv <- rbind(res$pv, pv)
            low <- up <- NULL
            for (k in 1:length(ci)) {
                for (icat in 1:(dim(b)[3] + 1)) {
                    res$plower[iscen, icat, k] <- 
                        rbind(low, quantile(pv[, icat], 
                                            probs = (1 - ci[k])/2))
                    res$pupper[iscen, icat, k] <- 
                        rbind(up, quantile(pv[, icat], 
                                           probs = (1 - (1 - ci[k])/2)))
                }
            }
        }
    }
    res
}

###############################################################################
## Functions to Visualize Outcome Likelihoods

format_for_ribbon_plot <- function(raw_likelihoods, 
                                   model_object, 
                                   base_data,
                                   counterfactuals,
                                   x_axis_selected = NA, 
                                   facet_selected = NULL,
                                   explicit_outcome_order = NA) {
    
    # the mlogit structure is a collection of arrays but ggplot wants dataframes
    # first we extract the arrays as matrices and bind them together
    # NOTE: the lower/upper arrays will have as many dimensions as there are
    #       confidence intervals (here we have 2 dimensions because we ask
    #       for 95 and 50 percent CI in our mlogitsimev call)
    num_col <- ncol(raw_likelihoods$lower)
    tidy_sim <- rbind(matrix(raw_likelihoods$lower[, , 1], ncol = num_col),
                      matrix(raw_likelihoods$lower[, , 2], ncol = num_col),
                      matrix(raw_likelihoods$upper[, , 1], ncol = num_col),
                      matrix(raw_likelihoods$upper[, , 2], ncol = num_col),
                      matrix(raw_likelihoods$pe, ncol = num_col)
    )
    
    # then we format the resulting collection to be properly grouped and
    # labelled for visualizing
    tidy_sim <- data.frame(tidy_sim)
    # the outcome names are retained in the model object - we take these and
    # label our prediction dataframe columns accordingly
    names(tidy_sim) <- model_object$lab
    # add a grouping variable for the three types of measures we get from
    # the prediction object
    tidy_sim$measure_type <- rep(c("lower95", "lower50", 
                                   "upper95", "upper50", 
                                   "pe"), 
                                 each = nrow(raw_likelihoods$upper))
    # if available, we also add the predictor (x-axis) value that will link the 
    # unique sets (lower, upper, pe) - this should naturally repeat to the 
    # appropriate length
    if(!is.na(x_axis_selected)) {
        tidy_sim$predictor <- counterfactuals[[x_axis_selected]]
    } else {
        # if no x-axis given, we just slap on a row-count
        tidy_sim$predictor <- 1:nrow(counterfactuals)
    }
    # finally, if there is a facet variable set, we also add it as a grouping 
    # variable (create a new summary variable rather than deal with the 
    # already existing columns)
    if(!is.null(facet_selected)) {
        # we get the levels from the original data object
        factor_levels <- levels(base_data[, facet_selected])
        # the number of repitions of the factor is determined by the length
        # of the x_axis variable / number of unique factor levels
        num_reps <- nrow(raw_likelihoods$upper) / length(factor_levels)
        # finally add the grouping variable
        tidy_sim$facet <- rep(factor_levels, each = num_reps)
    }    
    
    # collapsing and spreading variables to make visualizing easy
    # (this is a tad arbitrary - it is consisent with Brian's interpretation of
    # good ggplot practice)
    if(!is.null(facet_selected)) {
        # if a facet variable is set, respect it...
        tidy_sim <- gather(tidy_sim, outcome, likelihood, -measure_type, 
                           -predictor, -facet)    
    } else {
        # otherwise don't because it's not there
        tidy_sim <- gather(tidy_sim, outcome, likelihood, -measure_type, 
                           -predictor)
    }
    tidy_sim <- spread(tidy_sim, measure_type, likelihood)
    
    # check if an explicit order is provided for the levels of the "outcome"
    # variable - apply it if given
    if(!is.na(explicit_outcome_order)) {
        tidy_sim$outcome <- factor(tidy_sim$outcome, explicit_outcome_order)
    }
    
    # now we need to take our well formed data object and switch it to a long
    # format so that it is ready for the geom_ribbon needs
    
    if(is.null(facet_selected)) {
        id_columns <- c("predictor", "outcome")
    } else {
        id_columns <- c("predictor", "outcome", "facet")
    }
    
    fl_long <- tidy_sim %>%
        # we want to work with the lower/upper values separately, so we first 
        # remove one half of the values (here the upper) and the unneeded pe 
        # column
        dplyr::select(-pe, -upper95, -upper50) %>%
        # then we melt our data frame so that the 50/95 values are in the same
        # column
        melt(id.vars = id_columns, 
             value.name = "lower_values", 
             variable.name = "ci") %>%
        # now we extract the 50/95 from the 50/95 levels by dropping all the 
        # character values from the string
        mutate(ci = as.numeric(gsub(pattern = "^[a-z]*", 
                                    x = ci, 
                                    replacement = ""))) %>%
        # now we repeat for the other set of values (upper) and then join the
        # results into a single dataframe with our lower and upper values in 
        # their own columns
        left_join(
            tidy_sim %>%
                dplyr::select(-pe, -lower95, -lower50) %>%
                melt(id.vars = id_columns, 
                     value.name = "upper_values", 
                     variable.name = "ci") %>%
                mutate(ci = as.numeric(gsub(pattern = "^[a-z]*", 
                                            x = ci, 
                                            replacement = ""))),
            by = c(id_columns, "ci")) %>%
        # and we wrap up by adding an interaction variable so that our ggplot
        # group correctly recognizes that we want to group by both outcome and 
        # confidence interval group (50 or 95)
        mutate(group = interaction(ci, outcome))
    
    # returning our visualization-ready data
    return(fl_long)
}

get_ribbon_plot <- function(formatted_likelihoods,
                            facet_selected = NULL,
                            x_lab = "Predictor", 
                            y_lab = "Probability of Outcome",
                            custom_colors = NULL,
                            custom_x_axis_ticks = NULL) {
    
    plot_object <- ggplot(formatted_likelihoods, 
                          aes(x = predictor, group = group)) +
        # the geoms
        geom_ribbon(aes(fill = outcome, 
                        ymin = lower_values, ymax = upper_values,
                        alpha = factor(ci))) +
        # label adjustements
        labs(x = x_lab, y = y_lab) +
        # scale adjustments
        scale_alpha_manual(values = c(0.4, 0.5), guide = FALSE) +
        scale_y_continuous(limits = c(0, 1),
                           labels = percent,
                           expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0)) +
        # theme adjustments
        MOS_theme +
        guides(fill = guide_legend(title = NULL))
    
    # if custom_x_axis_ticks are provided, add these to an appropriate place
    # inside the plot
    if(!is.null(custom_x_axis_ticks)) {
        plot_object <- plot_object +
            scale_x_continuous(expand = c(0, 0), 
                               labels = custom_x_axis_ticks)
    }
    
    # if custom colors are provided, adjust the color scale and update theme
    if(!is.null(custom_colors)) {
        plot_object <- plot_object + 
            scale_fill_manual(values = custom_colors)
    }
    
    # if a facet variable is set, add the facet layer to the plot object
    if(!is.null(facet_selected)) {
        plot_object <- plot_object + 
            facet_wrap(~ facet, ncol = 3) +
            theme(panel.margin = unit(1, "lines"),
                  strip.text.x = element_text(size = 8)
            )
        # if custom x-axis ticks are provided, also want to tweak the x-axis
        # tick text and orientation to minimize collisions/overlap between
        # facets
        if(!is.null(custom_x_axis_ticks)) {
            plot_object <- plot_object +
                theme(axis.text.x = element_text(size = 8, angle = 45, 
                                                 hjust = 1, vjust = 1)
                )
        }
    }
    
    # return the plot object
    return(plot_object)
}

get_dot_cloud_plot <- function(formatted_likelihoods,
                               x_lab = "Simulated Outcome Probability", 
                               y_lab = "",
                               custom_colors = NULL) {
    # build the base plot object
    plot_object <- ggplot(formatted_likelihoods, 
                          aes(x = outcome, y = single_pe,
                              color = outcome, alpha = 0.10)) + 
        # the geoms
        geom_jitter(position = position_jitter(width = 0.25, height = 0)) +
        # label adjustments
        labs(title = "The Likelihood of Each Outcome Across 1000 Simulations",
             x = x_lab, y = y_lab) +
        # scale adjustments
        scale_x_discrete(limits = rev(levels(formatted_likelihoods$outcome))) +
        scale_y_continuous(limits = c(0, 1),
                           labels = scales::percent) +
        # theme adjustments
        MOS_theme +
        theme(legend.position="none") +
        coord_flip()
    
    # if custom colors are provided, adjust the color scale and update theme
    if(!is.null(custom_colors)) {
        plot_object <- plot_object + 
            scale_color_manual(values = custom_colors)
    }
    
    # return the plot object
    return(plot_object)
}

###############################################################################
## Assorted Helper Functions

# Extract the fixed ui options (the levels for any ui features that are
# generated statically - such as the x-axis choices - rather than dynamically -
# such as the sliders).
get_fixed_ui_options <- function(variable_config_list) {
    # collect the x-axis options names and definitions
    x_axis_options <- c()
    x_axis_definitions <- c()
    for(index in 1:length(variable_config_list)) {
        if(variable_config_list[[index]]$x_axis_candidate) {
            current_name       <- variable_config_list[[index]]$pretty_name
            current_def        <- variable_config_list[[index]]$definition
            x_axis_options     <- c(x_axis_options, current_name)
            x_axis_definitions <- c(x_axis_definitions, current_def)
        }
    }
    
    # collect the facet options names and definitions
    facet_options <- c()
    facet_definitions <- c()
    for(index in 1:length(variable_config_list)) {
        if(variable_config_list[[index]]$facet_candidate) {
            current_name      <- variable_config_list[[index]]$pretty_name
            current_def       <- variable_config_list[[index]]$definition
            facet_options     <- c(facet_options, current_name)
            facet_definitions <- c(facet_definitions, current_def)
        }
    }
    
    # return all option collections
    list(x_axis_options     = x_axis_options, 
         x_axis_definitions = x_axis_definitions,
         facet_options      = facet_options,
         facet_definitions  = facet_definitions)
}

# We need to be able to identify columns that are the result of interactions
# between base variables. However, the base data expansion makes it difficult
# to recover these columns - it also expands factor levels and does not use
# a unique separator for interaction combinations v. factor levels. This
# function finds the possible name permutations that may result from
# interactions and identifies columns that have matches - which will be the
# interaction columns.
get_interaction_col_names <- function(base_formula, exp_data) {
    # convert the base formula into a single character string (ignoring the 
    # outcome variable)
    formula_string <- as.character(base_formula)[[3]]
    
    # parse all the separate terms (not variables but rather anything that
    # occurs before or after a "+" symbol)
    formula_parsed <- strsplit(formula_string, "+", fixed = TRUE)
    
    # get rid of any spaces (note that strsplit returns a list - we unlist to 
    # make sure we work with a character vector)
    formula_parsed <- gsub(" ", "", unlist(formula_parsed))
    
    # now we collect just those terms that have an interaction symbol "*"
    # TODO check for : to mark interactions
    interaction_terms <- grep("\\*|:", formula_parsed, value = TRUE)
    
    # quickly check to see if there are any interaction terms at all - if none
    # we want to return "NA" so that later steps in this function don't fail
    # and so that we can identify the lack of interaction terms correctly in
    # later functions
    if(length(interaction_terms) == 0) {
        return(NA)
    }
    
    # for each term, we extract the variable names
    interaction_terms <- strsplit(interaction_terms, "\\*|:")
    
    # for each term, we need to construct all possible combinations of the 
    # variable names (variable names separated by a "." and - for factor-factor
    # interactions - possibly some other "." and text) - these are what we 
    # will use to identity the interaction data columns in the expanded data 
    # frame
    # 1. get the permutations of the variable name strings
    interaction_combos <- lapply(interaction_terms, permn)
    # 2. build the regex search strings to test if there are column matches 
    #    for each permutation (first term will always start the column name,
    #    second term will always occur somewhere in the string immediately
    #    after a period)
    reg_set <- c()
    
    for(combo_index in 1:length(interaction_combos)) {
        current_combo <- interaction_combos[[combo_index]]
        reg_combo <- list()
        
        for(subset_index in 1:length(current_combo)) {
            current_subset <- current_combo[[subset_index]]
            
            # first pattern
            first_pattern <- c(paste0("^", current_subset[[1]]))
            
            # second pattern (note that we only need to make up to the two-
            # way match - all three-way+ matches will be matched by a two-way;
            # also note that we match the PERIOD that occurs before the 
            # variable name - this let's us re-use this term to split
            # the name later on)
            second_pattern <- paste0("\\.(?=", 
                                     current_subset[[2]], 
                                     "[a-zA-Z])|",
                                     "\\.(?=", current_subset[[2]], "$)")
            
            reg_combo[[subset_index]] <- c(first_pattern, second_pattern)
        }
        
        reg_set[[combo_index]] <- reg_combo
    }
    
    # 3. test the regex pairs against the exp_data names to see which columns
    #    are interaction columns
    interaction_test_collection <- lapply(reg_set, function(x) {
        pair_collection <- list()
        for(index in 1:length(x)) {
            current_pair <- x[[index]]
            pair_collection[[index]] <- grepl(current_pair[[1]], 
                                              names(exp_data),
                                              perl = TRUE) & 
                grepl(current_pair[[2]], 
                      names(exp_data),
                      perl = TRUE)
        }
        
        return(pair_collection)
    })
    
    # 4. collapse the test collection so that it is a single index - any column
    #    that had one or more matches to a test pair is an interaction column
    interaction_matrix <- matrix(unlist(interaction_test_collection), 
                                 ncol = length(names(exp_data)), 
                                 byrow = TRUE)
    
    interaction_index <- apply(interaction_matrix, 2, any)
    
    # 5. get the subset of column names that match a permutation
    interaction_col_names <- names(exp_data)[interaction_index]
    
    # 6. get the regex terms that will allow us to split the columns (these
    #    may be in a funny order)
    reg_set_matches <- apply(interaction_matrix, 1, any)
    
    reg_set_matrix <- matrix(unlist(reg_set), ncol = 2, byrow = T)
    
    split_terms <- reg_set_matrix[,2][reg_set_matches]
    
    # 6. pair the column names with their regex split terms (fix any order
    #    issues)
    # NOTE: this first step gets just the first matching position - if the same
    #       variable is the second of multiple interaction pairs, multiple 
    #       values will be returned; this is not an issue because of how we
    #       do the next step
    split_term_order <- sapply(split_terms, function (x) 
        grep(x, interaction_col_names, perl = TRUE)[1])
    
    #       recreating the terms collection in the correct order (this will fill
    #       in duplicate matches as needed)
    split_terms <- split_terms[c(split_term_order)]
    
    # 7. join the terms with the col names
    interaction_cols <- data.frame(column_name = interaction_col_names, 
                                   split_term = split_terms,
                                   stringsAsFactors = FALSE)
    
    # return the collection of names and split terms
    return(interaction_cols)
}

# This function expands the variable configuration object to include some more
# features, specifically those needed to define the sliders. It calculates
# these from the base data object.
add_input_features <- function(variable_config_object, base_data) {
    # loop over the variables specified the variable configuration object
    for(index in 1:length(variable_config_object)) {
        # adjust object name to be more manageable
        vc <- variable_config_object
        
        # grab the current variable raw name
        current_var <- names(vc)[[index]]
        
        # if it's numeric, calculate the relevant values, otherwise assign NA
        # to the values so the properties exist but are appropriate for a non-
        # numeric variable
        if(is.numeric(base_data[[current_var]])) {
            current_median <- median(base_data[[current_var]])
            current_range  <- range(base_data[[current_var]])
            
            # apply the variable's transform_to_ui function (making the values
            # ui friendly)
            current_median   <- vc[[index]]$transform_for_ui(current_median)
            current_range[1] <- vc[[index]]$transform_for_ui(current_range[1])
            current_range[2] <- vc[[index]]$transform_for_ui(current_range[2])
            
            # in case of reverse transformations, make sure range is ordered
            # ascending
            current_range <- sort(current_range)
            
            # round the range values so that ugly values are more ui friendly
            current_range[1] <- floor(current_range[1])
            current_range[2] <- ceiling(current_range[2])
        } else {
            current_median <- NA
            current_range <- NA
        }
        
        # if median/range are NA, grab the variable levels, otherwise set to NA
        if(is.na(current_median)) {
            current_levels <- levels(base_data[[current_var]])
        } else {
            current_levels <- NA
        }
        
        # add the values to the variable configuration
        variable_config_object[[current_var]]$ui_median <- current_median
        variable_config_object[[current_var]]$ui_min <- current_range[1]
        variable_config_object[[current_var]]$ui_max <- current_range[2]
        variable_config_object[[current_var]]$ui_levels <- current_levels
    }
    
    # return the update variable_config_object
    return(variable_config_object)
}

# This function generates the actual slider objects. It expects the variable
# configuration object to determine which variables to make sliders for, but
# will also accept a vector of raw variable names that overrides the variable 
# configuration file to exclude selected variables. A unique "append" name must
# also be provided to ensure that separate slider sets to not share name space.
make_inputs <- function(variable_config_list, 
                        variables_to_drop = NA,
                        append_name,
                        facet_as_dropdown = FALSE) {
    # index which variables are slider candidates
    slider_index <- unlist(lapply(variable_config_list, 
                                  function(x) x$slider_candidate == TRUE))
    
    # if a vector of variables to drop has been provided, adjust their indices 
    # to FALSE so sliders are not made for them (this is primarily useful for
    # dropping the x-axis variable where needed)
    if(!is.na(variables_to_drop)) {
        for(index in 1:length(variables_to_drop)) {
            slider_index[variables_to_drop[index]] <- FALSE
        }
    }
    
    # if there are no slider candidates, we skip creating a slider set and
    # set the slider_set value so that it is handled properly later
    if(!any(slider_index)) {
        slider_set = NULL
    } else {
        
        # subset variable_config_list to just get the slider candidates
        # (excluding the x-axis variabe)
        selected_sliders <- variable_config_list[slider_index]
        
        # generate the sliders (and their popovers)
        slider_set <- lapply(1:length(selected_sliders), function(i) {
            popify(
                sliderInput(
                    inputId = paste0(append_name,
                                     "_",
                                     names(selected_sliders)[i]), 
                    label   = selected_sliders[[i]]$pretty_name,
                    min     = selected_sliders[[i]]$ui_min, 
                    max     = selected_sliders[[i]]$ui_max,
                    value   = selected_sliders[[i]]$ui_median,
                    step    = ifelse(is.na(selected_sliders[[i]]$slider_rounding),
                                     0.01,
                                     selected_sliders[[i]]$slider_rounding)),
                
                title     = selected_sliders[[i]]$pretty_name,
                content   = selected_sliders[[i]]$definition,
                placement = "bottom",
                trigger   = "hover"
            )
            
        })
    }
    
    # if facets are desired, we'll make drop-downs for each of those as well
    # (only really appropriate for Single Case mode)
    if(facet_as_dropdown) {
        # index which variables are facet candidates
        facet_index <- unlist(lapply(variable_config_list, 
                                     function(x) x$facet_candidate == TRUE))
        
        # if a vector of variables to drop has been provided, adjust their  
        # indices to FALSE so facets are not made for them
        if(!is.na(variables_to_drop)) {
            for(index in 1:length(variables_to_drop)) {
                facet_index[variables_to_drop[index]] <- FALSE
            }
        }
        
        # subset variable_config_list to just get the facet candidates
        selected_dropdowns <- variable_config_list[facet_index]
        
        # generate the dropdowns (and their popovers)
        dropdown_set <- lapply(1:length(selected_dropdowns), function(i) {
            popify(
                selectInput(
                    inputId = paste0(append_name,
                                     "_",
                                     names(selected_dropdowns)[i]), 
                    label   = selected_dropdowns[[i]]$pretty_name,
                    choices = selected_dropdowns[[i]]$ui_levels),
                
                title     = selected_dropdowns[[i]]$pretty_name,
                content   = selected_dropdowns[[i]]$definition,
                placement = "right",
                trigger   = "hover",
                options = list(container = "body")
            )
        })
    }
    
    if(exists("dropdown_set")) {
        return(c(slider_set, dropdown_set))
    } else {
        return(slider_set)   
    }
}

# This function updates the base data object to create a data object adjusted
# for appropriate slider values. The inputs to the function should echo the
# inputs used to make the slider set with the addition of specifying the 
# target object. To establish the reactive link, we also need to explicitly
# pass the input object.
apply_input_values <- function(update_target,
                               interaction_col_names,
                               variable_config_list,
                               input_call,
                               append_name,
                               base_data,
                               use_slider_values = TRUE,
                               use_dropdown_values = FALSE,
                               variables_to_drop = NA
) {
    # index which variables are input candidates - the way we build this index
    # will depend on which input source(s) we want to apply
    if(use_slider_values == TRUE & use_dropdown_values == FALSE) {
        input_index <- unlist(lapply(variable_config_list, 
                                     function(x) x$slider_candidate == TRUE))
    } else if(use_slider_values == TRUE & use_dropdown_values == TRUE) {
        input_index <- unlist(lapply(variable_config_list, 
                                     function(x) x$slider_candidate == TRUE ||
                                         x$facet_candidate == TRUE))
    } else {
        input_index <- unlist(lapply(variable_config_list, 
                                     function(x) x$facet_candidate == TRUE))
    }
    
    # if a vector of variables to drop has been provided, adjust their indices 
    # to FALSE so inputs are not made for them (this is primary use for this
    # is dropping the x-axis variable when needed)
    if(!is.na(variables_to_drop)) {
        for(index in 1:length(variables_to_drop)) {
            input_index[variables_to_drop[index]] <- FALSE
        }
    }
    
    # subset variable_config_list to just get the input candidates
    selected_inputs <- variable_config_list[input_index]
    
    # update the values based on current slider inputs
    for(i in 1:length(selected_inputs)) {
        # grab the key details
        current_var <- names(selected_inputs)[[i]]
        input_name <- paste0(append_name, "_", current_var)
        input_value <- isolate(input_call[[input_name]])
        input_type <- ifelse(selected_inputs[[i]]$slider_candidate,
                             "slider",
                             "dropdown")
        input_trans <- selected_inputs[[current_var]]$transform_for_model 
        
        # all the input variables initialize as "NULL" - we want to avoid
        # working with them until they've been assigned a value so we simply
        # return the update_target in that case
        # NOTE: input_call[[current_var]] IS a reactive link (actually a  
        #       flexible set of reactive links) - it links to the inputs 
        #       dynamically generated by the output$APPEND_input_set observers
        if(is.null(isolate(input_call[[input_name]]))) {
            return(update_target)
        }
        
        # apply the relevant transformation to convert slider values
        # to model-appropriate values
        model_value <- input_trans(input_value)
        
        # at this point, we apply the input value differently if the value came
        # from a slider or a dropdown;
        # the expanded dataset includes a single column for each slider
        # variable + appropriate interaction columns
        # in contrast, the dataset includes a column for each dropdown level 
        # except the reference level (the first factor level however the levels 
        # were ordered in base_data) + appropriate interaction columns for EACH
        # non-reference levels
        if(input_type == "slider") {
            # update the matching update_target column with the 
            # current slider value
            update_target[current_var] <- model_value
        } else if (input_type == "dropdown") {
            # we are going to have to update ALL of the non-interaction
            # columns associated with this factor - we want all the non-
            # selected levels to be set to "0" (not true) and the selected
            # level to be set to "1" (true)
            
            # we snag the variable levels from the base data
            var_levels <- levels(base_data[[current_var]])
            # we replace any non-alphanumeric values with "." to match the 
            # subsitution that occurs during the expansion from base_data to 
            # exp_data
            var_levels <- gsub("[^[:alnum:]]", ".", var_levels)
            # we create the non-interaction column names by combining the
            # raw variable name with the level names
            single_col_names <- paste(current_var, var_levels, sep = "")
            # we drop the reference level
            single_col_names <- single_col_names[-1]
            
            # we define the column that matches our selected factor level
            matching_col <- paste(current_var,
                                  gsub("[^[:alnum:]]", ".", input_value),
                                  sep = "")
            
            # we initially set all remaining the columns to "0"
            for(current_col in single_col_names) {
                update_target[current_col] <- 0
            }
            
            # finally, we verify that our matching column is present in the
            # expanded data set - if it is, we update it; if it isn't, it's the
            # reference level and we stop since we have set all non-reference
            # levels to "0"
            if(any(grepl(matching_col, names(update_target), fixed = TRUE))) {
                update_target[matching_col] <- 1
            }
        }
    }
    
    # now we quickly refresh all of the interaction columns (just doing this by
    # default is, on average, faster than testing if each input is related to
    # an interaction; it is also technically much easier to implement; this
    # claim may not hold if there are a large number of interactions)
    if(!is.na(interaction_col_names)) {
        # create a list of the interaction column names split in half (by their
        # matching split term)
        column_names <- interaction_col_names$column_name
        split_terms <- interaction_col_names$split_term
        interaction_list <- list()
        for(index in 1:length(column_names)) {
            current_split <- strsplit(column_names[index], 
                                      split_terms[index], 
                                      perl = TRUE)
            
            interaction_list[[index]] <- current_split
        }
        
        # update the interaction variables by multiplying their
        # source columns together
        for(current_set in 1:length(interaction_list)) {
            matching_names <- unlist(interaction_list[[current_set]])
            matching_cols <- update_target[matching_names]
            updated_col <- apply(matching_cols, 1, prod)
            update_target[column_names[[current_set]]] <- updated_col
        }
    }
    
    return(update_target)
}

# We need to summarize the key details of each ribbon plot. This builds a text
# string (with HTML formatting) that can be used for that purpose.
build_ribbon_summary <- function(x_axis_raw_name,
                                 facet_raw_name,
                                 variable_config_list,
                                 include_plot_summary) {
    
    # extract the matching x-axis config variable
    x_axis_target <- variable_config_list[[x_axis_raw_name]]
    
    # if a facet variables has been selected, we exract that config variable
    # as well
    if(!is.null(facet_raw_name)) {
        facet_target <- variable_config_list[[facet_raw_name]]
    } else {
        facet_target <- NA
    }
    
    # combine the relevant variable features to make the summary
    if(!is.na(facet_target)) {
        ribbon_defs <- paste0(
            "<strong>", x_axis_target$pretty_name, "</strong><br>",
            x_axis_target$definition, "<br><br>",
            
            "<strong>", facet_target$pretty_name, "</strong><br>",
            facet_target$definition, "<br><br>"
        )
    } else {
        ribbon_defs <- paste0(
            "<strong>", x_axis_target$pretty_name, "</strong><br>",
            x_axis_target$definition, "<br><br>"
        )
    }
    
    if(include_plot_summary) {
        ribbon_summary <- paste0(
            ribbon_defs,
            "<strong>Key Trends for the X-Axis Variable</strong><br>",
            x_axis_target$ribbon_plot_summary
        )
    } else {
        ribbon_summary <- paste0(
            ribbon_defs,
            "<strong>Key Trends for the X-Axis Variable</strong><br>",
            "Not available when using Advanced Options.")
    }
    
    # return the text string
    return(ribbon_summary)
}

# We need to be able to simply return the collections of inputs. This will
# allow us to test for when inputs in the collection have changed. The primary
# use for this is adjusting the Update/Simulate buttons to get user attention.
return_inputs <- function(variable_config_list,
                          input_call,
                          append_name,
                          base_data,
                          use_slider_values = TRUE,
                          use_dropdown_values = FALSE,
                          variables_to_drop = NA) {
    # index which variables are input candidates - the way we build this index
    # will depend on which input source(s) we want to apply
    if(use_slider_values == TRUE & use_dropdown_values == FALSE) {
        input_index <- unlist(lapply(variable_config_list, 
                                     function(x) x$slider_candidate == TRUE))
    } else if(use_slider_values == TRUE & use_dropdown_values == TRUE) {
        input_index <- unlist(lapply(variable_config_list, 
                                     function(x) x$slider_candidate == TRUE ||
                                         x$facet_candidate == TRUE))
    } else {
        input_index <- unlist(lapply(variable_config_list, 
                                     function(x) x$facet_candidate == TRUE))
    }
    
    # if a vector of variables to drop has been provided, adjust their indices 
    # to FALSE so inputs are not made for them (this is primary use for this
    # is dropping the x-axis variable when needed)
    if(!is.na(variables_to_drop)) {
        for(index in 1:length(variables_to_drop)) {
            input_index[variables_to_drop[index]] <- FALSE
        }
    }
    
    # subset variable_config_list to just get the input candidates
    selected_inputs <- variable_config_list[input_index]
    
    # return the current slider values (triggering reactive link to each)
    input_values <- c()
    for(i in 1:length(selected_inputs)) {
        # grab the key details
        current_var <- names(selected_inputs)[[i]]
        input_name <- paste0(append_name, "_", current_var)
        input_values <- c(input_values, input_call[[input_name]])
    }
    
    # return the values to avoid funky return errors
    return(input_values)
}

###############################################################################
## END OF SCRIPT
###############################################################################