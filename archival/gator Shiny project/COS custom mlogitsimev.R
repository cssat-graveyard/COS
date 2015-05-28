# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 4/1/2015
# Date updated: 

###############################################################################
## SCRIPT OVERVIEW

# goal: This is a VERY minor revision of Chris Adolph's mlogitsimev function
#       from his simcf package (https://github.com/chrisadolph/tile-simcf).
#
#       The function passes representative generated data to a collection of
#       simulated coefficients. It then summarizes the results to return key
#       features for each representative case passed to the cofficients:
#       point estimate (mean), upper value (quartile based on given confidence 
#       interval), lower value (quartile based on given confidence interval).
#
#       The updated function changes the point estimate technique from "mean"
#       (as described above) to "median". This is perhaps a more common choice
#       for this kind of simulation and avoids issues that arise as confidence
#       intervals get narrow (e.g., the mean falling outside the upper and 
#       lower quartiles).

# sketch of script
# - the function as written by Chris Adolph
#   - the revision is marked with a comment ## REVISION ##

###############################################################################
## STEP

mlogitsimev_med <- function (x, b, ci = 0.95, constant = 1, z = NULL, g = NULL, 
                             predict = FALSE, sims = 10) 
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
        stop("if g is provided, z must be an array with dimension 3 equal to the number of categories")
    }
    esims <- nrow(as.matrix(b))
    res <- list(lower = array(0, dim = c(dim(x)[1], (dim(x)[3] + 
                                                         1), length(ci))), upper = array(0, dim = c(dim(x)[1], 
                                                                                                    (dim(x)[3] + 1), length(ci))))
    if (predict) 
        res$pv <- NULL
    for (iscen in 1:dim(x)[1]) {
        simdenom <- 0
        for (icat in 1:(dim(b)[3])) {
            if (usegamma) {
                newdenom <- exp(b[, , icat] %*% x[iscen, , icat] + 
                                    g %*% z[iscen, , icat])
            }
            else {
                newdenom <- exp(b[, , icat] %*% x[iscen, , icat])
            }
            simdenom <- simdenom + newdenom
        }
        if (usegamma) {
            simdenom <- simdenom + exp(g %*% z[iscen, , dim(z)[3]])
        }
        else {
            simdenom <- simdenom + 1
        }
        simy <- matrix(NA, nrow = dim(b)[1], ncol = (dim(b)[3] + 
                                                         1))
        for (icat in 1:dim(x)[3]) {
            if (usegamma) 
                simy[, icat] <- exp(b[, , icat] %*% x[iscen, 
                                                      , icat] + g %*% z[iscen, , icat])/simdenom
            else simy[, icat] <- exp(b[, , icat] %*% x[iscen, 
                                                       , icat])/simdenom
        }
        if (usegamma) 
            simy[, ncol(simy)] <- exp(g %*% z[iscen, , dim(g)[3]])/simdenom
        else simy[, ncol(simy)] <- 1/simdenom
        simy <- apply(simy, 2, sort)
        ## REVISION ##
        # technique for calculating point estimate (pe) changed from mean to
        # median
        res$pe <- rbind(res$pe, apply(simy, 2, median))
        length.simy <- nrow(simy)
        low <- up <- NULL
        for (k in 1:length(ci)) {
            for (icat in 1:(dim(b)[3] + 1)) {
                res$lower[iscen, icat, k] <- rbind(low, quantile(simy[, 
                                                                      icat], probs = (1 - ci[k])/2))
                res$upper[iscen, icat, k] <- rbind(up, quantile(simy[, 
                                                                     icat], probs = (1 - (1 - ci[k])/2)))
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
                    res$plower[iscen, icat, k] <- rbind(low, quantile(pv[, 
                                                                         icat], probs = (1 - ci[k])/2))
                    res$pupper[iscen, icat, k] <- rbind(up, quantile(pv[, 
                                                                        icat], probs = (1 - (1 - ci[k])/2)))
                }
            }
        }
    }
    res
}

###############################################################################
## END OF SCRIPT
###############################################################################