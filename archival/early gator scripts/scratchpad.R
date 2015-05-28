selected_model <- add_logit
model_data <- model.frame(selected_model)

pe <- get_point_estimates(selected_model)
cvm <- get_covariance_matrix(selected_model)
coeffs <- get_coefficient_estimates(1000, pe, cvm, selected_model)
nd <- get_new_data(model_data, selected_model, "age")
predictions <- mlogitsimev(nd, coeffs)

# get factor and levels from original dataset
var_levels <- with(base_data, levels(get(facet_variable)))
# get all combinations of the factor name combined with the level name (in 
# the order that the levels are set)
factor_var_combinations <- paste0(facet_variable, var_levels)
# expand the counterfactual set to include appropriate combinations of the
# factor/level columns - all having range(0, 1, 1)

## BRIAN: You are here. You are exploring how to expand the counterfactual
## set to include all combinations of the factor levels. Then you want
## to drop the reference level.

x_axis_cuts <- 1:100

counterfactuals <- data.frame(x_axis_cuts, facet_var = 
                                  rep(factor_var_combinations, 
                                      each = length(x_axis_cuts))
                              )

counterfactuals <- spread(counterfactuals, facet_var, x_axis_cuts)

counterfactuals <- ifelse(is.na(counterfactuals), 0, 1)

counterfactuals <- counterfactuals[, -1]

counterfactuals <- data.frame(x_axis_cuts = rep(x_axis_cuts), counterfactuals)
