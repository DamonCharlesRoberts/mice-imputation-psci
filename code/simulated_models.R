# Title: Simulated Models and comparison

# Notes:
    #* Description: R script for models and comparisons between the imputation procedures
    #* Updated: 2022-05-23
    #* Updated by: dcr

# Setup
    #* Set seed
set.seed(717)
    #* Modularly load functions
box::use(
    lme4 = lme4[lmer],
    mice = mice[pool, complete],
    modelsummary = modelsummary[modelsummary],
    ggplot2 = ggplot2[ggplot, geom_density, aes, labs, theme_minimal, ggsave, xlim],
    patchwork = patchwork[...]
)
    #* Source in other scripts
source('code/simulation_data.R')
source('code/introduce_mar.R')
source('code/simulated_imputation.R')


# Models
    #* Full data
        #** Simple DGP Model
            #*** MVN
simple_mvn_full = lm(Y ~ X + Z, data = simple_dgp_mvn)
            #*** NMVN
#simple_nmvn_full = lm(Y ~ X + Z, data = simple_dgp_nmvn)
        #** Moderated DGP Model
            #*** MVN
moderated_mvn_full = lm(Y ~ X + Z + X:Z, data = moderated_dgp_mvn)
            #*** NMVN
#moderated_nmvn_full = lm(Y ~ X + Z + X:Z, data = moderated_dgp_nmvn)
        #** Hierarchical DGP Model
    #* LWD
        #** Simple DGP Model
            #*** MVN
simple_mvn_0.1_lwd = lm(Y ~ X + Z, data = simple_mvn_mar_0.1)
simple_mvn_0.4_lwd = lm(Y ~ X + Z, data = simple_mvn_mar_0.4)
simple_mvn_0.9_lwd = lm(Y ~ X + Z, data = simple_mvn_mar_0.9)
            #*** NMVN
#simple_nmvn_0.1_lwd = lm(Y ~ X + Z, data = simple_nmvn_mar_0.1)
#simple_nmvn_0.4_lwd = lm(Y ~ X + Z, data = simple_nmvn_mar_0.4)
#simple_nmvn_0.9_lwd = lm(Y ~ X + Z, data = simple_nmvn_mar_0.9)
        #** Moderated DGP Model
            #*** MVN
moderated_mvn_0.1_lwd = lm(Y ~ X + Z + X*Z, data = moderated_mvn_mar_0.1)
moderated_mvn_0.4_lwd = lm(Y ~ X + Z + X*Z, data = moderated_mvn_mar_0.4)
moderated_mvn_0.9_lwd = lm(Y ~ X + Z + X*Z, data = moderated_mvn_mar_0.9)
            #*** NMVN
#moderated_nmvn_0.1_lwd = lm(Y ~ X + Z + X*Z, data = moderated_nmvn_mar_0.1)
#moderated_nmvn_0.4_lwd = lm(Y ~ X + Z + X*Z, data = moderated_nmvn_mar_0.4)
#moderated_nmvn_0.9_lwd = lm(Y ~ X + Z + X*Z, data = moderated_nmvn_mar_0.9)
    #* Interpolation
        #** Simple DGP Model
            #*** MVN
simple_mvn_0.1_mean_model = pool(with(simple_mvn_0.1_mean, lm(Y ~ X + Z)))
simple_mvn_0.4_mean_model = pool(with(simple_mvn_0.4_mean, lm(Y ~ X + Z)))
simple_mvn_0.9_mean_model = pool(with(simple_mvn_0.9_mean, lm(Y ~ X + Z)))
simple_mvn_0.4_mean_model_subset = pool(with(simple_mvn_0.4_mean_subset, lm(Y ~ X + Z)))
            #*** NMVN
#simple_nmvn_0.1_mean_model = pool(with(simple_nmvn_0.1_mean, lm(Y ~ X + Z)))
#simple_nmvn_0.4_mean_model = pool(with(simple_nmvn_0.4_mean, lm(Y ~ X + Z)))
#simple_nmvn_0.9_mean_model = pool(with(simple_nmvn_0.9_mean, lm(Y ~ X + Z)))
#simple_nmvn_0.4_mean_model_subset = pool(with(simple_nmvn_0.4_mean_subset, lm(Y ~ X + Z)))
        #** Moderated DGP Model
            #*** MVN
moderated_mvn_0.1_mean_model = pool(with(moderated_mvn_0.1_mean, lm(Y ~ X + Z + X*Z)))
moderated_mvn_0.4_mean_model = pool(with(moderated_mvn_0.4_mean, lm(Y ~ X + Z + X*Z)))
moderated_mvn_0.9_mean_model = pool(with(moderated_mvn_0.9_mean, lm(Y ~ X + Z + X*Z)))
moderated_mvn_0.4_mean_model_subset = pool(with(moderated_mvn_0.4_mean_subset, lm(Y ~ X + Z + X*Z)))
            #*** NMVN
#moderated_nmvn_0.1_mean_model = pool(with(moderated_nmvn_0.1_mean, lm(Y ~ X + Z + X*Z)))
#moderated_nmvn_0.4_mean_model = pool(with(moderated_nmvn_0.4_mean, lm(Y ~ X + Z + X*Z)))
#moderated_nmvn_0.9_mean_model = pool(with(moderated_nmvn_0.9_mean, lm(Y ~ X + Z + X*Z)))
#moderated_nmvn_0.4_mean_model_subset = pool(with(moderated_nmvn_0.4_mean_subset, lm(Y ~ X + Z + X*Z)))

    #* AMELIA
        #** Simple DGP model
            #*** MVN
simple_mvn_0.1_amelia_model = pool(lapply(simple_mvn_0.1_amelia$imputations, function(x) lm(Y ~ X + Z, data = x)))
simple_mvn_0.4_amelia_model = pool(lapply(simple_mvn_0.4_amelia$imputations, function(x) lm(Y ~ X + Z, data = x)))
simple_mvn_0.9_amelia_model = pool(lapply(simple_mvn_0.9_amelia$imputations, function(x) lm(Y ~ X + Z, data = x)))
simple_mvn_0.4_amelia_model_subset = pool(lapply(simple_mvn_0.4_amelia_subset$imputations, function(x) lm(Y ~ X + Z, data = x)))
            #*** NMVN
#simple_nmvn_0.1_amelia_model = pool(lapply(simple_nmvn_0.1_amelia$imputations, function(x) lm(Y ~ X + Z, data = x)))
#simple_nmvn_0.4_amelia_model = pool(lapply(simple_nmvn_0.4_amelia$imputations, function(x) lm(Y ~ X + Z, data = x)))
#simple_nmvn_0.9_amelia_model = pool(lapply(simple_nmvn_0.9_amelia$imputations, function(x) lm(Y ~ X + Z, data = x)))
#simple_nmvn_0.4_amelia_model_subset = pool(lapply(simple_nmvn_0.4_amelia_subset$imputations, function(x) lm(Y ~ X + Z, data = x)))
        #** Moderated DGP Model
            #*** MVN
moderated_mvn_0.1_amelia_model = pool(lapply(moderated_mvn_0.1_amelia$imputations, function(x) lm(Y ~ X + Z + X*Z, data = x)))
moderated_mvn_0.4_amelia_model = pool(lapply(moderated_mvn_0.4_amelia$imputations, function(x) lm(Y ~ X + Z + X*Z, data = x)))
moderated_mvn_0.9_amelia_model = pool(lapply(moderated_mvn_0.9_amelia$imputations, function(x) lm(Y ~ X + Z + X*Z, data = x)))
moderated_mvn_0.4_amelia_model_subset = pool(lapply(moderated_mvn_0.4_amelia_subset$imputations, function(x) lm(Y ~ X + Z + X*Z, data = x)))
            #*** NMVN
#moderated_nmvn_0.1_amelia_model = pool(lapply(moderated_nmvn_0.1_amelia$imputations, function(x) lm(Y ~ X + Z + X*Z, data = x)))
#moderated_nmvn_0.4_amelia_model = pool(lapply(moderated_nmvn_0.4_amelia$imputations, function(x) lm(Y ~ X + Z + X*Z, data = x)))
#moderated_nmvn_0.9_amelia_model = pool(lapply(moderated_nmvn_0.9_amelia$imputations, function(x) lm(Y ~ X + Z + X*Z, data = x)))
#moderated_nmvn_0.4_amelia_model_subset = pool(lapply(moderated_nmvn_0.4_amelia_subset$imputations, function(x) lm(Y ~ X + Z + X*Z, data = x)))

    #* MICE with Bayesian linear model
        #** Simple DGP Models
            #*** MVN
simple_mvn_0.1_bayesian_model = pool(with(simple_mvn_0.1_linear_mice, lm(Y ~ X + Z)))
simple_mvn_0.4_bayesian_model = pool(with(simple_mvn_0.4_linear_mice, lm(Y ~ X + Z)))
simple_mvn_0.9_bayesian_model = pool(with(simple_mvn_0.9_linear_mice, lm(Y ~ X + Z)))
simple_mvn_0.4_bayesian_model_subset = pool(with(simple_mvn_0.4_linear_subset, lm(Y ~ X + Z)))
            #*** NMVN
#simple_nmvn_0.1_bayesian_model = pool(with(simple_nmvn_0.1_linear_mice, lm(Y ~ X + Z)))
#simple_nmvn_0.4_bayesian_model = pool(with(simple_nmvn_0.4_linear_mice, lm(Y ~ X + Z)))
#simple_nmvn_0.9_bayesian_model = pool(with(simple_nmvn_0.9_linear_mice, lm(Y ~ X + Z)))
#simple_nmvn_0.4_bayesian_model_subset = pool(with(simple_nmvn_0.4_linear_subset, lm(Y ~ X + Z)))
        #** Moderated DGP Models
            #*** MVN
moderated_mvn_0.1_bayesian_model = pool(with(moderated_mvn_0.1_linear_mice, lm(Y ~ X + Z + X*Z)))
moderated_mvn_0.4_bayesian_model = pool(with(moderated_mvn_0.4_linear_mice, lm(Y ~ X + Z + X*Z)))
moderated_mvn_0.9_bayesian_model = pool(with(moderated_mvn_0.9_linear_mice, lm(Y ~ X + Z + X*Z)))
moderated_mvn_0.4_bayesian_model_subset = pool(with(moderated_mvn_0.4_linear_subset, lm(Y ~ X + Z + X*Z)))
            #*** NMVN
#moderated_nmvn_0.1_bayesian_model = pool(with(moderated_nmvn_0.1_linear_mice, lm(Y ~ X + Z + X*Z)))
#moderated_nmvn_0.4_bayesian_model = pool(with(moderated_nmvn_0.4_linear_mice, lm(Y ~ X + Z + X*Z)))
#moderated_nmvn_0.9_bayesian_model = pool(with(moderated_nmvn_0.9_linear_mice, lm(Y ~ X + Z + X*Z)))
#moderated_nmvn_0.4_bayesian_model_subset = pool(with(moderated_nmvn_0.4_linear_subset, lm(Y ~ X + Z + X*Z)))
        #** Hierarchical DGP Models

    #* MICE with Random Forests
        #** Simple DGP Models
            #*** MVN
simple_mvn_0.1_rf_model = pool(with(simple_mvn_0.1_rf, lm(Y ~ X + Z)))
simple_mvn_0.4_rf_model = pool(with(simple_mvn_0.4_rf, lm(Y ~ X + Z)))
simple_mvn_0.9_rf_model = pool(with(simple_mvn_0.9_rf, lm(Y ~ X + Z)))
simple_mvn_0.4_rf_model_subset = pool(with(simple_mvn_0.4_rf_subset, lm(Y ~ X + Z)))
            #*** NMVN
#simple_nmvn_0.1_rf_model = pool(with(simple_nmvn_0.1_rf, lm(Y ~ X + Z)))
#simple_nmvn_0.4_rf_model = pool(with(simple_nmvn_0.4_rf, lm(Y ~ X + Z)))
#simple_nmvn_0.9_rf_model = pool(with(simple_nmvn_0.9_rf, lm(Y ~ X + Z)))
#simple_nmvn_0.4_rf_model_subset = pool(with(simple_nmvn_0.4_rf_subset, lm(Y ~ X + Z)))
        #** Moderated DGP Models
            #*** MVN
moderated_mvn_0.1_rf_model = pool(with(moderated_mvn_0.1_rf, lm(Y ~ X + Z + X*Z)))
moderated_mvn_0.4_rf_model = pool(with(moderated_mvn_0.4_rf, lm(Y ~ X + Z + X*Z)))
moderated_mvn_0.9_rf_model = pool(with(moderated_mvn_0.9_rf, lm(Y ~ X + Z + X*Z)))
moderated_mvn_0.4_rf_model_subset = pool(with(moderated_mvn_0.4_rf_subset, lm(Y ~ X + Z + X*Z)))
            #*** NMVN
#moderated_nmvn_0.1_rf_model = pool(with(moderated_nmvn_0.1_rf, lm(Y ~ X + Z + X*Z)))
#moderated_nmvn_0.4_rf_model = pool(with(moderated_nmvn_0.4_rf, lm(Y ~ X + Z + X*Z)))
#moderated_nmvn_0.9_rf_model = pool(with(moderated_nmvn_0.9_rf, lm(Y ~ X + Z + X*Z)))
#moderated_nmvn_0.4_rf_model_subset = pool(with(moderated_nmvn_0.4_rf_subset, lm(Y ~ X + Z + X*Z)))

# Procedure comparisons

# Tables
    #* Regression Results
gm = list(list('raw' = 'nobs', 'clean' = 'N', 'fmt' = 0), list('raw' = 'nimp', 'clean' = 'Imputations', 'fmt' = 0), list('raw' = 'adj.r.squared', 'clean' = 'Adj. $R^2$', 'fmt' = 2))
        #** Simple Models
            #*** MVN
model_mvn_0.1 = list(
    'Full' = simple_mvn_full,
    'LWD' = simple_mvn_0.1_lwd,
    'Means' = simple_mvn_0.1_mean_model,
    'AMELIA' = simple_mvn_0.1_amelia_model,
    'Linear MICE' = simple_mvn_0.1_bayesian_model,
    'Random Forest MICE' = simple_mvn_0.1_rf_model
)
model_mvn_0.1_table = modelsummary(model_mvn_0.1, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Simple MVN DGP with $10\\%$ missing \\label{tab:simple_0.1_mvn}', out = 'figures/simple_mvn_dgp_ols_results_0.1.tex', escape = FALSE)
model_mvn_0.4 = list(
    'Full' = simple_mvn_full,
    'LWD' = simple_mvn_0.4_lwd,
    'Means' = simple_mvn_0.4_mean_model,
    'AMELIA' = simple_mvn_0.4_amelia_model,
    'Linear MICE' = simple_mvn_0.4_bayesian_model,
    'Random Forest MICE' = simple_mvn_0.4_rf_model
)
model_mvn_0.4_table = modelsummary(model_mvn_0.4, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Simple MVN DGP with $40\\%$ missing \\label{tab:simple_0.4_mvn}', out = 'figures/simple_mvn_dgp_ols_results_0.4.tex', escape = FALSE)
model_mvn_0.9 = list(
    'Full' = simple_mvn_full,
    'LWD' = simple_mvn_0.9_lwd,
    'Means' = simple_mvn_0.9_mean_model,
    'AMELIA' = simple_mvn_0.9_amelia_model,
    'Linear MICE' = simple_mvn_0.9_bayesian_model,
    'Random Forest MICE' = simple_mvn_0.9_rf_model
)
model_mvn_0.9_table = modelsummary(model_mvn_0.9, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Simple MVN DGP with $90\\%$ missing \\label{tab:simple_0.9_mvn}', out = 'figures/simple_mvn_dgp_ols_results_0.9.tex', escape = FALSE)
model_mvn_0.4_subset = list(
    'Full' = simple_mvn_full,
    'LWD' = simple_mvn_0.4_lwd,
    'Means' = simple_mvn_0.4_mean_model_subset,
    'AMELIA' = simple_mvn_0.4_amelia_model_subset,
    'Linear MICE' = simple_mvn_0.4_bayesian_model_subset,
    'Random Forest MICE' = simple_mvn_0.4_rf_model_subset
)
model_mvn_0.4_table_subset = modelsummary(model_mvn_0.4_subset, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Simple DGP with $40\\%$ missing and predictors only \\label{tab:simple_0.4_subset_mvn}', out = 'figures/simple_mvn_dgp_ols_results_0.4_subset.tex', escape = FALSE)
            #*** NMVN
#model_nmvn_0.1 = list(
#    'Full' = simple_nmvn_full,
#    'LWD' = simple_nmvn_0.1_lwd,
#    'Means' = simple_nmvn_0.1_mean_model,
#    'AMELIA' = simple_nmvn_0.1_amelia_model,
#    'Linear MICE' = simple_nmvn_0.1_bayesian_model,
#    'Random Forest MICE' = simple_nmvn_0.1_rf_model
#)
#model_nmvn_0.1_table = modelsummary(model_nmvn_0.1, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Simple NNMV DGP with $10\\%$ missing \\label{tab:simple_0.1_nmvn}', out = 'figures/simple_nmvn_dgp_ols_results_0.1.tex', escape = FALSE)
#model_nmvn_0.4 = list(
#    'Full' = simple_nmvn_full,
#    'LWD' = simple_nmvn_0.4_lwd,
#    'Means' = simple_nmvn_0.4_mean_model,
#    'AMELIA' = simple_nmvn_0.4_amelia_model,
#    'Linear MICE' = simple_nmvn_0.4_bayesian_model,
#    'Random Forest MICE' = simple_nmvn_0.4_rf_model
#)
#model_nmvn_0.4_table = modelsummary(model_nmvn_0.4, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Simple NNMV DGP with $40\\%$ missing \\label{tab:simple_0.4_nmvn}', out = 'figures/simple_nmvn_dgp_ols_results_0.4.tex', escape = FALSE)
#model_nmvn_0.9 = list(
#    'Full' = simple_nmvn_full,
#    'LWD' = simple_nmvn_0.9_lwd,
#    'Means' = simple_nmvn_0.9_mean_model,
#    'AMELIA' = simple_nmvn_0.9_amelia_model,
#    'Linear MICE' = simple_nmvn_0.9_bayesian_model,
#    'Random Forest MICE' = simple_nmvn_0.9_rf_model
#)
#model_nmvn_0.9_table = modelsummary(model_nmvn_0.9, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Simple NNMV DGP with $90\\%$ missing \\label{tab:simple_0.9_nmvn}', out = 'figures/simple_nmvn_dgp_ols_results_0.9.tex', escape = FALSE)
#model_nmvn_0.4_subset = list(
#    'Full' = simple_nmvn_full,
#    'LWD' = simple_nmvn_0.4_lwd,
#    'Means' = simple_nmvn_0.4_mean_model_subset,
#    'AMELIA' = simple_nmvn_0.4_amelia_model_subset,
#    'Linear MICE' = simple_nmvn_0.4_bayesian_model_subset,
#    'Random Forest MICE' = simple_nmvn_0.4_rf_model_subset
#)
#model_nmvn_0.4_table_subset = modelsummary(model_nmvn_0.4_subset, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Simple NNMV DGP with $40\\%$ missing and predictors only \\label{tab:simple_0.4_subset_nmvn}', out = 'figures/simple_nmvn_dgp_ols_results_0.4_subset.tex', escape = FALSE)
        #** Moderated Models
            #*** MVN
moderated_mvn_model_0.1 = list(
    'Full' = moderated_mvn_full,
    'LWD' = moderated_mvn_0.1_lwd,
    'Means' = moderated_mvn_0.1_mean_model,
    'AMELIA' = moderated_mvn_0.1_amelia_model,
    'Linear MICE' = moderated_mvn_0.1_bayesian_model,
    'Random Forest MICE' = moderated_mvn_0.1_rf_model
)
moderated_mvn_model_0.1_table = modelsummary(moderated_mvn_model_0.1, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', 'X:Z' = 'X $\\times$ Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Moderated MVN DGP with $10\\%$ missing \\label{tab:moderated_0.1_mvn}', out = 'figures/moderated_mvn_dgp_ols_results_0.1.tex', escape = FALSE)
moderated_mvn_model_0.4 = list(
    'Full' = moderated_mvn_full,
    'LWD' = moderated_mvn_0.4_lwd,
    'Means' = moderated_mvn_0.4_mean_model,
    'AMELIA' = moderated_mvn_0.4_amelia_model,
    'Linear MICE' = moderated_mvn_0.4_bayesian_model,
    'Random Forest MICE' = moderated_mvn_0.4_rf_model
)
moderated_mvn_model_0.4_table = modelsummary(moderated_mvn_model_0.4, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', 'X:Z' = 'X $\\times$ Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Moderated MVN DGP with $40\\%$ missing \\label{tab:moderated_0.4_mvn}', out = 'figures/moderated_mvn_dgp_ols_results_0.4.tex', escape = FALSE)
moderated_mvn_model_0.9 = list(
    'Full' = moderated_mvn_full,
    'LWD' = moderated_mvn_0.9_lwd,
    'Means' = moderated_mvn_0.9_mean_model,
    'AMELIA' = moderated_mvn_0.9_amelia_model,
    'Linear MICE' = moderated_mvn_0.9_bayesian_model,
    'Random Forest MICE' = moderated_mvn_0.9_rf_model
)
moderated_mvn_model_0.9_table = modelsummary(moderated_mvn_model_0.9, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', 'X:Z' = 'X $\\times$ Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Moderated MVN DGP with $90\\%$ missing \\label{tab:moderated_0.9_mvn}', out = 'figures/moderated_mvn_dgp_ols_results_0.9.tex', escape = FALSE)
moderated_mvn_model_0.4_subset = list(
    'Full' = moderated_mvn_full,
    'LWD' = moderated_mvn_0.4_lwd,
    'Means' = moderated_mvn_0.4_mean_model_subset,
    'AMELIA' = moderated_mvn_0.4_amelia_model_subset,
    'Linear MICE' = moderated_mvn_0.4_bayesian_model_subset,
    'Random Forest MICE' = moderated_mvn_0.4_rf_model_subset
)
moderated_mvn_model_0.4_table_subset = modelsummary(moderated_mvn_model_0.4_subset, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', 'X:Z' = 'X $\\times$ Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Moderated MVN DGP with $40\\%$ missing and predictors only \\label{tab:moderated_0.4_subset_mvn}', out = 'figures/moderated_mvn_dgp_ols_results_0.4_subset.tex', escape = FALSE)
            #*** NMVN
#moderated_nmvn_model_0.1 = list(
#    'Full' = moderated_nmvn_full,
#    'LWD' = moderated_nmvn_0.1_lwd,
#    'Means' = moderated_nmvn_0.1_mean_model,
#    'AMELIA' = moderated_nmvn_0.1_amelia_model,
#    'Linear MICE' = moderated_nmvn_0.1_bayesian_model,
#    'Random Forest MICE' = moderated_nmvn_0.1_rf_model
#)
#moderated_nmvn_model_0.1_table = modelsummary(moderated_nmvn_model_0.1, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', 'X:Z' = 'X $\\times$ Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Moderated NNMV DGP with $10\\%$ missing \\label{tab:moderated_0.1_nmvn}', out = 'figures/moderated_dgp_ols_results_0.1.tex', escape = FALSE)
#moderated_nmvn_model_0.4 = list(
#    'Full' = moderated_nmvn_full,
#    'LWD' = moderated_nmvn_0.4_lwd,
#    'Means' = moderated_nmvn_0.4_mean_model,
#    'AMELIA' = moderated_nmvn_0.4_amelia_model,
#    'Linear MICE' = moderated_nmvn_0.4_bayesian_model,
#    'Random Forest MICE' = moderated_nmvn_0.4_rf_model
#)
#moderated_nmvn_model_0.4_table = modelsummary(moderated_nmvn_model_0.4, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', 'X:Z' = 'X $\\times$ Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Moderated NNMV DGP with $40\\%$ missing \\label{tab:moderated_0.4_nmvn}', out = 'figures/moderated_nmvn_dgp_ols_results_0.4.tex', escape = FALSE)
#moderated_nmvn_model_0.9 = list(
#    'Full' = moderated_nmvn_full,
#    'LWD' = moderated_nmvn_0.9_lwd,
#    'Means' = moderated_nmvn_0.9_mean_model,
#    'AMELIA' = moderated_nmvn_0.9_amelia_model,
#    'Linear MICE' = moderated_nmvn_0.9_bayesian_model,
#    'Random Forest MICE' = moderated_nmvn_0.9_rf_model
#)
#moderated_nmvn_model_0.9_table = modelsummary(moderated_nmvn_model_0.9, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', 'X:Z' = 'X $\\times$ Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Moderated NNMV DGP with $90\\%$ missing \\label{tab:moderated_0.9_nmvn}', out = 'figures/moderated_nmvn_dgp_ols_results_0.9.tex', escape = FALSE)
#moderated_nmvn_model_0.4_subset = list(
#    'Full' = moderated_nmvn_full,
#    'LWD' = moderated_nmvn_0.4_lwd,
#    'Means' = moderated_nmvn_0.4_mean_model_subset,
#    'AMELIA' = moderated_nmvn_0.4_amelia_model_subset,
#    'Linear MICE' = moderated_nmvn_0.4_bayesian_model_subset,
#    'Random Forest MICE' = moderated_nmvn_0.4_rf_model_subset
#)
#moderated_nmvn_model_0.4_table_subset = modelsummary(moderated_nmvn_model_0.4_subset, gof_map = gm, coef_map = c('X' = 'X', 'Z' = 'Z', 'X:Z' = 'X $\\times$ Z', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), notes = c('Data Source: Simulated data.','Estimates from OLS Regression.', 'Standard errors in parentheses.'), title = 'Regression Results from Moderated NNMV DGP with $40\\%$ missing and predictors only \\label{tab:moderated_0.4_subset_nmvn}', out = 'figures/moderated_nmvn_dgp_ols_results_0.4_subset.tex', escape = FALSE)


# Graphs
    #* Distributions - Y
        #** Mean
simple_0.4_dist_mean = ggplot() +
    geom_density(data = as.data.frame(simple_mvn_mar_0.4), aes(x = Y), color = 'black', linetype = 2) +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean)), aes(x = Y), color = '#404040') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 2)), aes(x = Y), color = '#505050') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 3)), aes(x = Y), color = '#606060') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 4)), aes(x = Y), color = '#696969') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 5)), aes(x = Y), color = '#787878') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 6)), aes(x= Y),
    color = '#888888') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 7)), aes(x = Y), color = '#989898') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 8)), aes(x = Y), color = '#A8A8A8') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 9)), aes(x = Y), color = '#B0B0B0') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 10)), aes(x = Y), color = '#BEBEBE') +
    geom_density(data = simple_dgp_mvn, aes(x = Y), color = 'black') +
    theme_minimal() + 
    labs(x = 'Y Variable', y = 'Density')
        #** AMELIA
simple_0.4_dist_amelia = ggplot() +
    geom_density(data = as.data.frame(simple_mvn_mar_0.4), aes(x = Y), color = 'black', linetype = 2) +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp1), aes(x = Y), color = '#404040') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp2), aes(x = Y), color = '#505050') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp3), aes(x = Y), color = '#606060') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp4), aes(x = Y), color = '#696969') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp5), aes(x = Y), color = '#787878') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp6), aes(x= Y),
    color = '#888888') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp7), aes(x = Y), color = '#989898') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp8), aes(x = Y), color = '#A8A8A8') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp9), aes(x = Y), color = '#B0B0B0') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp10), aes(x = Y), color = '#BEBEBE') +
    geom_density(data = simple_dgp_mvn, aes(x = Y), color = 'black') +
    theme_minimal() + 
    labs(x = 'Y Variable', y = 'Density')
        #** Linear MICE
simple_0.4_dist_linear = ggplot() +
    geom_density(data = as.data.frame(simple_mvn_mar_0.4), aes(x = Y), color = 'black', linetype = 2) +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice)), aes(x = Y), color = '#404040') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 2)), aes(x = Y), color = '#505050') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 3)), aes(x = Y), color = '#606060') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 4)), aes(x = Y), color = '#696969') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 5)), aes(x = Y), color = '#787878') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 6)), aes(x= Y),
    color = '#888888') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 7)), aes(x = Y), color = '#989898') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 8)), aes(x = Y), color = '#A8A8A8') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 9)), aes(x = Y), color = '#B0B0B0') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 10)), aes(x = Y), color = '#BEBEBE') +
    geom_density(data = simple_dgp_mvn, aes(x = Y), color = 'black') +
    theme_minimal() + 
    labs(x = 'Y Variable', y = 'Density')
        #** Random Forest MICE
simple_0.4_dist_rf = ggplot() +
    geom_density(data = as.data.frame(simple_mvn_mar_0.4), aes(x = Y), color = 'black', linetype = 2) +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf)), aes(x = Y), color = '#404040') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 2)), aes(x = Y), color = '#505050') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 3)), aes(x = Y), color = '#606060') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 4)), aes(x = Y), color = '#696969') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 5)), aes(x = Y), color = '#787878') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 6)), aes(x= Y),
    color = '#888888') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 7)), aes(x = Y), color = '#989898') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 8)), aes(x = Y), color = '#A8A8A8') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 9)), aes(x = Y), color = '#B0B0B0') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 10)), aes(x = Y), color = '#BEBEBE') +
    geom_density(data = simple_dgp_mvn, aes(x = Y), color = 'black') +
    theme_minimal() + 
    labs(x = 'Y Variable', y = 'Density')
simple_0.4_combined_dist = (simple_0.4_dist_mean + simple_0.4_dist_amelia) / (simple_0.4_dist_linear + simple_0.4_dist_rf) + plot_annotation(caption = 'Data Source: Simulated Simple DGP with 40% missing.', tag_levels = 'A')
ggsave(simple_0.4_combined_dist, file = 'figures/simple_dgp_0.4_dists.png', dpi = 300)

    #* Distributions - X
        #** Mean
simple_0.4_dist_mean_x = ggplot() +
    geom_density(data = as.data.frame(simple_dgp_mvn), aes(x = X), color = 'black', linetype = 2) +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean)), aes(x = X), color = '#404040') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 2)), aes(x = X), color = '#505050') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 3)), aes(x = X), color = '#606060') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 4)), aes(x = X), color = '#696969') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 5)), aes(x = X), color = '#787878') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 6)), aes(x = X),
    color = '#888888') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 7)), aes(x = X), color = '#989898') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 8)), aes(x = X), color = '#A8A8A8') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 9)), aes(x = X), color = '#B0B0B0') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 10)), aes(x = X), color = '#BEBEBE') +
    geom_density(data = simple_dgp_mvn, aes(x = X), color = 'black') +
    theme_minimal() + 
    labs(x = 'X Variable', y = 'Density')
        #** AMELIA
simple_0.4_dist_amelia_x = ggplot() +
    geom_density(data = as.data.frame(simple_dgp_mvn), aes(x = X), color = 'black', linetype = 2) +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp1), aes(x = X), color = '#404040') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp2), aes(x = X), color = '#505050') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp3), aes(x = X), color = '#606060') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp4), aes(x = X), color = '#696969') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp5), aes(x = X), color = '#787878') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp6), aes(x = X),
    color = '#888888') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp7), aes(x = X), color = '#989898') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp8), aes(x = X), color = '#A8A8A8') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp9), aes(x = X), color = '#B0B0B0') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp10), aes(x = X), color = '#BEBEBE') +
    geom_density(data = simple_dgp_mvn, aes(x = X), color = 'black') +
    theme_minimal() + 
    labs(x = 'X Variable', y = 'Density')
        #** Linear MICE
simple_0.4_dist_linear_x = ggplot() +
    geom_density(data = as.data.frame(simple_dgp_mvn), aes(x = X), color = 'black', linetype = 2) +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice)), aes(x = X), color = '#404040') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 2)), aes(x = X), color = '#505050') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 3)), aes(x = X), color = '#606060') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 4)), aes(x = X), color = '#696969') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 5)), aes(x = X), color = '#787878') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 6)), aes(x = X),
    color = '#888888') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 7)), aes(x = X), color = '#989898') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 8)), aes(x = X), color = '#A8A8A8') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 9)), aes(x = X), color = '#B0B0B0') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 10)), aes(x = X), color = '#BEBEBE') +
    geom_density(data = simple_dgp_mvn, aes(x = X), color = 'black') +
    theme_minimal() + 
    labs(x = 'X Variable', y = 'Density')
        #** Random Forest MICE
simple_0.4_dist_rf_x = ggplot() +
    geom_density(data = as.data.frame(simple_dgp_mvn), aes(x = X), color = 'black', linetype = 2) +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf)), aes(x = X), color = '#404040') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 2)), aes(x = X), color = '#505050') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 3)), aes(x = X), color = '#606060') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 4)), aes(x = X), color = '#696969') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 5)), aes(x = X), color = '#787878') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 6)), aes(x = X),
    color = '#888888') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 7)), aes(x = X), color = '#989898') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 8)), aes(x = X), color = '#A8A8A8') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 9)), aes(x = X), color = '#B0B0B0') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 10)), aes(x = X), color = '#BEBEBE') +
    geom_density(data = simple_dgp_mvn, aes(x = X), color = 'black') +
    theme_minimal() + 
    labs(x = 'X Variable', y = 'Density')
simple_0.4_combined_dist_x = (simple_0.4_dist_mean_x + simple_0.4_dist_amelia_x) / (simple_0.4_dist_linear_x + simple_0.4_dist_rf_x) + plot_annotation(caption = 'Data Source: Simulated Simple DGP with 40% missing.\n Black line represents distribution of complete vector.', tag_levels = 'A')
ggsave(simple_0.4_combined_dist_x, file = 'figures/simple_dgp_0.4_dists_x-var.png', dpi = 300)


    #* Distributions - Z
        #** Mean
simple_0.4_dist_mean_z = ggplot() +
    geom_density(data = as.data.frame(simple_dgp_mvn), aes(x = Z), color = 'black', linetype = 2) +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean)), aes(x = Z), color = '#404040') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 2)), aes(x = Z), color = '#505050') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 3)), aes(x = Z), color = '#606060') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 4)), aes(x = Z), color = '#696969') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 5)), aes(x = Z), color = '#787878') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 6)), aes(x = Z),
    color = '#888888') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 7)), aes(x = Z), color = '#989898') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 8)), aes(x = Z), color = '#A8A8A8') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 9)), aes(x = Z), color = '#B0B0B0') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_mean, 10)), aes(x = Z), color = '#BEBEBE') +
    geom_density(data = simple_dgp_mvn, aes(x = Z), color = 'black') +
    theme_minimal() + 
    labs(x = 'Z Variable', y = 'Density')
        #** AMELIA
simple_0.4_dist_amelia_z = ggplot() +
    geom_density(data = as.data.frame(simple_dgp_mvn), aes(x = Z), color = 'black', linetype = 2) +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp1), aes(x = Z), color = '#404040') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp2), aes(x = Z), color = '#505050') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp3), aes(x = Z), color = '#606060') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp4), aes(x = Z), color = '#696969') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp5), aes(x = Z), color = '#787878') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp6), aes(x = Z),
    color = '#888888') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp7), aes(x = Z), color = '#989898') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp8), aes(x = Z), color = '#A8A8A8') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp9), aes(x = Z), color = '#B0B0B0') +
    geom_density(data = as.data.frame(simple_mvn_0.4_amelia$imputations$imp10), aes(x = Z), color = '#BEBEBE') +
    geom_density(data = simple_dgp_mvn, aes(x = Z), color = 'black') +
    theme_minimal() + 
    labs(x = 'Z Variable', y = 'Density')
        #** Linear MICE
simple_0.4_dist_linear_z = ggplot() +
    geom_density(data = as.data.frame(simple_dgp_mvn), aes(x = Z), color = 'black', linetype = 2) +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice)), aes(x = Z), color = '#404040') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 2)), aes(x = Z), color = '#505050') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 3)), aes(x = Z), color = '#606060') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 4)), aes(x = Z), color = '#696969') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 5)), aes(x = Z), color = '#787878') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 6)), aes(x = Z),
    color = '#888888') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 7)), aes(x = Z), color = '#989898') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 8)), aes(x = Z), color = '#A8A8A8') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 9)), aes(x = Z), color = '#B0B0B0') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_linear_mice, 10)), aes(x = Z), color = '#BEBEBE') +
    geom_density(data = simple_dgp_mvn, aes(x = Z), color = 'black') +
    theme_minimal() + 
    labs(x = 'Z Variable', y = 'Density')
        #** Random Forest MICE
simple_0.4_dist_rf_z = ggplot() +
    geom_density(data = as.data.frame(simple_dgp_mvn), aes(x = Z), color = 'black', linetype = 2) +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf)), aes(x = Z), color = '#404040') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 2)), aes(x = Z), color = '#505050') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 3)), aes(x = Z), color = '#606060') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 4)), aes(x = Z), color = '#696969') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 5)), aes(x = Z), color = '#787878') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 6)), aes(x = Z),
    color = '#888888') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 7)), aes(x = Z), color = '#989898') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 8)), aes(x = Z), color = '#A8A8A8') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 9)), aes(x = Z), color = '#B0B0B0') +
    geom_density(data = as.data.frame(complete(simple_mvn_0.4_rf, 10)), aes(x = Z), color = '#BEBEBE') +
    geom_density(data = simple_dgp_mvn, aes(x = Z), color = 'black') +
    theme_minimal() + 
    labs(x = 'Z Variable', y = 'Density')
simple_0.4_combined_dist_z = (simple_0.4_dist_mean_z + simple_0.4_dist_amelia_z) / (simple_0.4_dist_linear_z + simple_0.4_dist_rf_z) + plot_annotation(caption = 'Data Source: Simulated Simple DGP with 40% missing.\n Black line represents distribution of complete vector.', tag_levels = 'A')
ggsave(simple_0.4_combined_dist_z, file = 'figures/simple_dgp_0.4_dists_z-var.png', dpi = 300)

# Code that didn't work
    #* Source
##source('code/amelia_transformer.R')
    #* Models
        #** AMELIA
            #** Simple Data Generating process
##simple_0.1_amelia_model = amelia_transformer(imputed = simple_0.1_amelia, specification = Y~X+Z)
##simple_0.4_amelia_model = amelia_transformer(imputed = simple_0.4_amelia, specification = Y~X+Z)
##simple_0.9_amelia_model = amelia_transformer(imputed = simple_0.9_amelia, specification = Y~X+Z)
##simple_0.4_subset_amelia_model = amelia_transformer(imputed = simple_0.4_amelia_subset, specification = Y~X+Z)
##simple_0.1_amelia_model = lapply(simple_0.1_amelia$imputations, function(i) lm(Y ~ X + Z, data = i))
##simple_0.1_betas = do.call(rbind, lapply(simple_0.1_amelia_model, function(i) coef(summary(i))[,1]))
##simple_0.1_std_errors = do.call(rbind, lapply(simple_0.1_amelia_model, function(i) coef(summary(i))[,2]))
##simple_0.1_combined = mi.meld(simple_0.1_betas, simple_0.1_std_errors)
##simple_0.1_combined = do.call(rbind.data.frame, simple_0.1_combined) |>
##    rownames_to_column() |> 
##    gather(variable, value, -rowname) |>
##    spread(rowname, value) |>
##    rename(term = variable, estimate = q.mi, std.error = se.mi)
##simple_0.1_combined <- list(
##    tidy = simple_0.1_combined,
##    glance = NULL
##)
##class(simple_0.1_combined) = 'modelsummary_list'
##
##simple_0.4_amelia_model = lapply(simple_0.4_amelia$imputations, function(i) lm(Y ~ X + Z, data = i))
##simple_0.4_betas = do.call(rbind, lapply(simple_0.1_amelia_model, function(i) coef(summary(i))[,1]))
##simple_0.4_std_errors = do.call(rbind, lapply(simple_0.4_amelia_model, function(i) coef(summary(i))[,2]))
##simple_0.4_combined = mi.meld(simple_0.4_betas, simple_0.4_std_errors)
##
##simple_0.9_amelia_model = lapply(simple_0.9_amelia$imputations, function(i) lm(Y ~ X + Z, data = i))
##simple_0.9_betas = do.call(rbind, lapply(simple_0.9_amelia_model, function(i) coef(summary(i))[,1]))
##simple_0.9_std_errors = do.call(rbind, lapply(simple_0.9_amelia_model, function(i) coef(summary(i))[,2]))
##simple_0.9_combined = mi.meld(simple_0.9_betas, simple_0.9_std_errors)
##
##simple_0.4_amelia_subset_model = lapply(simple_0.4_amelia_subset$imputations, function(i) lm(Y ~ X + Z, data = i))
##simple_0.4_amelia_subset_betas = do.call(rbind, lapply(simple_0.4_amelia_subset_model, function(i) coef(summary(i))[,1]))
##simple_0.4_amelia_subset_std_errors = do.call(rbind, lapply(simple_0.4_amelia_subset_model, function(i) coef(summary(i))[,2]))
##simple_0.4_amelia_subset_combined = mi.meld(simple_0.4_amelia_subset_betas, simple_0.4_amelia_subset_std_errors)
##          #** Moderated DGP model
##moderated_0.1_amelia_model = amelia_transformer(imputed = moderated_0.1_amelia, specification = Y~X+Z+X*Z)
##moderated_0.4_amelia_model = amelia_transformer(imputed = moderated_0.1_amelia, specification = Y~X+Z+X*Z)
##moderated_0.9_amelia_model = amelia_transformer(imputed = moderated_0.9_amelia, specification = Y~X+Z+X*Z)
##moderated_0.4_amelia_subset_mdoel = amelia_transformer(imputed = moderated_0.4_amelia_subset, specification = Y~X+Z+X*Z)
##moderated_0.1_amelia_model = lapply(moderated_0.1_amelia$imputations, function(i) lm(Y ~ X + Z + X * Z, data = i))
##moderated_0.1_betas = do.call(rbind, lapply(moderated_0.1_amelia_model, function(i) coef(summary(i))[,1]))
##moderated_0.1_std_errors = do.call(rbind, lapply(moderated_0.1_amelia_model, function(i) coef(summary(i))[,2]))
##moderated_0.1_combined = mi.meld(moderated_0.1_betas, moderated_0.1_std_errors)
##
##moderated_0.4_amelia_model = lapply(moderated_0.4_amelia$imputations, function(i) lm(Y ~ X + Z + X * Z, data = i))
##moderated_0.4_betas = do.call(rbind, lapply(moderated_0.4_amelia_model, function(i) coef(summary(i))[,1]))
##moderated_0.4_std_errors = do.call(rbind, lapply(moderated_0.4_amelia_model, function(i) coef(summary(i))[,2]))
##moderated_0.4_combined = mi.meld(moderated_0.4_betas, moderated_0.4_std_errors)
##
##moderated_0.9_amelia_model = lapply(moderated_0.9_amelia$imputations, function(i) lm(Y ~ X + Z + X * Z, data = i))
##moderated_0.9_betas = do.call(rbind, lapply(moderated_0.9_amelia_model, function(i) coef(summary(i))[,1]))
##moderated_0.9_std_errors = do.call(rbind, lapply(moderated_0.9_amelia_model, function(i) coef(summary(i))[,2]))
##moderated_0.9_combined = mi.meld(moderated_0.9_betas, moderated_0.9_std_errors)
##
##moderated_0.4_amelia_subset_model = lapply(moderated_0.4_amelia_subset$imputations, function(i) lm(Y ~ X + Z + X*Z, data = i))
##moderated_0.4_amelia_subset_betas = do.call(rbind, lapply(moderated_0.4_amelia_subset_model, function(i) coef(summary(i))[,1]))
##moderated_0.4_amelia_subset_std_errors = do.call(rbind, lapply(moderated_0.4_amelia_subset_model, function(i) coef(summary(i))[,2]))
##moderated_0.4_subset_combined = mi.meld(moderated_0.4_amelia_subset_betas, moderated_0.4_amelia_subset_std_errors)
##          #** Hierarchical DGP Model
##hierarchical_0.1_amelia_model = lapply(hierarchical_0.1_amelia$imputations, function(i) lmer(Y ~ Z + (1 | X), data = i))
##