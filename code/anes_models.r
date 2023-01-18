# Title: ANES models 

# Notes:
    #* Description: R script for ANES models
    #* Updated: 2022-06-28
    #* Updated by: dcr

# Setup
    #* Set seed
set.seed(717)
    #* Modularly load functions
box::use(
    dplyr = dplyr[mutate, case_when, rename, select],
    magrittr = magrittr[...],
    mice = mice[pool, complete, as.mids, md.pattern],
    modelsummary = modelsummary[modelsummary],
    finalfit = finalfit[missing_pairs, missing_compare],
    ggplot2 = ggplot2[labs, ggsave],
    kableExtra = kableExtra[kbl, save_kable],
    broom = broom[tidy]
)
    #* Load data
        #** Imputed data
load('data/anes_imputation_env.RData')

anes_amelia = anes_imp

anes_rf = anes_rf_mice_imp
        #** Clean ANES data w/ missingness
anes_raw = read.csv('data/anes_2020_clean.csv')
    #* Recode variables
anes_amelia$imputations = lapply(anes_amelia$imputations, function(i){
    i['white'] = as.numeric(ifelse(i['V201549x'] == 1, 1, 0))
    i['female'] = as.numeric(ifelse(i['V201600'] == 2, 1, 0))
    i['pid'] = as.numeric(ifelse(i['V202065x'] == 1, 1, ifelse(i['V202065x'] > 3, 2, 3)))
    i['dem_vote'] = as.numeric(ifelse(i['V202105x'] == 10, 1, 0))
    return(i)
})
anes_amelia$imputations = anes_amelia$imputations[names(anes_amelia$imputations) %in% c('imp4') == FALSE] 

anes_rf_long = complete(anes_rf, action = 'long', include = TRUE) %>%
    mutate(white = ifelse(V201549x == 1, 1, 0),
            female = ifelse(V201600 == 2, 1, 0),
            pid = ifelse(V202065x == 1, 1, ifelse(V202065x > 3, 2, ifelse(V202065x == 2, 3, NA))),
            dem_vote = ifelse(V202105x == 10, 1, 0)) %>%
    as.mids()
anes = anes_raw %>%
    mutate(white = ifelse(V201549x == 1, 1, 0),
            female = ifelse(V201600 == 2, 1, 0),
            pid = ifelse(V202065x == 1, 1, ifelse(V202065x > 3, 2, ifelse(V202065x == 2, 3, NA))),
            dem_vote = ifelse(V202105x == 10, 1, 0))

# Models
    #* LWD
lwd = lm(dem_vote ~ pid + V201511x + V201507x + V201533x + white + female, data = anes)
    #* AMELIA
amelia = pool(lapply(anes_amelia$imputations, function(x) lm(dem_vote ~ pid + V201511x + V201507x + V201533x + white + female, data = x)))
    #* RF-MICE
rf = pool(with(anes_rf_long, lm(dem_vote ~ pid + V201511x + V201507x + V201533x + white + female)))
    #* T-Test comparing point estimates
lwd_df = tidy(lwd)
amelia_df = tidy(amelia)
rf_df = tidy(rf)

tidy_t_test = function(x,y){
    tidied_df = list()
    for(i in 1:nrow(x)){
        beta_diff = x$estimate - y$estimate 
        se_x = (x$std.error)^2
        se_y = (y$std.error)^2
        denom = sqrt(se_x + se_y)
        t = beta_diff/denom
        tidied_df = append(tidied_df, t)
        names(tidied_df) <- as.character(x$term)
        return(tidied_df)
    }
}

lwd_vs_amelia = tidy_t_test(lwd_df, amelia_df)
lwd_vs_rf = tidy_t_test(lwd_df, rf_df)
amelia_vs_rf = tidy_t_test(amelia_df, rf_df)


# Tables
    #* Missing data patterns
anes = anes %>%
    mutate(`Democratic`= factor(dem_vote, labels = c('No', 'Yes')),
            `PID` = factor(pid, labels = c('Dem', 'Ind', 'Rep')),
            `White` = factor(white, labels = c('N-W', 'White')),
            `Female` = factor(female, labels = c('Male', 'Female')),
            `Education` = factor(V201511x),
            `Age` = V201507x,
            `Employment Status` = V201533x)
explanatory = c('PID', 'Age', 'Education', 'White', 'Female')
dependent = 'Democratic'
missing_pattern_table = anes %>% 
    missing_compare(dependent, explanatory)

missing_pattern_table = kbl(missing_pattern_table, row.names = FALSE, align = c('l', 'l', 'r', 'r', 'r'), booktabs = TRUE, caption = 'Pattern of missing data', label = 'anes_missing_pattern_table', format = 'latex')

save_kable(missing_pattern_table, file = 'figures/anes_missing_pattern_table.tex')
    #* Model
mods = list(
    'Listwise Deletion' = lwd,
    'AMELIA' = amelia,
    'RF-MICE' = rf
)
cm = c(
    'pid' = 'PID',
    'white' = 'White',
    'female' = 'Female',
    'V201511x' = 'Education',
    'V201507x' = 'Age',
    'V201533x' = 'Employed',
    '(Intercept)' = 'Constant'
)
gm = list(
    list('raw' = 'nobs', 'clean' = 'N', fmt = 0),
    list('raw' = 'nimp', 'clean' = 'Imputations', fmt = 0),
    list('raw' = 'adj.r.squared', 'clean' = 'Adj. R$^2$', fmt = 2)
)
modelsummary(mods, coef_map = cm, gof_map = gm, title = '2020 Democratic Vote Share \\label{tab:anes_regression_table}', notes = list('Data Source: 2020 American National Election Study.', 'Estimates from OLS.', 'Standard Errors in Parentheses.'), stars = c('*' = 0.05), out = 'figures/anes_regression_table.tex')
    #* T-tests

        #** LWD VS AMELIA
lwd_vs_amelia_ti <- data.frame(
    term = names(lwd_vs_amelia),
    estimate = c(as.numeric(lwd_vs_amelia[[1]]), as.numeric(lwd_vs_amelia[[2]]), as.numeric(lwd_vs_amelia[[3]]), as.numeric(lwd_vs_amelia[[4]]), as.numeric(lwd_vs_amelia[[5]]), as.numeric(lwd_vs_amelia[[6]]), as.numeric(lwd_vs_amelia[[7]])))
lwd_vs_amelia_gl = data.frame(
)
lwd_vs_amelia_mod = list(
    tidy = lwd_vs_amelia_ti,
    glance = lwd_vs_amelia_gl
)
class(lwd_vs_amelia_mod) = 'modelsummary_list'
modelsummary(lwd_vs_amelia_mod, coef_map = cm, notes = list('Two-way T-Test of point estimates.'), statistic = NULL, out = 'figures/lwd_vs_amelia_t-test.tex', title = 'Listwise Deletion Versus Amelia \\label{tab:lwd_vs_amelia}')
        #** LWD VS RF
lwd_vs_rf_ti <- data.frame(
    term = names(lwd_vs_rf),
    estimate = c(as.numeric(lwd_vs_rf[[1]]), as.numeric(lwd_vs_rf[[2]]), as.numeric(lwd_vs_rf[[3]]), as.numeric(lwd_vs_rf[[4]]), as.numeric(lwd_vs_rf[[5]]), as.numeric(lwd_vs_rf[[6]]), as.numeric(lwd_vs_rf[[7]])))
lwd_vs_rf_gl = data.frame(
)
lwd_vs_rf_mod = list(
    tidy = lwd_vs_rf_ti,
    glance = lwd_vs_rf_gl
)
class(lwd_vs_rf_mod) = 'modelsummary_list'
modelsummary(lwd_vs_rf_mod, coef_map = cm, statistic = NULL, notes = list('Two-way T-Test of point estimates.'), out = 'figures/lwd_vs_rf_t-test.tex', title = 'Listwise Deletion Versus RF-MICE \\label{tab:lwd_vs_rf}')
        #** AMELIA VS RF
amelia_vs_rf_ti <- data.frame(
    term = names(amelia_vs_rf),
    estimate = c(as.numeric(amelia_vs_rf[[1]]), as.numeric(amelia_vs_rf[[2]]), as.numeric(amelia_vs_rf[[3]]), as.numeric(amelia_vs_rf[[4]]), as.numeric(amelia_vs_rf[[5]]), as.numeric(amelia_vs_rf[[6]]), as.numeric(amelia_vs_rf[[7]]))
)
amelia_vs_rf_gl = data.frame(

)
amelia_vs_rf_mod = list(
    tidy = amelia_vs_rf_ti,
    glance = amelia_vs_rf_gl
)
class(amelia_vs_rf_mod) = 'modelsummary_list'
modelsummary(amelia_vs_rf_mod, coef_map = cm, statistic = NULL, notes = list('Two-way T-Test of point estimates.'), title = 'AMELIA Versus RF-MICE \\label{tab:amelia_vs_rf}', out = 'figures/amelia_vs_rf_t-test.tex')
# Graphs
    #* Missing data pattern
missing_pattern_plot = anes %>% missing_pairs(dependent, explanatory, position = 'fill') + labs(Title = 'Missing Data Patterns', caption = 'Data Source: 2020 American National Election Study')
ggsave(missing_pattern_plot, file = 'figures/anes_missing_pattern_plot.jpeg', width = 7, units = 'in')