# Title: ANES imputation

# Notes:
    #* Description: R Script to perform Imputation procedures for ANES dataset
    #* Updated: 2022-06-27
    #* Updated by: dcr

# Setup
    #* Set seed
set.seed(717)
    #* Load packages
box::use(
    Amelia = Amelia[amelia],
    mice = mice[mice]
)
    #* Run anes_cleaning.py first
anes = read.csv('data/anes_2020_clean.csv')

# AMELIA imputation
anes_imp = amelia(anes, m = 10, p2s = 0, parallel = 'multicore')

# RF-MICE imputation
anes_rf_mice_imp = mice(anes, m = 10, method = 'rf')

# Save
save(anes_imp, anes_rf_mice_imp, file = 'data/anes_imputation_env.RData')