# Title: Imputation with simulations

# Notes:
    #* Description: R script for imputing simulated datasets with MAR
    #* Updated: 2022-05-23
    #* Updated by: dcr

# Setup
    #* Set seed
set.seed(717)
    #* Modularly load packages and functions
box::use(
    dplyr = dplyr[select, rename],
    Amelia = Amelia[amelia],
    mice = mice[mice]
)
    #* Make low dimension subsets of datasets
        #** MVN
simple_mvn_mar_0.4_subset = simple_mvn_mar_0.4 |>
    select(X, Z, Y)
moderated_mvn_mar_0.4_subset = moderated_mvn_mar_0.4 |>
    select(X, Z, Y)
        #** NMVN
#simple_nmvn_mar_0.4_subset = simple_nmvn_mar_0.4 |>
#    select(X, Z, Y)
#moderated_nmvn_mar_0.4_subset = moderated_nmvn_mar_0.4 |>
#    select(X, Z, Y)
#hierarchical_nmvn_mar_0.4_subset = hierarchical_nmvn_mar_0.4 |>
#    select(X, Z, Y)

# Imputation
    #* LWD
## NA
    #* Mean imputation
{
        #** Simple DGP imputation
            #*** MVN
    simple_mvn_0.1_mean = mice(simple_mvn_mar_0.1, m = 10, meth = 'mean')
    simple_mvn_0.4_mean = mice(simple_mvn_mar_0.4, m = 10, meth = 'mean')
    simple_mvn_0.9_mean = mice(simple_mvn_mar_0.9, m = 10, meth = 'mean')
    simple_mvn_0.4_mean_subset = mice(simple_mvn_mar_0.4_subset, m = 10, meth = 'mean')
            #*** NMVN
    #simple_nmvn_0.1_mean = mice(simple_nmvn_mar_0.1, m = 10, meth = 'mean')
    #simple_nmvn_0.4_mean = mice(simple_nmvn_mar_0.4, m = 10, meth = 'mean')
    #simple_nmvn_0.9_mean = mice(simple_nmvn_mar_0.9, m = 10, meth = 'mean')
    #simple_nmvn_0.4_mean_subset = mice(simple_nmvn_mar_0.4_subset, m = 10, meth = 'mean')
        #** Moderated DGP imputation
            #*** MVN
    moderated_mvn_0.1_mean = mice(moderated_mvn_mar_0.1, m = 10, meth = 'mean')
    moderated_mvn_0.4_mean = mice(moderated_mvn_mar_0.4, m = 10, meth = 'mean')
    moderated_mvn_0.9_mean = mice(moderated_mvn_mar_0.9, m = 10, meth = 'mean')
    moderated_mvn_0.4_mean_subset = mice(moderated_mvn_mar_0.4_subset, m = 10, meth = 'mean')
            #*** NMVN
    #moderated_nmvn_0.1_mean = mice(moderated_nmvn_mar_0.1, m = 10, meth = 'mean')
    #moderated_nmvn_0.4_mean = mice(moderated_nmvn_mar_0.4, m = 10, meth = 'mean')
    #moderated_nmvn_0.9_mean = mice(moderated_nmvn_mar_0.9, m = 10, meth = 'mean')
    #moderated_nmvn_0.4_mean_subset = mice(moderated_nmvn_mar_0.4_subset, m = 10, meth = 'mean')
}
    #* AMELIA
{
            #** Simple DGP imputation
                #*** MVN
    simple_mvn_0.1_amelia = amelia(simple_mvn_mar_0.1, m = 10)
    simple_mvn_0.4_amelia = amelia(simple_mvn_mar_0.4, m = 10)
    simple_mvn_0.9_amelia = amelia(simple_mvn_mar_0.9, m = 10)
    simple_mvn_0.4_amelia_subset = amelia(simple_mvn_mar_0.4_subset, m = 10)
                #*** NMVN
    #simple_nmvn_0.1_amelia = amelia(simple_nmvn_mar_0.1, m = 10)
    #simple_nmvn_0.4_amelia = amelia(simple_nmvn_mar_0.4, m = 10)
    #simple_nmvn_0.9_amelia = amelia(simple_nmvn_mar_0.9, m = 10)
    #simple_nmvn_0.4_amelia_subset = amelia(simple_nmvn_mar_0.4_subset, m = 10)
            #** Modated DGP imputation
                #*** MVN
    moderated_mvn_0.1_amelia = amelia(moderated_mvn_mar_0.1, m = 10)
    moderated_mvn_0.4_amelia = amelia(moderated_mvn_mar_0.4, m = 10)
    moderated_mvn_0.9_amelia = amelia(moderated_mvn_mar_0.9, m = 10)
    moderated_mvn_0.4_amelia_subset = amelia(moderated_mvn_mar_0.4_subset, m = 10)
                #*** NMVN
    #moderated_nmvn_0.1_amelia = amelia(moderated_nmvn_mar_0.1, m = 10)
    #moderated_nmvn_0.4_amelia = amelia(moderated_nmvn_mar_0.4, m = 10)
    #moderated_nmvn_0.9_amelia = amelia(moderated_nmvn_mar_0.9, m = 10)
    #moderated_nmvn_0.4_amelia_subset = amelia(moderated_nmvn_mar_0.4_subset, m = 10)
            #** Hierarchical DGP imputation
    #hierarchical_0.1_amelia = amelia(hierarchical_mar_0.1, m = 10)
    #hierarchical_0.4_amelia = amelia(hierarchical_mar_0.4, m = 10)
    #hierarchical_0.9_amelia = amelia(hierarchical_mar_0.1, m = 10)
    #hierarchical_0.4_amelia_subset = amelia(hierarchical_mar_0.4_subset, m = 10)
}

    #* MICE with Bayesian Linear regression
{
        #** Simple DGP imputation
            #*** MVN
    simple_mvn_0.1_linear_mice = mice(simple_mvn_mar_0.1, m = 10, meth = 'norm')
    simple_mvn_0.4_linear_mice = mice(simple_mvn_mar_0.4, m = 10, meth = 'norm')
    simple_mvn_0.9_linear_mice = mice(simple_mvn_mar_0.9, m = 10, meth = 'norm')
    simple_mvn_0.4_linear_subset = mice(simple_mvn_mar_0.4_subset, m = 10, meth = 'norm')
            #*** NMVN
    #simple_nmvn_0.1_linear_mice = mice(simple_nmvn_mar_0.1, m = 10, meth = 'norm')
    #simple_nmvn_0.4_linear_mice = mice(simple_nmvn_mar_0.4, m = 10, meth = 'norm')
    #simple_nmvn_0.9_linear_mice = mice(simple_nmvn_mar_0.9, m = 10, meth = 'norm')
    #simple_nmvn_0.4_linear_subset = mice(simple_nmvn_mar_0.4_subset, m = 10, meth = 'norm')
        #** Moderated DGP imputation
            #*** MVN
    moderated_mvn_0.1_linear_mice = mice(moderated_mvn_mar_0.1, m = 10, meth = 'norm')
    moderated_mvn_0.4_linear_mice = mice(moderated_mvn_mar_0.4, m = 10, meth = 'norm')
    moderated_mvn_0.9_linear_mice = mice(moderated_mvn_mar_0.9, m = 10, meth = 'norm')
    moderated_mvn_0.4_linear_subset = mice(moderated_mvn_mar_0.4_subset, m = 10, meth = 'norm')
            #*** NMVN
    #moderated_nmvn_0.1_linear_mice = mice(moderated_nmvn_mar_0.1, m = 10, meth = 'norm')
    #moderated_nmvn_0.4_linear_mice = mice(moderated_nmvn_mar_0.4, m = 10, meth = 'norm')
    #moderated_nmvn_0.9_linear_mice = mice(moderated_nmvn_mar_0.9, m = 10, meth = 'norm')
    #moderated_nmvn_0.4_linear_subset = mice(moderated_nmvn_mar_0.4_subset, m = 10, meth = 'norm')
}

    #* MICE with Random Forests
{
        #** Simple DGP imputation
            #*** MVN
    simple_mvn_0.1_rf = mice(simple_mvn_mar_0.1, m = 10, meth = 'rf')
    simple_mvn_0.4_rf = mice(simple_mvn_mar_0.4, m = 10, meth = 'rf')
    simple_mvn_0.9_rf = mice(simple_mvn_mar_0.9, m = 10, meth = 'rf')
    simple_mvn_0.4_rf_subset = mice(simple_mvn_mar_0.4_subset, m = 10, meth = 'rf')
            #*** NMVN
    #simple_nmvn_0.1_rf = mice(simple_nmvn_mar_0.1, m = 10, meth = 'rf')
    #simple_nmvn_0.4_rf = mice(simple_nmvn_mar_0.4, m = 10, meth = 'rf')
    #simple_nmvn_0.9_rf = mice(simple_nmvn_mar_0.9, m = 10, meth = 'rf')
    #simple_nmvn_0.4_rf_subset = mice(simple_nmvn_mar_0.4_subset, m = 10, meth = 'rf')
        #** Moderated DGP imputation
            #*** MVN
    moderated_mvn_0.1_rf = mice(moderated_mvn_mar_0.1, m = 10, meth = 'rf')
    moderated_mvn_0.4_rf = mice(moderated_mvn_mar_0.4, m = 10, meth = 'rf')
    moderated_mvn_0.9_rf = mice(moderated_mvn_mar_0.9, m = 10, meth = 'rf')
    moderated_mvn_0.4_rf_subset = mice(moderated_mvn_mar_0.4_subset, m = 10, meth = 'rf')
            #*** NMVN
    #moderated_nmvn_0.1_rf = mice(moderated_nmvn_mar_0.1, m = 10, meth = 'rf')
    #moderated_nmvn_0.4_rf = mice(moderated_nmvn_mar_0.4, m = 10, meth = 'rf')
    #moderated_nmvn_0.9_rf = mice(moderated_nmvn_mar_0.9, m = 10, meth = 'rf')
    #moderated_nmvn_0.4_rf_subset = mice(moderated_nmvn_mar_0.4_subset, m = 10, meth = 'rf')
}