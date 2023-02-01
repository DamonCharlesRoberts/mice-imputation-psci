# Title: Functions for R code
    #* Notes:
        #** Description: script to produce functions for main qmd file
        #** Updated: 2023-01-13
            #*** by: dcr

# Imputation

impute <- function(
    df = amputed, con = engine4, m = 10, package = NULL, meth = NULL
    ) {
    #' Impute the amputed simulation datasets
    #' Using either amelia, miceRanger, or mice

    #' Parameters
    #' ------
    #' df(list(data.frame)): dataset with missing data
    #' m(int): number of datasets to produce in imputation
    #' package(str): name of package used to do imputation.
    #'    Either amelia, miceRanger, or mice
    #' meth(str): method of imputation
    #'    for mice package only
    #'    uses methods allowed by mice package only

    #' Returns
    #' -----
    #' A list object that contains:
    #'    depends on package arg, but generally:
    #'        dataframes of original data with missingness
    #'        dataframes of imputed data
    if (package == "Amelia") {
        x <- lapply(df, amelia, m = m) # nolint
        df <- lapply(mice::complete, x, "long")
    } else if (package == "miceRanger") {
        x <- lapply(df, miceRanger, m = m, verbose = FALSE) # nolint
        df <- lapply(x, miceRanger::completeData)
    } else {
        x <- lapply(df, mice, m = m, meth = meth) # nolint
        df <- lapply(x, mice::complete, "long")
    }
    return(df)
}