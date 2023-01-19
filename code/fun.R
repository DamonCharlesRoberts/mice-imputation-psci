"""
    Title: Functions for R code
    Notes:
        - Description: script to produce functions for main qmd file
        - Updated: 2023-01-13
            - by: dcr
"""

# Imputation

impute <- function(df = amputed, m = 10, package = NULL, meth = NULL){
    """
    Impute the amputed simulation datasets with either amelia, miceRanger, or mice

    Parameters
    ------
    df: a list of data.frames or tibbles
        dataset with missing data
    m:  int
        number of datasets to produce in imputation
    package: str
        name of package used to do imputation.
        Either amelia, miceRanger, or mice
    meth: str
        method of imputation
        for mice package only
        uses methods allowed by mice package only

    Returns
    -----
    A list object that contains:
        depends on package arg, but generally:
            dataframes of original data with missingness
            dataframes of imputed data
    """
    if(package == "Amelia"){
        x <- lapply(df, amelia, c(m = m))
    } else if(package == "miceRanger"){
        x <- lapply(df, miceRanger, c(m = m, verbose = FALSE))
    } else{
        x <- lapply(df, mice, c(m = m, meth = meth))
    }
    return(x)
}