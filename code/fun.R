# Title: Functions for R code
    #* Notes:
        #** Description: script to produce functions for main qmd file
        #** Updated: 2023-01-13
            #*** by: dcr

# complete_data

complete_data <- function(data_frame = NULL) {
    #' Clean the miceRanger output
    #' 
    #' Parameters
    #' ----
    #' data_frame(list(data.frame)): list of data.frames that have been imputed
    #' 
    #' Returns
    #' ----
    #' list of data.frame
    x <- lapply(data_frame, miceRanger::completeData)
    df <- dplyr::bind_rows(x, .id = "column_label")
}
# Imputation

impute <- function(
    df = amputed, con = engine4, m = 10, package = NULL, meth = NULL
    ) {
    #' Impute the amputed simulation datasets
    #'
    #' Description:
    #' ----
    #'  Using either amelia, miceRanger, or mice
    #'
    #' Parameters
    #' ------
    #' df(list(data.frame)): dataset with missing data
    #' m(int): number of datasets to produce in imputation
    #' package(str): name of package used to do imputation.
    #'    Either amelia, miceRanger, or mice
    #' meth(str): method of imputation
    #'    for mice package only
    #'    uses methods allowed by mice package only
    #'
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
        df <- lapply(x, complete_data)
    } else {
        x <- lapply(df, mice, m = m, meth = meth) # nolint
        df <- lapply(x, mice::complete, "long")
    }
    return(df)
}

# Discrepancy
discrepancy <- function(imputed = NULL, original = NULL) {
    #' discrepancy
    #'
    #' Description:
    #' ----
    #'  Function that calculates the mean discrepancy between the imputed and original dataset
    #'
    #' Parameters:
    #' ----
    #' imputed(list):
    #'  - List of imputed data.frames
    #' original(list):
    #'  - List of original data.frames
    #'
    #' Returns:
    #' ----
    #' data.frame of average discrepancies for each dataset

    # Create empty objects
    combined_X = NULL
    combined_Y = NULL
    combined_Z = NULL
    # Calculate mean discrepancy and add this to data.frame
    for (d in 1: datasets) {
        X <- mean(imputed[[d]]$X - original[[d]]$X)
        Y <- mean(imputed[[d]]$Y - original[[d]]$Y)
        Z <- mean(imputed[[d]]$Z - original[[d]]$Z)
        combined_X <- bind_rows(data.frame(combined_X), data.frame(X))
        combined_Y <- bind_rows(data.frame(combined_Y), data.frame(Y))
        combined_Z <- bind_rows(data.frame(combined_Z), data.frame(Z))
        mean_diff_df <- cbind(
            "X" = combined_X,
            "Y" = combined_Y,
            "Z" = combined_Z
        )
    }
    return(mean_diff_df)
}