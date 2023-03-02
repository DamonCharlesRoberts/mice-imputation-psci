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
    mr_list <- lapply(data_frame, miceRanger::completeData)
    mr_df_list <- lapply(mr_list, dplyr::bind_rows, .id = "dataset") # nolint
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
        df <- complete_data(x)
    } else {
        x <- lapply(df, mice, m = m, meth = meth, printFlag = FALSE) # nolint
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
    #'  Function that calculates the mean discrepancy between the imputed and original dataset # nolint
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
    combined_x <- NULL
    combined_y <- NULL
    combined_z <- NULL
    # Calculate mean discrepancy and add this to data.frame
    for (d in 1: datasets) { # nolint # nolint
        mean_x <- mean(imputed[[d]]$X - original[[d]]$X)
        mean_y <- mean(imputed[[d]]$Y - original[[d]]$Y)
        mean_z <- mean(imputed[[d]]$Z - original[[d]]$Z)
        combined_x <- bind_rows(data.frame(combined_x), data.frame(mean_x)) # nolint
        combined_y <- bind_rows(data.frame(combined_y), data.frame(mean_y)) # nolint
        combined_z <- bind_rows(data.frame(combined_z), data.frame(mean_z)) # nolint
        mean_diff_df <- cbind(
            "X" = combined_x,
            "Y" = combined_y,
            "Z" = combined_z
        )
    }
    return(mean_diff_df)
}