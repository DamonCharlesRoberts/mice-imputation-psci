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
discrepancy <- function(procedure, model = FALSE) {
    #' discrepancy
    #'
    #' Description:
    #' ----
    #'  Function that calculates the mean discrepancy between the imputed and original dataset # nolint
    #'
    #' Parameters:
    #' ----
    #' procedure(list):
    #'  - List of imputed data.frames
    #' model(bool):
    #'  - Is this to calculate the discrepancy between model estimates or raw data?
    #'
    #' Returns:
    #' ----
    #' data.frame of average discrepancies for each dataset
        # Create empty objects
        combined_x <- NULL
        combined_y <- NULL
        combined_z <- NULL
        # Perform all of the following for each random sample
        for (d in 1: datasets) { # nolint
            #* Define queries to the database
            if (procedure == "interpolate"){
                df_query <- paste0("mean_", as.character(d), sep = "")
            } else if (procedure == "amelia") {
                df_query <- paste0("amelia_", as.character(d), sep = "")
            } else if (procedure == "lmice") {
                df_query <- paste0("lmice_", as.character(d),
                sep = "")
            } else if (procedure == "rfmice") {
                df_query <- paste0("rfmice_", as.character(d), sep = "")
            } else if (procedure == "rfranger") {
                df_query <- paste0("rfranger_", as.character(d), sep = "")
            } else {
                df_query <- paste0("amputed_", as.character(d-1), sep = "")
            }
            #* Define an alternative engine
            if (procedure == "amputed") {
                alt_engine <- engine2
            } else {
                alt_engine <- engine3
            }
            #* If I'm not looking at discrepancies for the model and just the raw data...
            if (model == FALSE) {
                #** Dataframe query
                    #*** For imputed or amputed data
                df <- dbGetQuery(
                    alt_engine, paste0("SELECT * FROM ", df_query, sep = ""))
                    #*** For complete sample data
                original_query <- paste0("original_", as.character(d-1), sep = "")
                original <- dbGetQuery(engine, paste0("SELECT * FROM ", original_query, sep = ""))
                #** Take the mean difference between original and imputed for each sample
                mean_x <- mean(df$X - original$X)
                mean_y <- mean(df$Y - original$Y)
                mean_z <- mean(df$Z - original$Z)
            #* If I am looking at the discrepancy for the model, though...
            } else if (model == TRUE) {
                #** define an empty data.frame to store the mean of the posterior for each sample
                sample_mean <- NULL
                #** for each imputed/amputed dataset for each sample, do the following
                for (j in 1: 10){
                    if (procedure == "amputed"){
                        query <- paste0('SELECT * FROM ', df_query, sep = "")
                    } else if (procedure == "rfranger") {
                    #*** grab the data.frame
                        query <- paste0("SELECT * FROM ", df_query, " WHERE dataset = 'Dataset_", as.character(j), "'", sep = "")
                    } else {
                        query <- paste0('SELECT * FROM ', df_query, ' WHERE ".id" = ', as.character(j), sep = "")
                    }
                    df <- dbGetQuery(alt_engine, query)
                    #*** convert the data.frame into a list
                    df_complete <- df[complete.cases(df),]
                    df_list <- list(
                        N = nrow(df_complete),
                        x = df_complete$X,
                        z = df_complete$Z,
                        y = df_complete$Y
                    )
                    ##*** fit the stan model with the data from above
                    #fitted <- lm(
                    #    formula = Y ~ X + Z,
                    #    data = df
                    #)
                    #fitted_df <- data.frame(X = fitted$coefficients[[2]], Z = fitted$coefficients[[3]])
                    fitted <- sampling(compiled, df_list, chains = 1, iter = 100)
                    #fitted %<-% brms_multiple(formula = Y ~ X + Z, data = df)
                    #*** take the mean of the posterior estimates for each col
                    mean_posterior <- colMeans(as.data.frame(fitted))
                    ##*** for each sample, add these mean_posteriors to a dataframe
                    #sample_mean <- rbind(data.frame(sample_mean), fitted_df)
                    sample_mean <- rbind(data.frame(sample_mean), as.data.frame.list(mean_posterior))
                }
                #** take the difference between the posterior sample means and actual beta coefficients
                mean_x <- mean(sample_mean$beta_1 - 0.6)
                mean_z <- mean(sample_mean$beta_2 - 0.9)
            }
                #** store the discrepancies in a data.frame
            if (model == FALSE){
                combined_x <- bind_rows(data.frame(combined_x), data.frame(mean_x)) # nolint
                combined_z <- bind_rows(data.frame(combined_z), data.frame(mean_z)) # nolint
                combined_y <- bind_rows(data.frame(combined_y), data.frame(mean_y))
                discrepancy_df <- cbind(combined_x, combined_z, combined_y)
            } else {
                combined_x <- bind_rows(data.frame(combined_x), data.frame(mean_x)) # nolint
                combined_z <- bind_rows(data.frame(combined_z), data.frame(mean_z)) # nolint
                discrepancy_df <- cbind(combined_x, combined_z)
            }

        }
        #* return the dataframe of discrepancies
        return(discrepancy_df)
    }