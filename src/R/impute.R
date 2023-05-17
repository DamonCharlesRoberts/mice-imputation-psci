#' clean the output of the miceRanger imputation
#' @param resultList
#' @returns resultList
complete_data <- function(resultList = NULL) {
    #' Clean the miceRanger output
    #'
    #' Parameters
    #' ----
    #' data_frame(list(data.frame)): list of data.frames that have been imputed
    #'
    #' Returns
    #' ----
    #' list of data.frame
    mr_list <- base::lapply(
        resultList # take the resultList list of miceRanger objects
        ,miceRanger::completeData #create a list of data.table objects
    )
    mr_df_list <- base::lapply(
        mr_list #take the list of data.table objects
        ,data.table::rbindlist #and bind them into one data.frame
        , id = "imputations"#create an id column called imputations
    )
}
#'impute the data
#' @param df the amputed data.frame
#' @param m the number of imputations to perform
#' @param package the package to use for the imputation
#' @param meth the method of imputation
#' @return list of data.tables
#' @export
impute <- function(
    df
    ,m = 10
    ,package = NULL
    ,meth = NULL
) {
    # perform imputation
    if (package == "Amelia") {
        resultList <- base::by(
            df[,-1] # take the whole dataframe except the dataset column
            ,df$dataset # execute the following function per dataset factor level
            ,FUN=function (x) {
                Amelia::amelia(
                    x
                    ,m=m
                ) #impute the data with amelia
            }
        ) # run the specified function for the dataset across `dataset` column factors
        resultClean <- base::lapply( # create a list from iteratively performing the following
            resultList # take the list of amelia objects
            ,function(x) data.table::rbindlist(
                x$imputations # grab the imputed dataframes
                ,id="imputations" # create a new column called "imputations" storing the list name
            ) # and bind all of these imputed dataframes into one dataframe with an id column
        )
        #x <- lapply(df, amelia, m = m) # nolint
        #df <- lapply(mice::complete, x, "long")
    } else if (package == "miceRanger") {
        resultList <- base::by(
            df[,-1] #take the whole dataframe except the dataset column
            ,df$dataset # execute the following function per dataset factor level
            ,FUN=function (x) {
                miceRanger::miceRanger(
                    x
                    ,m=m
                    ,verbose=FALSE
                )
            }#impute the data with miceRanger
        )
        resultClean <- miceRanger::complete_data(resultList=resultList)
        #x <- lapply(df, miceRanger, m = m, verbose = FALSE) # nolint
        #df <- complete_data(x)
    } else {
        resultList <- base::by(
            df[,-1] # take the whole dataframe except the dataset column
            ,df$dataset #execute the following function per dataset factor level
            ,FUN=function (x) {
                mice::mice(
                    x
                    ,m=m
                    ,meth=meth
                    ,printFlag=FALSE
                )
            }#impute the data with mice and a user-specified method
        )
        #x <- lapply(df, mice, m = m, meth = meth, printFlag = FALSE) # nolint
        resultClean <- base::lapply(resultList, mice::complete, "long")
    }
    return(resultClean)
}

## Discrepancy
#discrepancy <- function(procedure, model = FALSE) {
#    #' discrepancy
#    #'
#    #' Description:
#    #' ----
#    #'  Function that calculates the mean discrepancy between the imputed and original dataset # nolint
#    #'
#    #' Parameters:
#    #' ----
#    #' procedure(list):
#    #'  - List of imputed data.frames
#    #' model(bool):
#    #'  - Is this to calculate the discrepancy between model estimates or raw data?
#    #'
#    #' Returns:
#    #' ----
#    #' data.frame of average discrepancies for each dataset
#        # Create empty objects
#        combined_x <- NULL
#        combined_y <- NULL
#        combined_z <- NULL
#        # Perform all of the following for each random sample
#        for (d in 1: datasets) { # nolint
#            #* Define queries to the database
#            if (procedure == "interpolate"){
#                df_query <- paste0("mean_", as.character(d), sep = "")
#            } else if (procedure == "amelia") {
#                df_query <- paste0("amelia_", as.character(d), sep = "")
#            } else if (procedure == "lmice") {
#                df_query <- paste0("lmice_", as.character(d),
#                sep = "")
#            } else if (procedure == "rfmice") {
#                df_query <- paste0("rfmice_", as.character(d), sep = "")
#            } else if (procedure == "rfranger") {
#                df_query <- paste0("rfranger_", as.character(d), sep = "")
#            } else {
#                df_query <- paste0("amputed_", as.character(d-1), sep = "")
#            }
#            #* Define an alternative engine
#            if (procedure == "amputed") {
#                alt_engine <- engine2
#            } else {
#                alt_engine <- engine3
#            }
#            #* If I'm not looking at discrepancies for the model and just the raw data...
#            if (model == FALSE) {
#                #** Dataframe query
#                    #*** For imputed or amputed data
#                df <- dbGetQuery(
#                    alt_engine, paste0("SELECT * FROM ", df_query, sep = ""))
#                    #*** For complete sample data
#                original_query <- paste0("original_", as.character(d-1), sep = "")
#                original <- dbGetQuery(engine, paste0("SELECT * FROM ", original_query, sep = ""))
#                #** Take the mean difference between original and imputed for each sample
#                mean_x <- mean(df$X - original$X)
#                mean_y <- mean(df$Y - original$Y)
#                mean_z <- mean(df$Z - original$Z)
#            #* If I am looking at the discrepancy for the model, though...
#            } else if (model == TRUE) {
#                #** define an empty data.frame to store the mean of the posterior for each sample
#                sample_mean <- NULL
#                #** for each imputed/amputed dataset for each sample, do the following
#                for (j in 1: 10){
#                    if (procedure == "amputed"){
#                        query <- paste0('SELECT * FROM ', df_query, sep = "")
#                    } else if (procedure == "rfranger") {
#                    #*** grab the data.frame
#                        query <- paste0("SELECT * FROM ", df_query, " WHERE dataset = 'Dataset_", as.character(j), "'", sep = "")
#                    } else {
#                        query <- paste0('SELECT * FROM ', df_query, ' WHERE ".id" = ', as.character(j), sep = "")
#                    }
#                    df <- dbGetQuery(alt_engine, query)
#                    #*** convert the data.frame into a list
#                    df_complete <- df[complete.cases(df),]
#                    df_list <- list(
#                        N = nrow(df_complete),
#                        x = df_complete$X,
#                        z = df_complete$Z,
#                        y = df_complete$Y
#                    )
#                    ##*** fit the stan model with the data from above
#                    #fitted <- lm(
#                    #    formula = Y ~ X + Z,
#                    #    data = df
#                    #)
#                    #fitted_df <- data.frame(X = fitted$coefficients[[2]], Z = fitted$coefficients[[3]])
#                    fitted <- sampling(compiled, df_list, chains = 1, iter = 100)
#                    #fitted %<-% brms_multiple(formula = Y ~ X + Z, data = df)
#                    #*** take the mean of the posterior estimates for each col
#                    mean_posterior <- colMeans(as.data.frame(fitted))
#                    ##*** for each sample, add these mean_posteriors to a dataframe
#                    #sample_mean <- rbind(data.frame(sample_mean), fitted_df)
#                    sample_mean <- rbind(data.frame(sample_mean), as.data.frame.list(mean_posterior))
#                }
#                #** take the difference between the posterior sample means and actual beta coefficients
#                mean_x <- mean(sample_mean$beta_1 - 0.6)
#                mean_z <- mean(sample_mean$beta_2 - 0.9)
#            }
#                #** store the discrepancies in a data.frame
#            if (model == FALSE){
#                combined_x <- bind_rows(data.frame(combined_x), data.frame(mean_x)) # nolint
#                combined_z <- bind_rows(data.frame(combined_z), data.frame(mean_z)) # nolint
#                combined_y <- bind_rows(data.frame(combined_y), data.frame(mean_y))
#                discrepancy_df <- cbind(combined_x, combined_z, combined_y)
#            } else {
#                combined_x <- bind_rows(data.frame(combined_x), data.frame(mean_x)) # nolint
#                combined_z <- bind_rows(data.frame(combined_z), data.frame(mean_z)) # nolint
#                discrepancy_df <- cbind(combined_x, combined_z)
#            }
#
#        }
#        #* return the dataframe of discrepancies
#        return(discrepancy_df)
#    }