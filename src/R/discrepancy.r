#' @title calculate discrepancies
#' 
#' @description 
#' This is a function that calculates the discrepancy between either
#' the imputed data or the beta coefficients relative to the full
#' data or parameters
#' 
#' @details
#' Requires a data.table object of the full sample data, and the imputed data
#' Need to also specify the procedure and whether I want to calculate
#' the discrepancy for the model or not.
#' 
#' @param sample_data a data.table object of the original sample data
#' @param imputed_data a list of data.table objects with the imputed data
#' @param procedure the procedure used
#' @param model is it the discrepancy for a model or not
#' @param ct_type whether to use the mean or median function
#' 
#' @returns df_discrepancy A data.table object of the discrepancies
#' 
#' @examples
#' df <- discrepancy(
#'   sample_data = df_sample
#'   , imputed_data = df_imputed
#'   , procedure = "interpolation"
#'   , model = FALSE
#'   , ct_type = mean
#' )
#' 
#' @export
discrepancy <- function (
  sample_data=NULL
  ,imputed_data=NULL
  ,procedure="interpolation"
  ,model=FALSE
  ,ct_type = mean
) {
  # Get imputed data based on pre-specified procedure
  #  #* bind the datasets of the same imputation procedure
  #  #* and record the dataset in the dataset column
  df_imputed_data <- imputed_data[procedure == procedure]
  #df_imputed_data <- df_imputed_data[
  #    , dataset := as.integer(dataset) # convert dataset column to integer
  #]
  sample_data <- sample_data[
    , dataset := base::as.integer(dataset)
  ]
    #* filter the original data for the rows that have the same id value across both datasets
  df_shortened_sample <- sample_data[sample_data$id %in% df_imputed_data$id]
    #* Define the variables to focus on
  list_variables <- c(
    "X"
    , "Y"
    , "Z"
  )
  # If just looking at raw data, calculate discrepancy
  if (model == FALSE) {
    # calculate the mean/median difference between original and imputed
      #* join the original data to the imputed data
    df_joined <- df_shortened_sample[
        imputed_data
        , on = c(
          "id"
          , "dataset"
        )
    ]
      #* subtract the original values from the imputed values
    df_discrepancy <- data.table::data.table(
        df_joined[, "id"]
        , df_joined[, "dataset"]
        , df_joined[, "imputations"]
        , df_joined[, "X"] - df_joined[, "i.X"]
        , df_joined[, "Z"] - df_joined[, "i.Z"]
        , df_joined[, "Y"] - df_joined[, "i.Y"]
    )
      #* if mean, calculate the mean of the discrepancy
    if (ct_type == "mean") {
        df_discrepancy <- df_discrepancy[
            , base::lapply(
                .SD
                , mean
            )
            , .SDcols = list_variables
            , by = dataset
        ]
    } else {
      #* if median, calculate the median of the discrepancy
        df_discrepancy <- df_discrepancy[
            , base::lapply(
                .SD
                , median
            )
            , .SDcols = list_variables
            , by = dataset
        ]
    }
    # return the discrepancy data.table
    return(df_discrepancy)
  } else if (model==TRUE) {
    # Keep only complete cases
    df_imputed_complete <- df_imputed_data[
        stats::complete.cases(df_imputed_data)
        ,
    ]
    # Convert to a list where each element is a dataset data.table
    list_datasets <- base::split(
        df_imputed_complete
        , by = c("dataset")
    )
    # Run ols on each element of that list
    fit <- base::lapply(
      list_datasets,
      function (x) {
        # convert each dataset into an imputationList
        list_imp_temp <- mitools::imputationList(
          base::split(
            x, x$imputations[-1]
          )
        )
        # fit a OLS regression to eahc imputationList element
        fit_temp <- base::with(list_imp_temp, stats::lm(Y ~ X + Z))
        # pool the OLS regressions across each dataset
        pooled_temp <- mitools::MIcombine(fit_temp)
        # return the pooled results of the OLS model
        return(pooled_temp)
      }
    )
    # Convert to data.table object
    df_tidy <- base::lapply(
      fit,
      function (x) {
        # make the pooled coefficients a data.frame
        df_pooled <- base::data.frame(
          x[1]$coefficients
        )
        # flip the data.frame
        df_pooled_tidy <- data.table::transpose(
          df_pooled
        )
        # set the row names to the column names after transposing
        df_pooled_tidy <- stats::setNames(
          df_pooled_tidy
          , base::rownames(df_pooled)
        ) |>
        data.table::as.data.table()
        df_pooled_short <- df_pooled_tidy[
            , `(Intercept)` := NULL
         ]
      }
    )
    # Combine all of the pooled results into a data.table
    df_tidy <- data.table::rbindlist(
      df_tidy
      ,id="dataset"
    )
    # subtract the estiamted coefficients from the parameters
    df_difference <- data.table::data.table(
        dataset = df_tidy$dataset
        , X = df_tidy$X - 0.6
        , Z = df_tidy$Z - 0.9
    )
    # if mean, calculate the mean discrepancy
    if (ct_type == "mean") {
       df_discrepancy <- df_difference[
        , base::lapply(
          .SD
          , mean
        )
        , .SDcols = list_variables[-2]
        , by = dataset
       ]
    # if median, calculate the median discrepancy
    } else {
       df_discrepancy <- df_difference[
        , base::lapply(
            .SD
            , median
        )
        , .SDcols = list_variables[-2]
        , by = dataset
       ]
    }
  }
  return(df_discrepancy)
}