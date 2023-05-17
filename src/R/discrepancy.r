#' calculate the discrepancy for a single sample
#' @param procedure the procedure used
#' @param model is it the discrepancy for a model or not
#' @param num should be equal to the number of samples generated
single_discrepancy <- function (
    procedure="interpolation"
    ,model=FALSE
    ,num=1
) {
    # Define the table name string
    dfQuery <- base::paste0(
        procedure
        ,"_"
        ,base::as.character(num)
        ,sep=""
    )
    # Define the connection
    if (procedure == "amputed") {
        altEngine <- engine2
    } else {
        altEngine <- engine3
    }
    # If just looking at raw data, calculate discrepancy
    if (model == FALSE) {
        # Query the amputed data
        amputedDF <- data.table::as.data.table(
            DBI::dbGetQuery(
                alt_engine
                ,base::paste0(
                    "SELECT * FROM"
                    ,dfQuery
                    ,sep=""
                )
            )
        )
        # Query the original data
        originalDF <- data.table::as.data.table(
            DBI::dbGetQuery(
                engine
                ,base::paste0(
                    "SELECT * FROM original_"
                    ,base::as.character(num-1)
                    ,sep=""
                )
            )
        )
        # calculate the mean difference between original and imputed
            #* Define the variables to focus on
        variables <- c("X","Y","Z")
            #* for each variable...
            #* ... calculate the row-wise difference
        originalMedian <- originalDF[
            ,lapply(
                .SD
                ,median
                ,na.rm=TRUE
            )
        ]
        amputedMedian <- amputedDF[
            ,lapply(
                .SD
                ,median
                ,na.rm=TRUE
            )
        ]
        discrepancyDF <- data.table::as.data.table(
            lapply(
                variables
                ,function (x) {
                    amputedMedian[,..x]-originalMedian[,..x]
                }
            )
        )
    } else if (model==TRUE) {
        # calculate the median posterior value
        medianPosterior <- base::lapply(
            1:10
            , function(x) {
                if (procedure == "rfranger") {
                    query <- base::paste0(
                        "SELECT * FROM "
                        ,dfQuery
                        ,"WHERE dataset = 'Dataset_"
                        ,base::as.character(x)
                        ,"'"
                        ,sep=""
                    )
                } else {
                    query <- base::paste0(
                        'SELECT * FROM '
                        ,dfQuery
                        ,'WHERE ".id" = '
                        ,base::as.character(x)
                        ,sep=""
                    )
                }
                df <- DBI::dbGetQuery(
                    alt_engine
                    ,query
                )
                # keep only complete cases
                dfComplete <- df[stats::complete.cases(df),]
                # convert the data.frame to a list
                dfCompleteList <- base::list(
                    N = base::nrow(dfComplete)
                    ,x=dfComplete[,"X"]
                    ,z=dfComplete[,"Z"]
                    ,y=dfComplete[,"Y"]
                )
                # Fit the stan model
                fitted <- rstan::sampling(
                    compiled
                    ,dfCompleteList
                    ,chains=1
                    ,iter=100
                )
                # take the median of the posterior
                medianPosterior <- data.table::as.data.table(
                    fitted
                    )[
                        ,lapply(.SD, median)
                    ]
            }
        )
        medianPosteriorDF <- data.table::rbindlist(medianPosterior)
        differenceDF <- data.table::data.table(
            X = medianPosteriorDF$X - 0.6
            ,Z = medianPosteriorDF$Z - 0.9
        )
        discrepancyDF <- differenceDF[
            ,lapply(
                .SD
                ,median
            )
        ]
    }

}

define_query <- function (
    procedure = "interpolation"
    ,model=FALSE
    ,i
) {
    # Define the table name string
    dfQuery <- paste0(
        procedure
        ,"_"
        ,as.character(i)
        ,sep=""
    )

    # Define the engine
    if (procedure == "amputed") {
        alt_engine <- engine2
    } else {
        alt_engine <- engine3
    }

    # Execute the query
        

}

discrepancy <- function () {

}